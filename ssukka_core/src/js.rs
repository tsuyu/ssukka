use crate::config::JsStringEncoding;
use crate::symbol_map::SymbolMap;
use aho_corasick::{AhoCorasick, MatchKind};
use rand::rngs::StdRng;
use rand::RngExt;
use std::collections::HashMap;

/// State machine states for JS lexing.
#[derive(Debug, Clone, Copy, PartialEq)]
enum State {
    Normal,
    SingleLineComment,
    MultiLineComment,
    SingleQuoteString,
    DoubleQuoteString,
    TemplateString,
}

/// Transform JS: replace class/ID references, encode string literals, minify.
///
/// `encoding` selects the string-literal strategy. [`JsStringEncoding::Array`]
/// is the AST engine's job; here it degrades to [`JsStringEncoding::Escapes`].
pub fn transform_js(
    js: &str,
    symbols: &SymbolMap,
    encoding: JsStringEncoding,
    minify: bool,
    rename_classes: bool,
    rename_ids: bool,
    rng: &mut StdRng,
) -> String {
    let mut result = js.to_owned();

    if rename_classes || rename_ids {
        result = replace_symbol_references(&result, symbols, rename_classes, rename_ids);
    }

    // `Array` requires the AST engine; without it we fall back to escapes.
    if encoding != JsStringEncoding::None {
        result = encode_js_strings(&result, rng);
    }

    // Keep the minified form only if it is provably the same program (and the
    // input parses at all); otherwise the script goes out unminified.
    if minify {
        let minified = minify_js(&result);
        if crate::js_ast::same_program(&result, &minified) {
            result = minified;
        }
    }

    result
}

/// Pick a randomized escape form for a single character.
///
/// Only strict-mode-safe forms are emitted (`\xHH`, `\uXXXX`, `\u{..}`); octal
/// escapes are deliberately avoided since they throw in strict mode / templates.
fn escape_char(ch: char, out: &mut String, rng: &mut StdRng) {
    let code = ch as u32;
    if code <= 0xFF {
        match rng.random_range(0u8..3) {
            0 => out.push_str(&format!("\\x{code:02x}")),
            1 => out.push_str(&format!("\\u{code:04x}")),
            _ => out.push_str(&format!("\\u{{{code:x}}}")),
        }
    } else if code <= 0xFFFF {
        if rng.random_bool(0.5) {
            out.push_str(&format!("\\u{code:04x}"));
        } else {
            out.push_str(&format!("\\u{{{code:x}}}"));
        }
    } else if rng.random_bool(0.5) {
        out.push_str(&format!("\\u{{{code:x}}}"));
    } else {
        // Surrogate pair for characters above the BMP
        let hi = ((code - 0x10000) >> 10) + 0xD800;
        let lo = ((code - 0x10000) & 0x3FF) + 0xDC00;
        out.push_str(&format!("\\u{hi:04x}\\u{lo:04x}"));
    }
}

/// Replace class/ID names inside JS string literals (`"foo"`, `'foo'`,
/// `` `foo` ``).
pub(crate) fn replace_symbol_references(
    js: &str,
    symbols: &SymbolMap,
    rename_classes: bool,
    rename_ids: bool,
) -> String {
    let chars: Vec<char> = js.chars().collect();
    let len = chars.len();
    let mut out = String::with_capacity(len);
    let mut i = 0;
    let mut state = State::Normal;

    // Build the match automata once and reuse them for every string literal.
    let class_ac = rename_classes.then(|| ac_from_map(symbols.classes())).flatten();
    let id_ac = rename_ids.then(|| ac_from_map(symbols.ids())).flatten();

    while i < len {
        match state {
            State::Normal => {
                if i + 1 < len && chars[i] == '/' && chars[i + 1] == '/' {
                    out.push(chars[i]);
                    state = State::SingleLineComment;
                    i += 1;
                } else if i + 1 < len && chars[i] == '/' && chars[i + 1] == '*' {
                    out.push(chars[i]);
                    state = State::MultiLineComment;
                    i += 1;
                } else if chars[i] == '\'' {
                    out.push(chars[i]);
                    state = State::SingleQuoteString;
                    i += 1;
                } else if chars[i] == '"' {
                    out.push(chars[i]);
                    state = State::DoubleQuoteString;
                    i += 1;
                } else if chars[i] == '`' {
                    out.push(chars[i]);
                    state = State::TemplateString;
                    i += 1;
                } else {
                    out.push(chars[i]);
                    i += 1;
                }
            },
            State::SingleLineComment => {
                out.push(chars[i]);
                if chars[i] == '\n' {
                    state = State::Normal;
                }
                i += 1;
            },
            State::MultiLineComment => {
                out.push(chars[i]);
                if chars[i] == '*' && i + 1 < len && chars[i + 1] == '/' {
                    out.push(chars[i + 1]);
                    i += 2;
                    state = State::Normal;
                } else {
                    i += 1;
                }
            },
            State::SingleQuoteString | State::DoubleQuoteString | State::TemplateString => {
                let quote = match state {
                    State::SingleQuoteString => '\'',
                    State::DoubleQuoteString => '"',
                    State::TemplateString => '`',
                    _ => unreachable!(),
                };

                let mut string_content = String::new();
                while i < len {
                    if chars[i] == '\\' && i + 1 < len {
                        let next = chars[i + 1];
                        string_content.push('\\');
                        string_content.push(next);
                        i += 2;
                        if next == 'u' {
                            if i < len && chars[i] == '{' {
                                while i < len {
                                    string_content.push(chars[i]);
                                    if chars[i] == '}' {
                                        i += 1;
                                        break;
                                    }
                                    i += 1;
                                }
                            } else {
                                for _ in 0..4 {
                                    if i < len {
                                        string_content.push(chars[i]);
                                        i += 1;
                                    }
                                }
                            }
                        } else if next == 'x' {
                            for _ in 0..2 {
                                if i < len {
                                    string_content.push(chars[i]);
                                    i += 1;
                                }
                            }
                        }
                    } else if chars[i] == quote {
                        break;
                    } else {
                        string_content.push(chars[i]);
                        i += 1;
                    }
                }

                // Replace symbols in the string content, word-boundary aware.
                let mut replaced = string_content;
                if let Some((ac, repls)) = &class_ac {
                    replaced = replace_words_ac(&replaced, ac, repls);
                }
                if let Some((ac, repls)) = &id_ac {
                    replaced = replace_words_ac(&replaced, ac, repls);
                }

                out.push_str(&replaced);

                if i < len {
                    out.push(chars[i]);
                    i += 1;
                }
                state = State::Normal;
            },
        }
    }

    out
}

/// Encode JS string literals with a randomized mix of escape forms.
fn encode_js_strings(js: &str, rng: &mut StdRng) -> String {
    let chars: Vec<char> = js.chars().collect();
    let len = chars.len();
    let mut out = String::with_capacity(len * 2);
    let mut i = 0;
    let mut state = State::Normal;

    while i < len {
        match state {
            State::Normal => {
                if i + 1 < len && chars[i] == '/' && chars[i + 1] == '/' {
                    out.push(chars[i]);
                    state = State::SingleLineComment;
                    i += 1;
                } else if i + 1 < len && chars[i] == '/' && chars[i + 1] == '*' {
                    out.push(chars[i]);
                    state = State::MultiLineComment;
                    i += 1;
                } else if chars[i] == '\'' {
                    out.push(chars[i]);
                    state = State::SingleQuoteString;
                    i += 1;
                } else if chars[i] == '"' {
                    out.push(chars[i]);
                    state = State::DoubleQuoteString;
                    i += 1;
                } else if chars[i] == '`' {
                    out.push(chars[i]);
                    state = State::TemplateString;
                    i += 1;
                } else {
                    out.push(chars[i]);
                    i += 1;
                }
            },
            State::SingleLineComment => {
                out.push(chars[i]);
                if chars[i] == '\n' {
                    state = State::Normal;
                }
                i += 1;
            },
            State::MultiLineComment => {
                out.push(chars[i]);
                if chars[i] == '*' && i + 1 < len && chars[i + 1] == '/' {
                    out.push(chars[i + 1]);
                    i += 2;
                    state = State::Normal;
                } else {
                    i += 1;
                }
            },
            State::SingleQuoteString | State::DoubleQuoteString | State::TemplateString => {
                let quote = match state {
                    State::SingleQuoteString => '\'',
                    State::DoubleQuoteString => '"',
                    State::TemplateString => '`',
                    _ => unreachable!(),
                };

                while i < len {
                    if chars[i] == '\\' && i + 1 < len {
                        // Keep existing escape sequences intact
                        let next = chars[i + 1];
                        out.push('\\');
                        out.push(next);
                        i += 2;
                        if next == 'u' {
                            if i < len && chars[i] == '{' {
                                // \u{...} code point escape
                                while i < len {
                                    out.push(chars[i]);
                                    if chars[i] == '}' {
                                        i += 1;
                                        break;
                                    }
                                    i += 1;
                                }
                            } else {
                                // \uXXXX: consume 4 hex digits
                                for _ in 0..4 {
                                    if i < len {
                                        out.push(chars[i]);
                                        i += 1;
                                    }
                                }
                            }
                        } else if next == 'x' {
                            // \xHH: consume 2 hex digits
                            for _ in 0..2 {
                                if i < len {
                                    out.push(chars[i]);
                                    i += 1;
                                }
                            }
                        }
                    } else if chars[i] == quote {
                        out.push(chars[i]);
                        i += 1;
                        state = State::Normal;
                        break;
                    } else if state == State::TemplateString && chars[i] == '$' && i + 1 < len && chars[i + 1] == '{' {
                        // Don't encode template literal expressions
                        out.push(chars[i]);
                        i += 1;
                    } else {
                        escape_char(chars[i], &mut out, rng);
                        i += 1;
                    }
                }
            },
        }
    }

    out
}

/// Separator owed between the last emitted token and the next one.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Gap {
    None,
    Space,
    Newline,
}

/// Conservative JS minification: strip comments and collapse whitespace, but
/// keep every line break that separated two tokens.
///
/// Line breaks carry meaning in JS (automatic semicolon insertion, `return` /
/// `throw` / `break` / postfix `++` before a newline), so a run of whitespace or
/// a comment that contained one becomes a single `\n`; other runs become one
/// space only where two tokens would otherwise merge. String literals, template
/// literals (including nested `${}` expressions) and regular-expression
/// literals are copied verbatim. Legal comments (`/*!`) and `/*@` comments are
/// kept.
///
/// There is no AST here: [`transform_js`] re-parses the input and the output
/// and keeps the original whenever they are not the same program.
fn minify_js(js: &str) -> String {
    let chars: Vec<char> = js.chars().collect();
    let len = chars.len();
    let mut out = String::with_capacity(len);
    let mut gap = Gap::None;
    // One entry per `${` of an enclosing template literal: the `{` depth opened
    // inside that substitution, so its closing `}` resumes the template.
    let mut tmpl: Vec<usize> = Vec::new();
    let mut i = 0;

    while i < len {
        let c = chars[i];

        // Whitespace (ASCII only; anything else is copied as a token char).
        if c == '\n' || c == '\r' {
            gap = Gap::Newline;
            i += 1;
            continue;
        }
        if c == ' ' || c == '\t' || c == '\u{000B}' || c == '\u{000C}' {
            if gap == Gap::None {
                gap = Gap::Space;
            }
            i += 1;
            continue;
        }

        // Comments. At a token start `//` and `/*` are always comments: a regex
        // can be neither empty nor start with `*`.
        if c == '/' && chars.get(i + 1) == Some(&'/') {
            // Leave the line terminator for the whitespace branch above.
            i += 2;
            while i < len && chars[i] != '\n' && chars[i] != '\r' {
                i += 1;
            }
            if gap == Gap::None {
                gap = Gap::Space;
            }
            continue;
        }
        if c == '/' && chars.get(i + 1) == Some(&'*') {
            let start = i;
            let keep = matches!(chars.get(i + 2), Some('!') | Some('@'));
            i += 2;
            let mut newline = false;
            while i < len && !(chars[i] == '*' && chars.get(i + 1) == Some(&'/')) {
                if chars[i] == '\n' || chars[i] == '\r' {
                    newline = true;
                }
                i += 1;
            }
            if i >= len {
                // Unterminated: the input is broken; copy the rest untouched.
                flush_gap(&mut out, &mut gap, c);
                out.extend(&chars[start..]);
                break;
            }
            i += 2;
            if keep {
                flush_gap(&mut out, &mut gap, c);
                out.extend(&chars[start..i]);
                continue;
            }
            if newline {
                gap = Gap::Newline;
            } else if gap == Gap::None {
                gap = Gap::Space;
            }
            continue;
        }

        // String literals.
        if c == '\'' || c == '"' {
            flush_gap(&mut out, &mut gap, c);
            out.push(c);
            i += 1;
            while i < len {
                let d = chars[i];
                out.push(d);
                i += 1;
                if d == '\\' {
                    if i < len {
                        out.push(chars[i]);
                        i += 1;
                    }
                } else if d == c || d == '\n' || d == '\r' {
                    break;
                }
            }
            continue;
        }

        // Template literals.
        if c == '`' {
            flush_gap(&mut out, &mut gap, c);
            out.push(c);
            i += 1;
            if copy_template_body(&chars, &mut i, &mut out) {
                tmpl.push(0);
            }
            continue;
        }

        // Regular-expression literal (copied verbatim) or a division operator.
        if c == '/' && regex_allowed(&out) {
            if let Some(end) = scan_regex(&chars, i) {
                flush_gap(&mut out, &mut gap, c);
                out.extend(&chars[i..end]);
                i = end;
                continue;
            }
        }

        // Any other token character.
        flush_gap(&mut out, &mut gap, c);
        out.push(c);
        i += 1;
        if let Some(depth) = tmpl.last_mut() {
            if c == '{' {
                *depth += 1;
            } else if c == '}' {
                if *depth == 0 {
                    // This `}` closes a `${`: back inside the template literal.
                    tmpl.pop();
                    if copy_template_body(&chars, &mut i, &mut out) {
                        tmpl.push(0);
                    }
                } else {
                    *depth -= 1;
                }
            }
        }
    }

    out
}

/// Emit the separator owed before a token starting with `next`.
fn flush_gap(out: &mut String, gap: &mut Gap, next: char) {
    match *gap {
        Gap::None => {},
        Gap::Newline => {
            if !out.is_empty() {
                out.push('\n');
            }
        },
        Gap::Space => {
            if needs_space(out.chars().next_back(), next) {
                out.push(' ');
            }
        },
    }
    *gap = Gap::None;
}

/// Copy template-literal text verbatim from `*i` (just past a backtick or a
/// substitution's closing `}`) up to the closing backtick, or up to and
/// including a `${`. Returns `true` if it stopped at a `${`.
fn copy_template_body(chars: &[char], i: &mut usize, out: &mut String) -> bool {
    while *i < chars.len() {
        let d = chars[*i];
        out.push(d);
        *i += 1;
        if d == '\\' {
            if *i < chars.len() {
                out.push(chars[*i]);
                *i += 1;
            }
        } else if d == '`' {
            return false;
        } else if d == '$' && chars.get(*i) == Some(&'{') {
            out.push('{');
            *i += 1;
            return true;
        }
    }
    false
}

/// Whether a `/` after the output so far starts a regular expression rather
/// than a division. A wrong "regex" guess is harmless (the span is copied
/// verbatim); a wrong "division" guess is caught by the program check.
fn regex_allowed(out: &str) -> bool {
    let Some(last) = out.chars().next_back() else {
        return true;
    };
    if is_ident_char(last) {
        let word: String = {
            let mut w: Vec<char> = out.chars().rev().take_while(|&ch| is_ident_char(ch)).collect();
            w.reverse();
            w.into_iter().collect()
        };
        return matches!(
            word.as_str(),
            "return"
                | "typeof"
                | "instanceof"
                | "in"
                | "of"
                | "new"
                | "delete"
                | "void"
                | "throw"
                | "case"
                | "do"
                | "else"
                | "yield"
                | "await"
        );
    }
    !matches!(last, ')' | ']' | '}' | '\'' | '"' | '`')
}

/// End index (exclusive, flags included) of a regex literal starting at the
/// `/` at `start`, or `None` if none closes on this line.
fn scan_regex(chars: &[char], start: usize) -> Option<usize> {
    let mut i = start + 1;
    let mut in_class = false;
    while i < chars.len() {
        match chars[i] {
            '\n' | '\r' => return None,
            '\\' => i += 1,
            '[' => in_class = true,
            ']' => in_class = false,
            '/' if !in_class => {
                i += 1;
                while i < chars.len() && is_ident_char(chars[i]) {
                    i += 1;
                }
                return Some(i);
            },
            _ => {},
        }
        i += 1;
    }
    None
}

/// Identifier (or number) character: two of these must not touch.
fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$' || c == '\\' || (!c.is_ascii() && !c.is_whitespace())
}

/// Whether dropping the whitespace between `prev` and a token starting with
/// `next` would merge two tokens into a different one.
fn needs_space(prev: Option<char>, next: char) -> bool {
    let Some(p) = prev else {
        return false;
    };
    (is_ident_char(p) && is_ident_char(next))
        || (p == '+' && next == '+')
        || (p == '-' && next == '-')
        || (p == '/' && (next == '/' || next == '*'))
        || (p.is_ascii_digit() && next == '.')
        || (p == '<' && next == '!')
        || (p == '-' && next == '>')
}

/// Scan JS for class/ID references in DOM APIs (`getElementById`,
/// `querySelector`/`All`, `classList.*`).
pub fn extract_js_references(js: &str, symbols: &mut SymbolMap, rename_classes: bool, rename_ids: bool) {
    if rename_ids {
        extract_function_string_args(js, "getElementById", |name| {
            symbols.register_id(name);
        });
    }

    if rename_classes {
        for func in &[
            "classList.add",
            "classList.remove",
            "classList.toggle",
            "classList.contains",
        ] {
            extract_function_string_args(js, func, |name| {
                symbols.register_class(name);
            });
        }
    }

    if rename_classes || rename_ids {
        for func in &["querySelector", "querySelectorAll"] {
            extract_function_string_args(js, func, |selector| {
                extract_selectors_from_query(selector, symbols, rename_classes, rename_ids);
            });
        }
    }
}

/// Extract string arguments from function calls like `funcName("value")`.
fn extract_function_string_args(js: &str, func_name: &str, mut callback: impl FnMut(&str)) {
    let mut search_from = 0;
    while let Some(pos) = js[search_from..].find(func_name) {
        let abs_pos = search_from + pos + func_name.len();
        let rest = &js[abs_pos..];

        let rest = rest.trim_start();
        if let Some(rest) = rest.strip_prefix('(') {
            let rest = rest.trim_start();
            if let Some(value) = extract_string_literal(rest) {
                callback(&value);
            }
        }
        search_from = abs_pos;
    }
}

/// Extract a string literal value from the start of a string slice.
fn extract_string_literal(s: &str) -> Option<String> {
    let s = s.trim_start();
    let quote = s.chars().next()?;
    if quote != '"' && quote != '\'' {
        return None;
    }

    let mut value = String::new();
    let mut chars = s[1..].chars();
    loop {
        let ch = chars.next()?;
        if ch == '\\' {
            if let Some(escaped) = chars.next() {
                value.push(escaped);
            }
        } else if ch == quote {
            return Some(value);
        } else {
            value.push(ch);
        }
    }
}

/// Extract class/ID names from a CSS selector string (as used in querySelector).
fn extract_selectors_from_query(selector: &str, symbols: &mut SymbolMap, rename_classes: bool, rename_ids: bool) {
    let chars: Vec<char> = selector.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        if chars[i] == '.' && rename_classes {
            i += 1;
            let start = i;
            while i < len && (chars[i].is_ascii_alphanumeric() || chars[i] == '-' || chars[i] == '_') {
                i += 1;
            }
            if i > start {
                let name: String = chars[start..i].iter().collect();
                symbols.register_class(&name);
            }
        } else if chars[i] == '#' && rename_ids {
            i += 1;
            let start = i;
            while i < len && (chars[i].is_ascii_alphanumeric() || chars[i] == '-' || chars[i] == '_') {
                i += 1;
            }
            if i > start {
                let name: String = chars[start..i].iter().collect();
                symbols.register_id(&name);
            }
        } else {
            i += 1;
        }
    }
}

/// Extract class name prefixes used in JS string concatenation.
///
/// Finds patterns like `'tier-' +` or `"tier-border-" +` and returns the
/// trailing CSS-name prefix (e.g., `tier-`, `tier-border-`).
pub fn extract_concatenation_prefixes(js: &str) -> Vec<String> {
    let mut prefixes = Vec::new();
    let chars: Vec<char> = js.chars().collect();
    let len = chars.len();
    let mut i = 0;
    let mut state = State::Normal;

    while i < len {
        match state {
            State::Normal => {
                if i + 1 < len && chars[i] == '/' && chars[i + 1] == '/' {
                    state = State::SingleLineComment;
                    i += 2;
                } else if i + 1 < len && chars[i] == '/' && chars[i + 1] == '*' {
                    state = State::MultiLineComment;
                    i += 2;
                } else if chars[i] == '\'' || chars[i] == '"' {
                    let quote = chars[i];
                    i += 1;
                    let start = i;
                    while i < len {
                        if chars[i] == '\\' && i + 1 < len {
                            i += 2;
                        } else if chars[i] == quote {
                            break;
                        } else {
                            i += 1;
                        }
                    }
                    let content: String = chars[start..i].iter().collect();
                    if i < len {
                        i += 1; // skip closing quote
                    }
                    // Check if followed by +
                    let mut j = i;
                    while j < len && chars[j].is_ascii_whitespace() {
                        j += 1;
                    }
                    if j < len && chars[j] == '+' && content.ends_with('-') {
                        if let Some(prefix) = extract_trailing_prefix(&content) {
                            if !prefixes.contains(&prefix) {
                                prefixes.push(prefix);
                            }
                        }
                    }
                } else if chars[i] == '`' {
                    state = State::TemplateString;
                    i += 1;
                } else {
                    i += 1;
                }
            },
            State::SingleLineComment => {
                if chars[i] == '\n' {
                    state = State::Normal;
                }
                i += 1;
            },
            State::MultiLineComment => {
                if chars[i] == '*' && i + 1 < len && chars[i + 1] == '/' {
                    i += 2;
                    state = State::Normal;
                } else {
                    i += 1;
                }
            },
            State::TemplateString => {
                if chars[i] == '\\' && i + 1 < len {
                    i += 2;
                } else if chars[i] == '`' {
                    state = State::Normal;
                    i += 1;
                } else {
                    i += 1;
                }
            },
            _ => {
                i += 1;
            },
        }
    }

    prefixes
}

/// Extract the trailing CSS class name prefix from a string.
/// E.g., from `class="exec-banner tier-border-`, returns `tier-border-`.
fn extract_trailing_prefix(s: &str) -> Option<String> {
    let bytes = s.as_bytes();
    let len = bytes.len();
    if len == 0 || bytes[len - 1] != b'-' {
        return None;
    }

    let mut start = len;
    while start > 0 {
        let ch = bytes[start - 1];
        if ch.is_ascii_alphanumeric() || ch == b'-' || ch == b'_' {
            start -= 1;
        } else {
            break;
        }
    }

    let prefix = &s[start..];
    if prefix.len() > 1 {
        Some(prefix.to_owned())
    } else {
        None
    }
}

/// Replace class/ID names in text via word-boundary matching, for non-JS
/// script content (e.g. JSON) where the names appear as values.
pub fn replace_symbols_word_boundary(
    text: &str,
    symbols: &crate::symbol_map::SymbolMap,
    rename_classes: bool,
    rename_ids: bool,
) -> String {
    let mut result = text.to_owned();
    if rename_classes {
        if let Some((ac, repls)) = ac_from_map(symbols.classes()) {
            result = replace_words_ac(&result, &ac, &repls);
        }
    }
    if rename_ids {
        if let Some((ac, repls)) = ac_from_map(symbols.ids()) {
            result = replace_words_ac(&result, &ac, &repls);
        }
    }
    result
}

/// Build a leftmost-longest automaton over a name map's originals, plus the
/// aligned replacements (indexed by pattern id). `None` if the map is empty.
fn ac_from_map(map: &HashMap<String, String>) -> Option<(AhoCorasick, Vec<String>)> {
    if map.is_empty() {
        return None;
    }
    let patterns: Vec<&str> = map.keys().map(String::as_str).collect();
    let repls: Vec<String> = patterns.iter().map(|p| map[*p].clone()).collect();
    let ac = AhoCorasick::builder()
        .match_kind(MatchKind::LeftmostLongest)
        .build(&patterns)
        .ok()?;
    Some((ac, repls))
}

/// Replace whole-word matches in one linear pass: leftmost-longest plus a
/// class/ID boundary check, so a name is never rewritten inside a larger ident.
fn replace_words_ac(text: &str, ac: &AhoCorasick, repls: &[String]) -> String {
    let bytes = text.as_bytes();
    let mut out = String::with_capacity(text.len());
    let mut last = 0;
    for m in ac.find_iter(text) {
        let (s, e) = (m.start(), m.end());
        let before_ok = s == 0 || !is_css_name_char(bytes[s - 1]);
        let after_ok = e >= bytes.len() || !is_css_name_char(bytes[e]);
        if before_ok && after_ok {
            out.push_str(&text[last..s]);
            out.push_str(&repls[m.pattern().as_usize()]);
            last = e;
        }
    }
    out.push_str(&text[last..]);
    out
}

fn is_css_name_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'-'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_string_literals() {
        use rand::SeedableRng;
        let mut rng = StdRng::seed_from_u64(42);
        let input = r#"var x = "hello";"#;
        let result = encode_js_strings(input, &mut rng);
        assert!(!result.contains("hello"));
        // At least one escape form must appear.
        assert!(result.contains("\\x") || result.contains("\\u"));
    }

    #[test]
    fn minify_removes_comments() {
        let input = "var x = 1; // comment\nvar y = 2;";
        let result = minify_js(input);
        assert!(!result.contains("comment"));
        assert!(result.contains("var x"));
    }

    #[test]
    fn minify_preserves_strings() {
        let input = r#"var x = "  spaces  ";"#;
        let result = minify_js(input);
        assert!(result.contains("  spaces  "));
    }

    /// Minify through the public path (with the same-program check) and assert
    /// the output is still the same program.
    fn min_checked(js: &str) -> String {
        use rand::SeedableRng;
        let mut rng = StdRng::seed_from_u64(1);
        let out = transform_js(js, &SymbolMap::new(Some(1)), JsStringEncoding::None, true, false, false, &mut rng);
        assert!(crate::js_ast::same_program(js, &out), "not the same program:\n{js}\n---\n{out}");
        out
    }

    #[test]
    fn minify_keeps_line_break_between_statements() {
        // Regression: a real WordPress (ea11y) script. Dropping the newline after
        // the arrow-function body produced `}document...`, a SyntaxError.
        let input = "const onSkipLinkClick = () => {\n  const h = document.querySelector('html');\n  h.style['scroll-behavior'] = 'smooth';\n}\ndocument.addEventListener(\"DOMContentLoaded\", () => {\n  if (!document.querySelector('#content')) { document.querySelector('.ea11y-skip-to-content-link').remove(); }\n});";
        let out = minify_js(input);
        assert!(out.contains("}\ndocument.addEventListener"), "{out}");
        assert!(!out.contains("  "), "indentation not removed: {out}");
        min_checked(input);
    }

    #[test]
    fn minify_keeps_return_newline() {
        // `return\nx` returns undefined; `return x` would change behaviour.
        let input = "function f(){\n  return\n  x;\n}";
        let out = minify_js(input);
        assert!(out.contains("return\nx"), "{out}");
        assert!(!crate::js_ast::same_program(input, "function f(){return x;}"));
    }

    #[test]
    fn minify_line_comment_keeps_separator() {
        let input = "x = 1 // one\ny = 2";
        assert_eq!(minify_js(input), "x=1\ny=2");
        let input = "a /* c */ b";
        assert_eq!(minify_js(input), "a b");
        let input = "a = 1 /* line\nbreak */ b = 2";
        assert_eq!(minify_js(input), "a=1\nb=2");
    }

    #[test]
    fn minify_copies_regex_literals_verbatim() {
        let input = "var r = s.replace(/\\/\\//g, ''), q = /'  [/]  \"/i;\nvar t = a / b / c;";
        let out = minify_js(input);
        assert!(out.contains("/\\/\\//g"), "{out}");
        assert!(out.contains("/'  [/]  \"/i"), "{out}");
        assert!(out.contains("var t=a/b/c;"), "{out}");
        min_checked(input);
    }

    #[test]
    fn minify_keeps_operator_spacing() {
        assert_eq!(minify_js("a + +b; c - -d; e + ++f"), "a+ +b;c- -d;e+ ++f");
        assert_eq!(minify_js("x = y / /re/.source.length"), "x=y/ /re/.source.length");
        assert_eq!(minify_js("1 .toString()"), "1 .toString()");
        assert_eq!(minify_js("var é = 1"), "var é=1");
    }

    #[test]
    fn minify_nested_template_literals() {
        let input = "var s = `a  ${ x ? `b  ${ {k:1}.k }  c` : d }  e`;";
        let out = minify_js(input);
        assert!(out.contains("`a  ${x?`b  ${{k:1}.k}  c`:d}  e`"), "{out}");
        min_checked(input);
    }

    #[test]
    fn minify_keeps_legal_comments() {
        let out = minify_js("/*! (c) Vendor */\nvar a = 1;");
        assert!(out.starts_with("/*! (c) Vendor */"), "{out}");
    }

    #[test]
    fn minify_falls_back_when_input_does_not_parse() {
        use rand::SeedableRng;
        let mut rng = StdRng::seed_from_u64(1);
        let broken = "var a = ;   // broken\n  var b = 2;";
        let out = transform_js(broken, &SymbolMap::new(Some(1)), JsStringEncoding::None, true, false, false, &mut rng);
        assert_eq!(out, broken);
    }

    #[test]
    fn extract_getelementbyid() {
        let js = r#"document.getElementById("myId");"#;
        let mut symbols = SymbolMap::new(Some(42));
        extract_js_references(js, &mut symbols, true, true);
        assert!(symbols.get_id("myId").is_some());
    }

    #[test]
    fn extract_classlist_add() {
        let js = r#"el.classList.add("active");"#;
        let mut symbols = SymbolMap::new(Some(42));
        extract_js_references(js, &mut symbols, true, true);
        assert!(symbols.get_class("active").is_some());
    }

    #[test]
    fn extract_queryselector() {
        let js = r#"document.querySelector(".foo #bar");"#;
        let mut symbols = SymbolMap::new(Some(42));
        extract_js_references(js, &mut symbols, true, true);
        assert!(symbols.get_class("foo").is_some());
        assert!(symbols.get_id("bar").is_some());
    }

    #[test]
    fn replace_references_in_strings() {
        let js = r#"var cls = "myClass";"#;
        let mut symbols = SymbolMap::new(Some(42));
        symbols.register_class("myClass");
        let obf = symbols.get_class("myClass").unwrap().to_owned();
        let result = replace_symbol_references(js, &symbols, true, false);
        assert!(result.contains(&obf));
        assert!(!result.contains("myClass"));
    }
}
