use rand::rngs::StdRng;
use rand::RngExt;

/// Encode a string into a mix of HTML entities (decimal, hex, named).
///
/// Each character is randomly encoded using one of three forms,
/// making pattern recognition harder while remaining valid HTML.
///
/// `text` is the raw source text of a node, so it may already contain character
/// references (`&raquo;`, `&#8220;`). Those are copied through verbatim: encoding
/// their `&` would turn `&raquo;` into the literal text "&raquo;".
pub fn encode_entities(text: &str, rng: &mut StdRng) -> String {
    let mut out = String::with_capacity(text.len() * 6);
    let mut i = 0;
    while i < text.len() {
        let rest = &text[i..];
        let ch = rest.chars().next().expect("non-empty remainder");
        if ch == '&' {
            let n = reference_len(rest);
            if n > 0 {
                out.push_str(&rest[..n]);
                i += n;
                continue;
            }
        }
        i += ch.len_utf8();
        if ch == '\n' || ch == '\r' {
            // Line breaks stay literal: the parser drops a literal newline right after
            // `<pre>` / `<textarea>` / `<listing>` but keeps `&#10;`, and turns a literal CRLF
            // into one LF while `&#13;` stays a CR that renders as a space in `<pre>`.
            out.push(ch);
            continue;
        }
        if let Some(named) = named_entity(ch) {
            match rng.random_range(0u8..3) {
                0 => out.push_str(named),
                1 => encode_decimal(ch, &mut out),
                _ => encode_hex(ch, &mut out),
            }
        } else if ch.is_ascii_alphanumeric() || ch == ' ' {
            // Randomly encode or pass through for common chars
            match rng.random_range(0u8..3) {
                0 => out.push(ch),
                1 => encode_decimal(ch, &mut out),
                _ => encode_hex(ch, &mut out),
            }
        } else {
            // Non-ASCII or special chars: always encode
            match rng.random_range(0u8..2) {
                0 => encode_decimal(ch, &mut out),
                _ => encode_hex(ch, &mut out),
            }
        }
    }
    out
}

/// Encode a single attribute value into entities.
/// Preserves structure needed for attribute parsing.
///
/// Like [`encode_entities`], references already present in the source value
/// (`&quot;` in a `data-settings` JSON attribute, for one) pass through verbatim.
pub fn encode_attr_value(value: &str, rng: &mut StdRng) -> String {
    let mut out = String::with_capacity(value.len() * 6);
    let mut i = 0;
    while i < value.len() {
        let rest = &value[i..];
        let ch = rest.chars().next().expect("non-empty remainder");
        if ch == '&' {
            let n = reference_len(rest);
            if n > 0 {
                out.push_str(&rest[..n]);
                i += n;
                continue;
            }
        }
        i += ch.len_utf8();
        if ch == '\n' || ch == '\r' {
            // A literal CRLF in an attribute value is one LF; `&#13;&#10;` would stay CR LF.
            out.push(ch);
            continue;
        }
        match rng.random_range(0u8..3) {
            0 => out.push(ch),
            1 => encode_decimal(ch, &mut out),
            _ => encode_hex(ch, &mut out),
        }
    }
    out
}

/// Byte length of the character reference, or reference-shaped run, that starts
/// at the `&` in `s[0]`; `0` when the `&` is not followed by one.
///
/// Deliberately a *shape* test (`&` + optional `#`/`#x` + up to 32 ASCII
/// alphanumerics + optional `;`), not a lookup of valid names. Copying the bytes
/// unchanged keeps whatever meaning the browser gives them: a real entity stays
/// an entity, `&foo;` stays literal text, and a legacy reference without a
/// semicolon (`&copy 2026`) is not split by encoding the letters after the `&`.
fn reference_len(s: &str) -> usize {
    let b = s.as_bytes();
    debug_assert_eq!(b.first(), Some(&b'&'));
    let mut i = 1;
    if b.get(i) == Some(&b'#') {
        i += 1;
        if matches!(b.get(i), Some(b'x' | b'X')) {
            i += 1;
        }
    }
    let start = i;
    while i < b.len() && i - start < 32 && b[i].is_ascii_alphanumeric() {
        i += 1;
    }
    if i == start {
        return 0;
    }
    if b.get(i) == Some(&b';') {
        i += 1;
    }
    i
}

/// Decode the character references in `text` when every one of them is a numeric
/// reference or a name from a small table of common ones. `None` as soon as an
/// `&` is followed by a name this table does not know, so the caller can leave
/// that text alone instead of guessing (an unknown name may be a real entity).
///
/// Used where raw source text must become literal characters: the structural
/// restore script assigns `textContent`, which does not parse entities.
pub fn decode_known_references(text: &str) -> Option<String> {
    let mut out = String::with_capacity(text.len());
    let mut i = 0;
    while i < text.len() {
        let rest = &text[i..];
        let ch = rest.chars().next().expect("non-empty remainder");
        if ch != '&' {
            out.push(ch);
            i += ch.len_utf8();
            continue;
        }
        let n = reference_len(rest);
        if n == 0 {
            out.push('&'); // a lone ampersand is literal
            i += 1;
            continue;
        }
        let token = &rest[..n];
        let body = token[1..].strip_suffix(';')?; // no semicolon: a legacy/unknown form, do not guess
        let decoded = if let Some(num) = body.strip_prefix('#') {
            let code = match num.strip_prefix(['x', 'X']) {
                Some(hex) => u32::from_str_radix(hex, 16).ok()?,
                None => num.parse::<u32>().ok()?,
            };
            char::from_u32(code).filter(|c| *c != '\0')?
        } else {
            named_reference(body)?
        };
        out.push(decoded);
        i += n;
    }
    Some(out)
}

fn named_reference(name: &str) -> Option<char> {
    Some(match name {
        "amp" => '&',
        "lt" => '<',
        "gt" => '>',
        "quot" => '"',
        "apos" => '\'',
        "nbsp" => '\u{a0}',
        "copy" => '\u{a9}',
        "reg" => '\u{ae}',
        "laquo" => '\u{ab}',
        "raquo" => '\u{bb}',
        "ndash" => '\u{2013}',
        "mdash" => '\u{2014}',
        "lsquo" => '\u{2018}',
        "rsquo" => '\u{2019}',
        "ldquo" => '\u{201c}',
        "rdquo" => '\u{201d}',
        "hellip" => '\u{2026}',
        "bull" => '\u{2022}',
        "middot" => '\u{b7}',
        "times" => '\u{d7}',
        _ => return None,
    })
}

fn encode_decimal(ch: char, out: &mut String) {
    out.push_str(&format!("&#{};", ch as u32));
}

fn encode_hex(ch: char, out: &mut String) {
    out.push_str(&format!("&#x{:x};", ch as u32));
}

fn named_entity(ch: char) -> Option<&'static str> {
    match ch {
        '&' => Some("&amp;"),
        '<' => Some("&lt;"),
        '>' => Some("&gt;"),
        '"' => Some("&quot;"),
        '\'' => Some("&#39;"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::SeedableRng;

    /// Regression: a newline written as `&#10;` right after `<textarea>` / `<pre>` is NOT dropped
    /// by the parser (a literal one is), so every edit form gained a leading blank line.
    #[test]
    fn line_breaks_are_never_entity_encoded() {
        for seed in 0..50u64 {
            let mut rng = StdRng::seed_from_u64(seed);
            let text = encode_entities("\nline one\r\nline two\rend", &mut rng);
            assert!(text.starts_with('\n'), "seed {seed}: {text}");
            assert!(text.contains("\r\n"), "seed {seed}: CRLF must stay literal: {text}");
            assert!(!text.contains("&#10;") && !text.contains("&#xa;") && !text.contains("&#13;"), "{text}");
            let attr = encode_attr_value("a\nb\r\nc", &mut rng);
            assert!(attr.contains('\n') && attr.contains("\r\n"), "seed {seed}: {attr}");
            assert!(!attr.contains("&#10;") && !attr.contains("&#xa;") && !attr.contains("&#13;"), "{attr}");
        }
    }

    #[test]
    fn encode_entities_roundtrip_safe() {
        let mut rng = StdRng::seed_from_u64(42);
        let input = "Hello <World> & 'friends'";
        let encoded = encode_entities(input, &mut rng);
        // < and > must be entity-encoded, never left raw
        assert!(!encoded.contains('<'));
        assert!(!encoded.contains('>'));
    }

    /// The decoded meaning of `s` as a browser would read it for the cases the
    /// tests use (numeric references plus the small named table).
    fn meaning(s: &str) -> String {
        decode_known_references(s).unwrap_or_else(|| panic!("undecodable in test: {s:?}"))
    }

    #[test]
    fn existing_references_are_not_double_encoded() {
        // Taken from a real WordPress page: titles, quotes inside a JSON attribute.
        let samples = [
            "Suapan Lembaga Kemajuan Pertanian Kemubu (KADA) &raquo;",
            "Keputusan carian untuk &#8220;padi&#8221; &#8211; KADA",
            "{&quot;background_background&quot;:&quot;classic&quot;}",
            "a &amp; b &lt;c&gt; &#x27;d&#x27; &nbsp; e",
            "5 & 6 and a trailing &",
        ];
        for seed in 0..50u64 {
            for s in samples {
                let mut rng = StdRng::seed_from_u64(seed);
                let t = encode_entities(s, &mut rng);
                assert_eq!(meaning(&t), meaning(s), "text seed {seed}: {s:?} -> {t:?}");
                let mut rng = StdRng::seed_from_u64(seed);
                let a = encode_attr_value(s, &mut rng);
                assert_eq!(meaning(&a), meaning(s), "attr seed {seed}: {s:?} -> {a:?}");
            }
        }
    }

    #[test]
    fn source_references_are_copied_verbatim() {
        let mut rng = StdRng::seed_from_u64(1);
        let out = encode_entities("x&raquo;y&#8220;z&copy 1", &mut rng);
        for token in ["&raquo;", "&#8220;", "&copy"] {
            assert!(out.contains(token), "{token} must survive intact: {out}");
        }
        // A reference was never re-encoded into an escaped ampersand.
        assert!(!out.contains("&amp;raquo"), "{out}");
        assert!(!out.contains("&#38;"), "{out}");
        assert!(!out.contains("&#x26;"), "{out}");
    }

    #[test]
    fn an_ampersand_that_is_not_an_entity_stays_literal() {
        // `&T` / `&A` are not entities, so a browser shows them as written; the
        // run after the `&` is copied as is so nothing can turn it into one.
        let mut rng = StdRng::seed_from_u64(3);
        let out = encode_entities("AT&T and Q&A", &mut rng);
        assert!(out.contains("&T"), "{out}");
        assert!(out.contains("&A"), "{out}");
        assert!(!out.contains("&#38;"), "{out}");
    }

    #[test]
    fn a_lone_ampersand_is_still_encoded() {
        let mut rng = StdRng::seed_from_u64(1);
        let out = encode_entities("a & b", &mut rng);
        // Not followed by a name: it is a literal ampersand, and must stay one.
        assert_eq!(meaning(&out), "a & b", "{out}");
    }

    #[test]
    fn decode_refuses_names_it_does_not_know() {
        assert_eq!(decode_known_references("&raquo;"), Some("\u{bb}".to_string()));
        assert_eq!(
            decode_known_references("&#8220;x&#x201D;"),
            Some("\u{201c}x\u{201d}".to_string())
        );
        assert_eq!(decode_known_references("5 & 6"), Some("5 & 6".to_string()));
        assert_eq!(decode_known_references("&eacute;"), None, "unknown name: do not guess");
        assert_eq!(
            decode_known_references("&copy 2026"),
            None,
            "no semicolon: do not guess"
        );
        assert_eq!(decode_known_references("&#0;"), None);
        assert_eq!(decode_known_references("&#xD800;"), None, "lone surrogate");
    }

    #[test]
    fn deterministic_with_same_seed() {
        let mut rng1 = StdRng::seed_from_u64(99);
        let mut rng2 = StdRng::seed_from_u64(99);
        let input = "test string";
        assert_eq!(encode_entities(input, &mut rng1), encode_entities(input, &mut rng2),);
    }
}
