use table::{JoiningType, joining_type, Script, script};
use unicode_normalization::char::canonical_combining_class;

fn after(s: &str, off: usize) -> Option<char> {
    s[off..].chars().nth(1)
}

fn before(s: &str, off: usize) -> Option<char> {
    s[..off].chars().rev().next()
}

//
// The following are CONTEXTJ rules
//

pub fn check_contextj(c: char, s: &str, off: usize) -> bool {
    match c {
        '\u{200c}' => zero_width_non_joiner(s, off),
        '\u{200d}' => zero_width_joiner(s, off),
        _ => panic!("Unhandled CONTEXTJ character: U+{:04X}", c as u32)
    }
}

// Check that U+200C ZERO WIDTH NON-JOINER follows a virama (ccc=9)
// or breaks a cursive connection
fn zero_width_non_joiner(s: &str, off: usize) -> bool {
    if let Some(9) = before(s, off).map(canonical_combining_class) { return true; }

    let before = s[..off].chars()
                         .rev()
                         .map(joining_type)
                         .skip_while(|&j| j == JoiningType::Transparent)
                         .next();
    match before {
        Some(JoiningType::LeftJoining) | Some(JoiningType::DualJoining) => (),
        _ => return false
    }

    let after = s[off..].chars()
                        .skip(1)
                        .map(joining_type)
                        .skip_while(|&j| j == JoiningType::Transparent)
                        .next();
    match after {
        Some(JoiningType::RightJoining) | Some(JoiningType::DualJoining) => (),
        _ => return false
    }

    true
}

//
// Check that U+200D ZERO WIDTH JOINER follows a virama (ccc=9)
fn zero_width_joiner(s: &str, off: usize) -> bool {
    if let Some(9) = before(s, off).map(canonical_combining_class) { return true; }
    false
}

//
// The following are CONTEXTO rules
//

pub fn check_contexto(c: char, s: &str, off: usize) -> bool {
    match c {
        '\u{0375}' => greek_lower_numeral_sign(s, off),
        '\u{05f3}' | '\u{05f4}' => hebrew_punctuation_geresh(s, off),
        '\u{00b7}' => middle_dot(s, off),
        '\u{0660}'...'\u{0669}' | '\u{06f0}'...'\u{06f9}' => arabic_indic_digits(s, off),
        _ => panic!("Unhandled CONTEXTO character: U+{:04X}", c as u32)
    }
}

// Check whether U+0375 GREEK LOWER NUMERAL SIGN is followed by a greek character
fn greek_lower_numeral_sign(s: &str, off: usize) -> bool {
    if let Some(c) = after(s, off) {
        if script(c) == Script::Greek { return true; }
    }
    false
}

// Check whether U+05F3 HEBREW PUNCTUATION GERESH or U+05F4 HEBREW PUNCTUATION GERESHAYIM are
// preceded by a hebrew character
fn hebrew_punctuation_geresh(s: &str, off: usize) -> bool {
    if let Some(c) = before(s, off) {
        if script(c) == Script::Hebrew { return true; }
    }
    false
}

// Check wheter U+00B7 MIDDLE DOT is allowed in this position
// Between 'l' (U+006C) characters only,
// used to permit the Catalan character ela geminada to be expressed.
fn middle_dot(s: &str, off: usize) -> bool {
    before(s, off).map(|c| c == 'l').unwrap_or(false)
    && after(s, off).map(|c| c == 'l').unwrap_or(false)
}

// Arabic-Indic Digits (U+0660-U+0669) may not be mixed with
// Extended Arabic-Indic Digits (U+06F0-U+06F9)
fn arabic_indic_digits(s: &str, _off: usize) -> bool {
    s.chars().any(|c| match c { '\u{0660}'...'\u{0669}' => true, _ => false })
    ^ s.chars().any(|c| match c { '\u{06f0}'...'\u{06f9}' => true, _ => false })
}

#[cfg(test)]
mod test {
    use super::{zero_width_non_joiner, zero_width_joiner, greek_lower_numeral_sign,
                hebrew_punctuation_geresh, middle_dot, arabic_indic_digits};

    #[test]
    fn zwj_zwnj_no_context() {
        assert!(!zero_width_non_joiner("x\u{200c}y", 1));
        assert!(!zero_width_non_joiner("x\u{200d}y", 1));
        assert!(!zero_width_non_joiner("x\u{200c}", 1));
        assert!(!zero_width_non_joiner("x\u{200d}", 1));
        assert!(!zero_width_non_joiner("\u{200c}y", 0));
        assert!(!zero_width_non_joiner("\u{200d}y", 0));
    }

    #[test]
    fn break_cursive_connection() {
        assert!(zero_width_non_joiner("\u{0635}\u{200c}\u{0644}", 2));
        assert!(zero_width_non_joiner("\u{0635}\u{0651}\u{200c}\u{0644}", 4));
        assert!(zero_width_non_joiner("\u{0635}\u{200c}\u{20dd}\u{0644}", 2));
        assert!(zero_width_non_joiner("\u{0635}\u{0651}\u{200c}\u{20dd}\u{0644}", 4));
    }

    #[test]
    fn virama_zwj() {
        assert!(zero_width_joiner("\u{0928}\u{094d}\u{200d}", 6));
    }

    #[test]
    fn virama_zwnj() {
        assert!(zero_width_non_joiner("\u{0924}\u{094d}\u{200c}\u{0930}\u{093f}", 6));
    }

    #[test]
    fn greek_numeral() {
        assert!(greek_lower_numeral_sign("\u{0375}\u{03b1}", 0));
        assert!(!greek_lower_numeral_sign("\u{0375}", 0));
        assert!(!greek_lower_numeral_sign("\u{0375}x", 0));
    }

    #[test]
    fn hebrew_geresh() {
        assert!(hebrew_punctuation_geresh("\u{05d2}\u{05f3}", 2));
        assert!(!hebrew_punctuation_geresh("\u{05f3}", 0));
        assert!(!hebrew_punctuation_geresh("x\u{05f3}", 1));
    }

    #[test]
    fn hebrew_gereshayim() {
        assert!(hebrew_punctuation_geresh("\u{05d2}\u{05f4}", 2));
        assert!(!hebrew_punctuation_geresh("\u{05f4}", 0));
        assert!(!hebrew_punctuation_geresh("x\u{05f4}", 1));
    }

    #[test]
    fn ela_gemiada() {
        assert!(middle_dot("l\u{b7}l", 1));
        assert!(!middle_dot("x\u{b7}", 1));
        assert!(!middle_dot("\u{b7}y", 0));
        assert!(!middle_dot("x\u{b7}l", 1));
        assert!(!middle_dot("l\u{b7}y", 1));
        assert!(!middle_dot("x\u{b7}y", 1));
    }

    #[test]
    fn mixed_arabic_inidic_digits() {
        assert!(arabic_indic_digits("\u{0660}\u{0661}", 0));
        assert!(!arabic_indic_digits("\u{0660}\u{06f1}", 0));
        assert!(arabic_indic_digits("\u{0660}\u{0661}", 2));
        assert!(!arabic_indic_digits("\u{0660}\u{06f1}", 2));
        assert!(arabic_indic_digits("\u{06f0}\u{06f1}", 0));
        assert!(!arabic_indic_digits("\u{06f0}\u{0661}", 0));
        assert!(arabic_indic_digits("\u{06f0}\u{06f1}", 2));
        assert!(!arabic_indic_digits("\u{06f0}\u{0661}", 2));
        assert!(arabic_indic_digits("\u{0660}x", 0));
        assert!(arabic_indic_digits("\u{06f0}x", 0));
        assert!(arabic_indic_digits("x\u{0660}", 1));
        assert!(arabic_indic_digits("x\u{06f0}", 1));
    }
}
