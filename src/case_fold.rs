use std::iter::Iterator;

use table::case_folded;

enum CaseFold {
    Other(&'static [char]),
    This(char)
}

pub struct ToCasefold {
    folded: Option<CaseFold>,
}

impl Iterator for ToCasefold {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        let folded = self.folded.take();
        let folded = if let Some(f) = folded { f } else { return None; };
        match folded {
            CaseFold::This(c) => return Some(c),
            CaseFold::Other(folded) => {
                let c = folded[0];
                let folded = &folded[1..];
                if folded.len() > 0 {
                    self.folded = Some(CaseFold::Other(folded))
                }
                return Some(c);
            }
        }
    }
}

pub fn to_casefold(c: char) -> ToCasefold {
    ToCasefold {
        folded: Some(match case_folded(c) {
            None => CaseFold::This(c),
            Some(f) => CaseFold::Other(f)
        })
    }
}

#[cfg(test)]
mod test {
    use super::to_casefold;

    #[test]
    fn ascii() {
        assert_eq!(to_casefold('A').collect::<String>(), "a");
        assert_eq!(to_casefold('Z').collect::<String>(), "z");
        assert_eq!(to_casefold('a').collect::<String>(), "a");
        assert_eq!(to_casefold('z').collect::<String>(), "z");
    }

    #[test]
    fn ligature() {
        assert_eq!(to_casefold('\u{fb00}').collect::<String>(), "ff");
        assert_eq!(to_casefold('\u{fb02}').collect::<String>(), "fl");
    }

    #[test]
    fn letter_upsilon_with_dialytika_tonos() {
        assert_eq!(to_casefold('\u{3b0}').collect::<String>(), "\u{3c5}\u{308}\u{301}");
    }
}
