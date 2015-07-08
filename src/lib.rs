extern crate unicode_normalization;

use table::{precis_result, PrecisResult};

mod table;
mod case_fold;
mod context;

pub use case_fold::to_casefold;

pub fn check_identifier_class(s: &str) -> bool {
    s.char_indices().all(|(off, c)| match precis_result(c) {
        PrecisResult::PValid => true,
        PrecisResult::FreePValid => false,
        PrecisResult::ContextJ => context::check_contextj(c, s, off),
        PrecisResult::ContextO => context::check_contexto(c, s, off),
        PrecisResult::Unassigned => false,
        PrecisResult::Disallowed => false
    })
}

pub fn check_freeform_class(s: &str) -> bool {
    s.char_indices().all(|(off, c)| match precis_result(c) {
        PrecisResult::PValid => true,
        PrecisResult::FreePValid => true,
        PrecisResult::ContextJ => context::check_contextj(c, s, off),
        PrecisResult::ContextO => context::check_contexto(c, s, off),
        PrecisResult::Unassigned => false,
        PrecisResult::Disallowed => false
    })
}

#[cfg(test)]
mod test {
    use super::{check_identifier_class, check_freeform_class};

    #[test]
    fn virama() {
        assert!(check_identifier_class("\u{0924}\u{094d}\u{200c}\u{0930}\u{093f}"));
        assert!(check_identifier_class("\u{0d23}\u{0d4d}\u{200d}"));
        assert!(check_identifier_class("\u{d7a}"));

        assert!(check_freeform_class("\u{0924}\u{094d}\u{200c}\u{0930}\u{093f}"));
        assert!(check_freeform_class("\u{0d23}\u{0d4d}\u{200d}"));
        assert!(check_freeform_class("\u{d7a}"));
    }
}
