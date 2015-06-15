use table::{precis_result, PrecisResult};

mod table;

pub fn check_identifier_class(s: &str) -> bool {
    s.chars().all(|x| match precis_result(x) {
        PrecisResult::PValid => true,
        PrecisResult::FreePValid => false,
        PrecisResult::ContextJ => panic!("CONTEXTJ checking not implemented"),
        PrecisResult::ContextO => panic!("CONTEXTO checking not implemented"),
        PrecisResult::Unassigned => false,
        PrecisResult::Disallowed => false
    })
}

pub fn check_freeform_class(s: &str) -> bool {
    s.chars().all(|x| match precis_result(x) {
        PrecisResult::PValid => true,
        PrecisResult::FreePValid => true,
        PrecisResult::ContextJ => panic!("CONTEXTJ checking not implemented"),
        PrecisResult::ContextO => panic!("CONTEXTO checking not implemented"),
        PrecisResult::Unassigned => false,
        PrecisResult::Disallowed => false
    })
}
