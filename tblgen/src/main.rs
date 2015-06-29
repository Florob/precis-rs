extern crate hyper;
extern crate unicode_normalization;

use std::collections::{BTreeSet, HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::iter;
use std::str::FromStr;
use std::fmt;

use hyper::Client;
use unicode_normalization::UnicodeNormalization;

#[derive(Copy, Clone, Debug, PartialEq)]
enum GeneralCategory {
    UppercaseLetter,      // Lu
    LowercaseLetter,      // Ll
    TitlecaseLetter,      // Lt
    // CasedLetter,       // LC = Lu | Ll | Lt
    ModifierLetter,       // Lm
    OtherLetter,          // Lo
    // Letter,            // L = Lu | Ll | Lt | Lm | Lo
    NonspacingMark,       // Mn
    SpacingMark,          // Mc
    EnclosingMark,        // Me
    // Mark,              // M = Mn | Mc | Me
    DecimalNumber,        // Nd
    LetterNumber,         // Nl
    OtherNumber,          // No
    // Number,            // N = Nd | Nl | No
    ConnectorPunctuation, // Pc
    DashPunctuation,      // Pd
    OpenPunctuation,      // Ps
    ClosePunctuation,     // Pe
    InitialPunctuation,   // Pi
    FinalPunctuation,     // Pf
    OtherPunctuation,     // Po
    // Punctuation,       // P = Pc | Pd | Ps | Pe | Pi | Pf | Po
    MathSymbol,           // Sm
    CurrencySymbol,       // Sc
    ModifierSymbol,       // Sk
    OtherSymbol,          // So
    // Symbol,            // S = Sm | Sc | Sk | So
    SpaceSeparator,       // Zs
    LineSeparator,        // Zl
    ParagraphSeparator,   // Zp
    // Separator,         // Z = Zs | Zl | Zp
    Control,              // Cc
    Format,               // Cf
    Surrogate,            // Cs
    PrivateUse,           // Co
    Unassigned,           // Cn
    // Other              // C = Cc | Cf | Cs | Co | Cn
}

impl FromStr for GeneralCategory {
    type Err = ();

    fn from_str(s: &str) -> Result<GeneralCategory, ()> {
        match s {
            "Lu" => Ok(GeneralCategory::UppercaseLetter),
            "Ll" => Ok(GeneralCategory::LowercaseLetter),
            "Lt" => Ok(GeneralCategory::TitlecaseLetter),
            "Lm" => Ok(GeneralCategory::ModifierLetter),
            "Lo" => Ok(GeneralCategory::OtherLetter),
            "Mn" => Ok(GeneralCategory::NonspacingMark),
            "Mc" => Ok(GeneralCategory::SpacingMark),
            "Me" => Ok(GeneralCategory::EnclosingMark),
            "Nd" => Ok(GeneralCategory::DecimalNumber),
            "Nl" => Ok(GeneralCategory::LetterNumber),
            "No" => Ok(GeneralCategory::OtherNumber),
            "Pc" => Ok(GeneralCategory::ConnectorPunctuation),
            "Pd" => Ok(GeneralCategory::DashPunctuation),
            "Ps" => Ok(GeneralCategory::OpenPunctuation),
            "Pe" => Ok(GeneralCategory::ClosePunctuation),
            "Pi" => Ok(GeneralCategory::InitialPunctuation),
            "Pf" => Ok(GeneralCategory::FinalPunctuation),
            "Po" => Ok(GeneralCategory::OtherPunctuation),
            "Sm" => Ok(GeneralCategory::MathSymbol),
            "Sc" => Ok(GeneralCategory::CurrencySymbol),
            "Sk" => Ok(GeneralCategory::ModifierSymbol),
            "So" => Ok(GeneralCategory::OtherSymbol),
            "Zs" => Ok(GeneralCategory::SpaceSeparator),
            "Zl" => Ok(GeneralCategory::LineSeparator),
            "Zp" => Ok(GeneralCategory::ParagraphSeparator),
            "Cc" => Ok(GeneralCategory::Control),
            "Cf" => Ok(GeneralCategory::Format),
            "Cs" => Ok(GeneralCategory::Surrogate),
            "Co" => Ok(GeneralCategory::PrivateUse),
            "Cn" => Ok(GeneralCategory::Unassigned),
            _ => Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum HangulSyllableType {
    LeadingJamo,  // L
    VowelJamo,    // V
    TrailingJamo, // T
    LVSyllable,   // LV
    LVTSyllable   // LVT
}

impl FromStr for HangulSyllableType {
    type Err = ();

    fn from_str(s: &str) -> Result<HangulSyllableType, ()> {
        match s {
            "L" => Ok(HangulSyllableType::LeadingJamo),
            "V" => Ok(HangulSyllableType::VowelJamo),
            "T" => Ok(HangulSyllableType::TrailingJamo),
            "LV" => Ok(HangulSyllableType::LVSyllable),
            "LVT" => Ok(HangulSyllableType::LVTSyllable),
            _ => Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum JoiningType {
    RightJoining, // R
    LeftJoining,  // L
    DualJoining,  // D
    JoinCausing,  // C
    NonJoining,   // U
    Transparent   // T
}

impl FromStr for JoiningType {
    type Err = ();

    fn from_str(s: &str) -> Result<JoiningType, ()> {
        match s {
            "R" => Ok(JoiningType::RightJoining),
            "L" => Ok(JoiningType::LeftJoining),
            "D" => Ok(JoiningType::DualJoining),
            "C" => Ok(JoiningType::JoinCausing),
            "U" => Ok(JoiningType::NonJoining),
            "T" => Ok(JoiningType::Transparent),
            _ => Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum PrecisResult {
    PValid,
    FreePValid,
    ContextJ,
    ContextO,
    Unassigned,
    Disallowed
}

impl fmt::Display for PrecisResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", match *self {
            PrecisResult::PValid => "PVALID",
            PrecisResult::FreePValid => "FREE_PVAL",
            PrecisResult::ContextJ => "CONTEXTJ",
            PrecisResult::ContextO => "CONTEXTO",
            PrecisResult::Unassigned => "UNASSIGNED",
            PrecisResult::Disallowed => "DISALLOWED"
        })
    }
}

fn is_noncharacter(c: u32, noncharacters: &HashSet<u32>) -> bool {
    noncharacters.contains(&c)
}

// J: General_Category(cp) is in {Cn} and Noncharacter_Code_Point(cp) = False
fn is_unassigned(c: u32, gcs: &Vec<GeneralCategory>, ncs: &HashSet<u32>) -> bool {
    gcs[c as usize] == GeneralCategory::Unassigned && !is_noncharacter(c, ncs)
}

// K: cp is in {0021..007E}
fn is_ascii7(c: u32) -> bool {
    if let 0x21...0x7e = c { true } else { false }
}

// H: Join_Control(cp) = True
fn is_join_control(c: u32, jcs: &HashSet<u32>) -> bool {
    jcs.contains(&c)
}

// I: Hangul_Syllable_Type(cp) is in {L, V, T}
fn is_old_hangul_jamo(c: u32, syllable_type: &HashMap<u32, HangulSyllableType>) -> bool {
    if let Some(ty) = syllable_type.get(&c) {
        return match *ty {
            HangulSyllableType::LeadingJamo
            | HangulSyllableType::VowelJamo
            | HangulSyllableType::TrailingJamo => true,
            _ => false
        };
    }
    false
}

// M: Default_Ignorable_Code_Point(cp) = True or
fn is_precis_ignorable(c: u32, default_ignorable: &HashSet<u32>, ncs: &HashSet<u32>) -> bool {
    default_ignorable.contains(&c) || is_noncharacter(c, ncs)
}

// L: Control(cp) = True
// We actually test GeneralCategory(cp) is in {Cc} (This is likely errata to RFC 7564)
fn is_control(c: u32, gcs: &Vec<GeneralCategory>) -> bool {
    gcs[c as usize] == GeneralCategory::Control
}

// Q: toNFKC(cp) != cp
fn is_compat(c: u32) -> bool {
    let c = if let Some(c) = std::char::from_u32(c) { c } else { return false; };
    let nfkc = iter::repeat(c).take(1).nfkc().next().unwrap();
    nfkc != c
}

// A: General_Category(cp) is in {Ll, Lu, Lo, Nd, Lm, Mn, Mc}
fn is_letter_digits(c: u32, gcs: &Vec<GeneralCategory>) -> bool {
    match gcs[c as usize] {
        GeneralCategory::LowercaseLetter
        | GeneralCategory::UppercaseLetter
        | GeneralCategory::ModifierLetter
        | GeneralCategory::OtherLetter
        | GeneralCategory::DecimalNumber
        | GeneralCategory::NonspacingMark
        | GeneralCategory::SpacingMark => true,
        _ => false
    }
}

// R: General_Category(cp) is in {Lt, Nl, No, Me}
fn is_other_letter_digits(c: u32, gcs: &Vec<GeneralCategory>) -> bool {
    match gcs[c as usize] {
        GeneralCategory::TitlecaseLetter
        | GeneralCategory::LetterNumber
        | GeneralCategory::OtherNumber
        | GeneralCategory::EnclosingMark => true,
        _ => false
    }
}

// N: General_Category(cp) is in {Zs}
fn is_spaces(c: u32, gcs: &Vec<GeneralCategory>) -> bool {
    gcs[c as usize] == GeneralCategory::SpaceSeparator
}

// O: General_Category(cp) is in {Sm, Sc, Sk, So}
fn is_symbols(c: u32, gcs: &Vec<GeneralCategory>) -> bool {
    match gcs[c as usize] {
        GeneralCategory::MathSymbol
        | GeneralCategory::CurrencySymbol
        | GeneralCategory::ModifierSymbol
        | GeneralCategory::OtherSymbol => true,
        _ => false
    }
}

/// P: General_Category(cp) is in {Pc, Pd, Ps, Pe, Pi, Pf, Po}
fn is_punctuation(c: u32, gcs: &Vec<GeneralCategory>) -> bool {
    match gcs[c as usize] {
        GeneralCategory::ConnectorPunctuation
        | GeneralCategory::DashPunctuation
        | GeneralCategory::OpenPunctuation
        | GeneralCategory::ClosePunctuation
        | GeneralCategory::InitialPunctuation
        | GeneralCategory::FinalPunctuation
        | GeneralCategory::OtherPunctuation => true,
        _ => false
    }
}

fn make_exceptions() -> HashMap<u32, PrecisResult> {
    let mut exceptions: HashMap<u32, PrecisResult> = HashMap::new();
    // PVALID -- Would otherwise have been DISALLOWED
    exceptions.insert(0x00df, PrecisResult::PValid); // LATIN SMALL LETTER SHARP S
    exceptions.insert(0x03c2, PrecisResult::PValid); // GREEK SMALL LETTER FINAL SIGMA
    exceptions.insert(0x06fd, PrecisResult::PValid); // ARABIC SIGN SINDHI AMPERSAND
    exceptions.insert(0x06fe, PrecisResult::PValid); // ARABIC SIGN SINDHI POSTPOSITION MEN
    exceptions.insert(0x0f0b, PrecisResult::PValid); // TIBETAN MARK INTERSYLLABIC TSHEG
    exceptions.insert(0x3007, PrecisResult::PValid); // IDEOGRAPHIC NUMBER ZERO

    // CONTEXTO -- Would otherwise have been DISALLOWED
    exceptions.insert(0x00b7, PrecisResult::ContextO); // MIDDLE DOT
    exceptions.insert(0x0375, PrecisResult::ContextO); // GREEK LOWER NUMERAL SIGN (KERAIA)
    exceptions.insert(0x05f3, PrecisResult::ContextO); // HEBREW PUNCTUATION GERESH
    exceptions.insert(0x05f4, PrecisResult::ContextO); // HEBREW PUNCTUATION GERSHAYIM
    exceptions.insert(0x30fb, PrecisResult::ContextO); // KATAKANA MIDDLE DOT

    // CONTEXTO -- Would otherwise have been PVALID
    exceptions.insert(0x0660, PrecisResult::ContextO); // ARABIC-INDIC DIGIT ZERO
    exceptions.insert(0x0661, PrecisResult::ContextO); // ARABIC-INDIC DIGIT ONE
    exceptions.insert(0x0662, PrecisResult::ContextO); // ARABIC-INDIC DIGIT TWO
    exceptions.insert(0x0663, PrecisResult::ContextO); // ARABIC-INDIC DIGIT THREE
    exceptions.insert(0x0664, PrecisResult::ContextO); // ARABIC-INDIC DIGIT FOUR
    exceptions.insert(0x0665, PrecisResult::ContextO); // ARABIC-INDIC DIGIT FIVE
    exceptions.insert(0x0666, PrecisResult::ContextO); // ARABIC-INDIC DIGIT SIX
    exceptions.insert(0x0667, PrecisResult::ContextO); // ARABIC-INDIC DIGIT SEVEN
    exceptions.insert(0x0668, PrecisResult::ContextO); // ARABIC-INDIC DIGIT EIGHT
    exceptions.insert(0x0669, PrecisResult::ContextO); // ARABIC-INDIC DIGIT NINE
    exceptions.insert(0x06f0, PrecisResult::ContextO); // EXTENDED ARABIC-INDIC DIGIT ZERO
    exceptions.insert(0x06f1, PrecisResult::ContextO); // EXTENDED ARABIC-INDIC DIGIT ONE
    exceptions.insert(0x06f2, PrecisResult::ContextO); // EXTENDED ARABIC-INDIC DIGIT TWO
    exceptions.insert(0x06f3, PrecisResult::ContextO); // EXTENDED ARABIC-INDIC DIGIT THREE
    exceptions.insert(0x06f4, PrecisResult::ContextO); // EXTENDED ARABIC-INDIC DIGIT FOUR
    exceptions.insert(0x06f5, PrecisResult::ContextO); // EXTENDED ARABIC-INDIC DIGIT FIVE
    exceptions.insert(0x06f6, PrecisResult::ContextO); // EXTENDED ARABIC-INDIC DIGIT SIX
    exceptions.insert(0x06f7, PrecisResult::ContextO); // EXTENDED ARABIC-INDIC DIGIT SEVEN
    exceptions.insert(0x06f8, PrecisResult::ContextO); // EXTENDED ARABIC-INDIC DIGIT EIGHT
    exceptions.insert(0x06f9, PrecisResult::ContextO); // EXTENDED ARABIC-INDIC DIGIT NINE

    // DISALLOWED -- Would otherwise have been PVALID
    exceptions.insert(0x0640, PrecisResult::Disallowed); // ARABIC TATWEEL
    exceptions.insert(0x07fa, PrecisResult::Disallowed); // NKO LAJANYALAN
    exceptions.insert(0x302e, PrecisResult::Disallowed); // HANGUL SINGLE DOT TONE MARK
    exceptions.insert(0x302f, PrecisResult::Disallowed); // HANGUL DOUBLE DOT TONE MARK
    exceptions.insert(0x3031, PrecisResult::Disallowed); // VERTICAL KANA REPEAT MARK
    exceptions.insert(0x3032, PrecisResult::Disallowed); // VERTICAL KANA REPEAT
                                                         // WITH VOICED SOUND MARK
    exceptions.insert(0x3033, PrecisResult::Disallowed); // VERTICAL KANA REPEAT MARK UPPER HALF
    exceptions.insert(0x3034, PrecisResult::Disallowed); // VERTICAL KANA REPEAT
                                                         // WITH VOICED SOUND MARK UPPER HA
    exceptions.insert(0x3035, PrecisResult::Disallowed); // VERTICAL KANA REPEAT MARK LOWER HALF
    exceptions.insert(0x303b, PrecisResult::Disallowed); // VERTICAL IDEOGRAPHIC ITERATION MARK

    exceptions
}

fn write_precis_table<W: Write>(out: &mut W, gcs: &Vec<GeneralCategory>, jcs: &HashSet<u32>,
                                noncharacters: &HashSet<u32>,
                                default_ignorable: &HashSet<u32>,
                                hangul_syllable_type: &HashMap<u32, HangulSyllableType>,
                                exceptions: &HashMap<u32, PrecisResult>,
                                back_compat: &HashMap<u32, PrecisResult>) {
    let (x, y, z) = unicode_normalization::UNICODE_VERSION;
    writeln!(out, "// Generated from Unicode {}.{}.{}{}", x, y, z, r#"
use std::cmp::Ordering::{Less, Equal, Greater};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PrecisResult {
    PValid,
    FreePValid,
    ContextJ,
    ContextO,
    Unassigned,
    Disallowed
}

pub fn precis_result(c: char) -> PrecisResult {
    let c = c as u32;
    match PRECIS_RESULTS.binary_search_by(|&(lo, hi, _)| {
        if hi < c { Less }
        else if lo <= c && c <= hi { Equal }
        else { Greater }
    }) {
        Ok(idx) => PRECIS_RESULTS[idx].2,
        Err(_) => unreachable!()
    }
}
"#).unwrap();

    writeln!(out, "pub static PRECIS_RESULTS: &'static [(u32, u32, PrecisResult)] = &[").unwrap();
    let mut prev = None;
    let mut i = 0u32;
    for cp in 0u32..(0x10ffff + 1) {
        let result = if let Some(&res) = exceptions.get(&cp) {
            res
        } else if let Some(&res) = back_compat.get(&cp) {
            res
        } else if is_unassigned(cp, &gcs, &noncharacters) {
            PrecisResult::Unassigned
        } else if is_ascii7(cp) {
            PrecisResult::PValid
        } else if is_join_control(cp, &jcs) {
            PrecisResult::ContextJ
        } else if is_old_hangul_jamo(cp, &hangul_syllable_type) {
            PrecisResult::Disallowed
        } else if is_precis_ignorable(cp, &default_ignorable, &noncharacters) {
            PrecisResult::Disallowed
        } else if is_control(cp, &gcs) {
            PrecisResult::Disallowed
        } else if is_compat(cp) {
            PrecisResult::FreePValid
        } else if is_letter_digits(cp, &gcs) {
            PrecisResult::PValid
        } else if is_other_letter_digits(cp, &gcs) {
            PrecisResult::FreePValid
        } else if is_spaces(cp, &gcs) {
            PrecisResult::FreePValid
        } else if is_symbols(cp, &gcs) {
            PrecisResult::FreePValid
        } else if is_punctuation(cp, &gcs) {
            PrecisResult::FreePValid
        } else {
            PrecisResult::Disallowed
        };

        if prev != Some(result) {
            if let Some(ty) = prev {
                println!("{:04X} {}", cp - 1, ty);
                write!(out, "0x{:04x}, PrecisResult::{:?}),", cp - 1, ty).unwrap();
                i += 1;
                if i % 2 == 1 { writeln!(out, "").unwrap(); }
            }
            if i == 0 || i % 2 == 1 { write!(out, "   ").unwrap(); }
            print!("{:04X}-", cp);
            write!(out, " (0x{:04x}, ", cp).unwrap();
        }
        prev = Some(result);
    }
    println!("{:04X} {}", 0x10ffff, prev.unwrap());
    writeln!(out, "0x{:04x}, PrecisResult::{:?})", 0x10ffff, prev.unwrap()).unwrap();
    writeln!(out, "];").unwrap();
}

fn write_script_table<W: Write>(out: &mut W, script: &Vec<(u32, u32, String)>) {
    let scripts: BTreeSet<&String> = script.iter().map(|&(_, _, ref x)| x).collect();

    writeln!(out, "{}", r#"
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Script {"#).unwrap();
    for s in scripts {
        writeln!(out, "    {},", s).unwrap();
    }
    writeln!(out, "    Unknown").unwrap();
    writeln!(out, "}}").unwrap();

    writeln!(out, "{}", r#"
pub fn script(c: char) -> Script {
    let c = c as u32;
    match SCRIPT.binary_search_by(|&(lo, hi, _)| {
        if hi < c { Less }
        else if c < lo { Greater }
        else { Equal }
    }) {
        Ok(idx) => SCRIPT[idx].2,
        Err(_) => Script::Unknown
    }
}
"#).unwrap();

    writeln!(out, "pub static SCRIPT: &'static [(u32, u32, Script)] = &[").unwrap();
    for &(lo, hi, ref s) in script {
        writeln!(out, "    (0x{:04x}, 0x{:04x}, Script::{}),", lo, hi, s).unwrap();
    }
    writeln!(out, "];").unwrap();
}

fn write_joining_type_table<W: Write>(out: &mut W, joining_type: &Vec<(u32, u32, JoiningType)>) {
    writeln!(out, "{}", r#"
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum JoiningType {
    RightJoining, // R
    LeftJoining,  // L
    DualJoining,  // D
    JoinCausing,  // C
    NonJoining,   // U
    Transparent   // T
}

pub fn joining_type(c: char) -> JoiningType {
    let c = c as u32;
    match JOINING_TYPE.binary_search_by(|&(lo, hi, _)| {
        if hi < c { Less }
        else if c < lo { Greater }
        else { Equal }
    }) {
        Ok(idx) => JOINING_TYPE[idx].2,
        Err(_) => JoiningType::NonJoining
    }
}
"#).unwrap();

    write!(out, "pub static JOINING_TYPE: &'static [(u32, u32, JoiningType)] = &[").unwrap();
    for (i, &(lo, hi, ref j)) in joining_type.iter().enumerate() {
        if i % 2 == 0 { write!(out, "\n   ").unwrap(); }
        write!(out, " (0x{:04x}, 0x{:04x}, JoiningType::{:?}),", lo, hi, j).unwrap();
    }
    writeln!(out, "\n];").unwrap();
}

fn main() {
    const SEPS: &'static [char] = &[';', '#'];

    let mut client = Client::new();

    let (x, y, z) = unicode_normalization::UNICODE_VERSION;
    let url = format!("http://www.unicode.org/Public/{}.{}.{}/ucd/UnicodeData.txt", x, y, z);
    let f = client.get(&url).send().unwrap();
    let f = BufReader::new(f);

    let mut gcs: Vec<GeneralCategory> = Vec::new();

    let mut prev = None;
    for line in f.lines() {
        let line = line.unwrap();
        let mut fields = line.split(';');
        let cp = fields.next().and_then(|x| u32::from_str_radix(x, 16).ok()).unwrap();
        let name = fields.next().unwrap();
        let gc: GeneralCategory = fields.next().unwrap().parse().unwrap();
        if name.ends_with("Last>") {
            let prev_cp = prev.unwrap();
            gcs.extend(iter::repeat(gc).take((cp - prev_cp) as usize));
        } else {
            let missing = cp as usize - gcs.len();
            gcs.extend(iter::repeat(GeneralCategory::Unassigned).take(missing));
            gcs.push(gc);
        }
        prev = Some(cp);
    }
    assert_eq!(gcs.len() - 1, 0x10fffd);
    // Add two final noncharacters
    gcs.push(GeneralCategory::Unassigned); // U+10FFFE
    gcs.push(GeneralCategory::Unassigned); // U+10FFFF

    fn parse_props_file<F>(file: &str, mut f: F) where F: FnMut(u32, Option<u32>, &str) {
        let mut client = Client::new();
        let (x, y, z) = unicode_normalization::UNICODE_VERSION;
        let url = format!("http://www.unicode.org/Public/{}.{}.{}/ucd/{}", x, y, z, file);
        let file = client.get(&url).send().unwrap();
        let file = BufReader::new(file);

        for line in file.lines() {
            let line = line.unwrap();
            if line.len() == 0 || line.starts_with('#') { continue; }
            let mut fields = line.split(SEPS);
            let cps = fields.next().unwrap().trim_right_matches(' ');
            let prop = fields.next().unwrap().trim_matches(' ');
            let mut cps = cps.split("..");
            let first = cps.next().and_then(|x| u32::from_str_radix(x, 16).ok()).unwrap();
            let last = cps.next().and_then(|x| u32::from_str_radix(x, 16).ok());
            f(first, last, prop);
        }
    }

    let mut noncharacters: HashSet<u32> = HashSet::new();
    let mut jcs: HashSet<u32> = HashSet::new();
    parse_props_file("PropList.txt", |first, last, prop| {
        match prop {
            "Noncharacter_Code_Point" => {
                if let Some(last) = last {
                    noncharacters.extend(first..(last + 1));
                } else {
                    noncharacters.insert(first);
                }
            }
            "Join_Control" => {
                if let Some(last) = last {
                    jcs.extend(first..(last + 1));
                } else {
                    jcs.insert(first);
                }
            }
            _ => ()
        }
    });

    let mut default_ignorable: HashSet<u32> = HashSet::new();
    parse_props_file("DerivedCoreProperties.txt", |first, last, prop| {
        if prop != "Default_Ignorable_Code_Point" { return; }
        if let Some(last) = last {
            default_ignorable.extend(first..(last + 1));
        } else {
            default_ignorable.insert(first);
        }
    });

    let mut hangul_syllable_type: HashMap<u32, HangulSyllableType> = HashMap::new();
    parse_props_file("HangulSyllableType.txt", |first, last, prop| {
        let ty = prop.parse().unwrap();
        if let Some(last) = last {
            hangul_syllable_type.extend((first..(last + 1)).zip(iter::repeat(ty)));
        } else {
            hangul_syllable_type.insert(first, ty);
        }
    });

    fn join_adjacent<T: PartialEq>(mut v: Vec<(u32, u32, T)>) -> Vec<(u32, u32, T)> {
        v.sort_by(|x, y| x.0.cmp(&y.0));
        v.into_iter().fold(Vec::new(), |mut acc, elem| {
            let mut same = false;
            if let Some(last) = acc.last_mut() {
                if last.2 == elem.2 && last.1 == elem.0 - 1 {
                    last.1 = elem.1;
                    same = true;
                }
            }
            if same { return acc; }
            acc.push(elem);
            acc
        })
    }

    let mut script: Vec<(u32, u32, String)> = Vec::new();
    parse_props_file("Scripts.txt", |first, last, prop| {
        let prop = prop.replace("_", "");
        script.push((first, last.unwrap_or(first), prop));
    });
    let script = join_adjacent(script);

    let mut joining_type: Vec<(u32, u32, JoiningType)> = Vec::new();
    parse_props_file("extracted/DerivedJoiningType.txt", |first, last, prop| {
        joining_type.push((first, last.unwrap_or(first), prop.parse().unwrap()));
    });
    let joining_type = join_adjacent(joining_type);

    let exceptions = make_exceptions();
    let back_compat: HashMap<u32, PrecisResult> = HashMap::new();

    let mut out = File::create("precis-table.rs").unwrap();
    write_precis_table(&mut out, &gcs, &jcs, &noncharacters, &default_ignorable,
                       &hangul_syllable_type, &exceptions, &back_compat);
    write_script_table(&mut out, &script);
    write_joining_type_table(&mut out, &joining_type);
}
