// Check wheter U+00B7 MIDDLE DOT is allowed in this position
// Between 'l' (U+006C) characters only,
// used to permit the Catalan character ela geminada to be expressed.
fn contexto_middle_dot(s: &str, off: usize) -> bool {
    const ELA_GEMIADA: [char; 3] = ['l', '\u{b7}', 'l'];
    let sub = &s[(off-1)..(off+3)];
    sub.chars().zip(ELA_GEMIADA.iter()).all(|(x, y)| x == *y)
}
