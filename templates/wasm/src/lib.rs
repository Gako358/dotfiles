use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn gcd(a: i64, b: i64) -> u64 {
    if b == 0 {
        a.unsigned_abs()
    } else {
        gcd(b, a % b)
    }
}
