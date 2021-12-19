use wasm_bindgen::prelude::*;

use solutions::{Day, Solution};

#[wasm_bindgen]
pub struct WebSolution(Solution);

#[wasm_bindgen]
impl WebSolution {
    #[wasm_bindgen(getter)]
    pub fn part1(&self) -> String {
        self.0.part1.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn part2(&self) -> String {
        self.0.part1.clone()
    }
}

#[wasm_bindgen]
pub fn run_day(day: Day, input: String) -> WebSolution {
    WebSolution(solutions::run_day(day, input))
}
