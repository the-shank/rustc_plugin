#![allow(dead_code)]
#![allow(clippy::disallowed_names)]

pub fn add(left: usize, right: usize) -> usize {
  left + right
}

pub struct Foo {
  i: i32,
  j: u32,
}

pub fn foo_fn(foo: Foo) -> i32 {
  foo.i
}

use std::mem;

struct Bar {
  vec: Vec<i32>,
}

fn bar_fn() {
  println!("size_of Bar is {}", mem::size_of::<Bar>());
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_works() {
    let result = add(2, 2);
    assert_eq!(result, 4);
  }
}
