import gleam/int
import gleam/list
import gleam/option.{None}

import genome/module.{type Module, Constant, List, Module, String}

fn fizzbuzz(n: Int) -> List(String) {
  use i <- list.map(list.range(1, n))

  case i % 3, i % 5 {
    0, 0 -> "FizzBuzz"
    0, _ -> "Fizz"
    _, 0 -> "Buzz"
    _, _ -> int.to_string(i)
  }
}

@generate(gen_fizzbuzz(128), "fizzbuzz_128")
@generate(gen_fizzbuzz(256), "fizzbuzz_256")
fn gen_fizzbuzz(n: Int) -> Module {
  let value = List(list.map(fizzbuzz(n), String), None)

  Module(..module.new(), constants: [Constant(True, "fizzbuzz", value)])
}
