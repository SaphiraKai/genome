import glance.{
  type Constant, type Expression, type Type, Constant, Private, Public,
}
import gleam/option.{type Option}

pub fn name(constant: Constant, name: String) -> Constant {
  Constant(..constant, name:)
}

pub fn make_public(constant: Constant) -> Constant {
  Constant(..constant, publicity: Public)
}

pub fn make_private(constant: Constant) -> Constant {
  Constant(..constant, publicity: Private)
}

pub fn annotation(constant: Constant, annotation: Option(Type)) -> Constant {
  Constant(..constant, annotation:)
}

pub fn value(constant: Constant, value: Expression) -> Constant {
  Constant(..constant, value:)
}
