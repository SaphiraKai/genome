import glance.{
  type Function, type FunctionParameter, type Statement, type Type, Function,
  Private, Public,
}
import gleam/option.{type Option}

pub fn name(function: Function, name: String) -> Function {
  Function(..function, name:)
}

pub fn make_public(function: Function) -> Function {
  Function(..function, publicity: Public)
}

pub fn make_private(function: Function) -> Function {
  Function(..function, publicity: Private)
}

pub fn parameters(
  function: Function,
  parameters: List(FunctionParameter),
) -> Function {
  Function(..function, parameters:)
}

pub fn return(function: Function, return: Option(Type)) -> Function {
  Function(..function, return:)
}

pub fn body(function: Function, body: List(Statement)) -> Function {
  Function(..function, body:)
}
