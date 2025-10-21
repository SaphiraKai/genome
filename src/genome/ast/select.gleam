import gleam/list

import genome/ast.{
  type Constant, type CustomType, type Edit, type Function, type Import,
  type Module, type TypeAlias, Edit,
}
import genome/ast/insert
import genome/ast/remove

pub fn select_import(
  with fun: fn(Import) -> Bool,
) -> fn(Module) -> #(List(Import), Edit(Import)) {
  let #(remove, insert) = #(remove.import_, insert.import_)

  fn(m: Module) {
    let values = m.imports |> list.filter(keeping: fun)

    #(values, Edit(remove:, insert:))
  }
}

pub fn select_custom_types(
  with fun: fn(CustomType) -> Bool,
) -> fn(Module) -> #(List(CustomType), Edit(CustomType)) {
  let #(remove, insert) = #(remove.custom_type, insert.custom_type)

  fn(m: Module) {
    let values = m.custom_types |> list.filter(keeping: fun)

    #(values, Edit(remove:, insert:))
  }
}

pub fn select_type_alias(
  with fun: fn(TypeAlias) -> Bool,
) -> fn(Module) -> #(List(TypeAlias), Edit(TypeAlias)) {
  let #(remove, insert) = #(remove.type_alias, insert.type_alias)

  fn(m: Module) {
    let values = m.type_aliases |> list.filter(keeping: fun)

    #(values, Edit(remove:, insert:))
  }
}

pub fn select_constant(
  with fun: fn(Constant) -> Bool,
) -> fn(Module) -> #(List(Constant), Edit(Constant)) {
  let #(remove, insert) = #(remove.constant, insert.constant)

  fn(m: Module) {
    let values = m.constants |> list.filter(keeping: fun)

    #(values, Edit(remove:, insert:))
  }
}

pub fn select_function(
  with fun: fn(Function) -> Bool,
) -> fn(Module) -> #(List(Function), Edit(Function)) {
  let #(remove, insert) = #(remove.function, insert.function)

  fn(m: Module) {
    let values = m.functions |> list.filter(keeping: fun)

    #(values, Edit(remove:, insert:))
  }
}
