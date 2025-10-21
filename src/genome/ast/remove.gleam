import genome/ast.{
  type Constant, type CustomType, type Function, type Import, type Module,
  type TypeAlias, Module,
}
import gleam/list

pub fn import_(in module: Module, remove import_: Import) -> Module {
  let Module(imports:, ..) = module
  let #(head, tail) = imports |> list.split_while(fn(i) { i == import_ })
  let imports = list.append(head, list.drop(tail, 1))

  Module(..module, imports:)
}

pub fn custom_type(in module: Module, remove custom_type: CustomType) -> Module {
  let Module(custom_types:, ..) = module

  let #(head, tail) =
    custom_types |> list.split_while(fn(t) { t != custom_type })

  let custom_types =
    list.append(head, case list.rest(tail) {
      Ok(rest) -> rest
      Error(_) -> []
    })

  Module(..module, custom_types:)
}

pub fn type_alias(in module: Module, remove type_alias: TypeAlias) -> Module {
  let Module(type_aliases:, ..) = module
  let #(head, tail) =
    type_aliases |> list.split_while(fn(i) { i == type_alias })
  let type_aliases = list.append(head, list.drop(tail, 1))

  Module(..module, type_aliases:)
}

pub fn constant(in module: Module, remove constant: Constant) -> Module {
  let Module(constants:, ..) = module
  let #(head, tail) = constants |> list.split_while(fn(i) { i == constant })
  let constants = list.append(head, list.drop(tail, 1))

  Module(..module, constants:)
}

pub fn function(in module: Module, remove function: Function) -> Module {
  let Module(functions:, ..) = module
  let #(head, tail) = functions |> list.split_while(fn(i) { i == function })
  let functions = list.append(head, list.drop(tail, 1))

  Module(..module, functions:)
}
