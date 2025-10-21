import genome/ast.{type Attribute, type Import, Import, ImportType, ImportValue}
import glance
import gleam/list
import gleam/option.{Some}

pub fn attributes(import_: Import, attributes: List(Attribute)) -> Import {
  Import(..import_, attributes:)
}

pub fn module(import_: Import, module: String) -> Import {
  Import(..import_, module:)
}

pub fn alias(import_: Import, alias: String) -> Import {
  Import(..import_, alias: Some(alias))
}

pub fn unqualified_types(
  import_: Import,
  unqualified_types: List(glance.UnqualifiedImport),
) -> Import {
  let unqualified_types =
    unqualified_types
    |> list.map(fn(a) { ImportType(name: a.name, alias: a.alias) })

  Import(..import_, unqualified_types:)
}

pub fn unqualified_values(
  import_: Import,
  unqualified_values: List(glance.UnqualifiedImport),
) -> Import {
  let unqualified_values =
    unqualified_values
    |> list.map(fn(a) { ImportValue(name: a.name, alias: a.alias) })

  Import(..import_, unqualified_values:)
}
