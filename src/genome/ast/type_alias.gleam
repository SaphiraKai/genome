import glance.{type Type, type TypeAlias, Private, Public, TypeAlias}

pub fn name(type_alias: TypeAlias, name: String) -> TypeAlias {
  TypeAlias(..type_alias, name:)
}

pub fn make_public(type_alias: TypeAlias) -> TypeAlias {
  TypeAlias(..type_alias, publicity: Public)
}

pub fn make_private(type_alias: TypeAlias) -> TypeAlias {
  TypeAlias(..type_alias, publicity: Private)
}

pub fn parameters(type_alias: TypeAlias, parameters: List(String)) -> TypeAlias {
  TypeAlias(..type_alias, parameters:)
}

pub fn alias_to(type_alias: TypeAlias, aliased: Type) -> TypeAlias {
  TypeAlias(..type_alias, aliased:)
}
