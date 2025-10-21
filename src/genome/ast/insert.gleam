import genome/ast.{
  type Constant, type CustomType, type Function, type Import, type Module,
  type TypeAlias, Module,
}

pub fn import_(in module: Module, insert import_: Import) -> Module {
  let Module(imports:, ..) = module

  Module(..module, imports: [import_, ..imports])
}

pub fn custom_type(in module: Module, insert custom_type: CustomType) -> Module {
  let Module(custom_types:, ..) = module

  Module(..module, custom_types: [custom_type, ..custom_types])
}

pub fn type_alias(in module: Module, insert type_alias: TypeAlias) -> Module {
  let Module(type_aliases:, ..) = module

  Module(..module, type_aliases: [type_alias, ..type_aliases])
}

pub fn constant(in module: Module, insert constant: Constant) -> Module {
  let Module(constants:, ..) = module

  Module(..module, constants: [constant, ..constants])
}

pub fn function(in module: Module, insert function: Function) -> Module {
  let Module(functions:, ..) = module

  Module(..module, functions: [function, ..functions])
}
