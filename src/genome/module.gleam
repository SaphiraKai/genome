import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/string

import glance

pub type Module {
  Module(
    imports: List(Import),
    constants: List(Constant),
    custom_types: List(CustomType),
    functions: List(Function),
  )
}

pub fn new() -> Module {
  Module([], [], [], [])
}

pub fn from_glance(glance_module: glance.Module) -> Module {
  let imports = glance_module.imports |> list.map(import_from_glance)
  let constants = glance_module.constants |> list.map(constant_from_glance)
  let custom_types =
    glance_module.custom_types |> list.map(custom_type_from_glance)
  let functions = glance_module.functions |> list.map(function_from_glance)

  Module(imports:, constants:, custom_types:, functions:)
}

pub fn append(this module: Module, with other: Module) -> Module {
  Module(
    imports: list.append(module.imports, other.imports),
    constants: list.append(module.constants, other.constants),
    custom_types: list.append(module.custom_types, other.custom_types),
    functions: list.append(module.functions, other.functions),
  )
}

pub fn to_string(this module: Module) -> String {
  let imports =
    module.imports |> list.map(import_to_string) |> string.join("\n")
  let constants =
    module.constants |> list.map(constant_to_string) |> string.join("\n")
  let custom_types =
    module.custom_types |> list.map(custom_type_to_string) |> string.join("\n")
  let functions =
    module.functions |> list.map(function_to_string) |> string.join("\n")

  imports <> "\n" <> constants <> "\n" <> custom_types <> "\n" <> functions
}

pub type Attribute {
  External(target: String, path: String, value: String)
  Generate(expression: Expression)
}

fn attribute_from_glance(glance_attribute: glance.Attribute) -> Attribute {
  case glance_attribute {
    glance.Attribute("generate", [argument]) ->
      Generate(expression_from_glance(argument))
    glance.Attribute(
      "external",
      [glance.Variable(target), glance.String(path), glance.String(value)],
    ) -> External(target, path, value)
    glance.Attribute(..) -> {
      let msg = "invalid attribute: " <> string.inspect(glance_attribute)
      panic as msg
    }
  }
}

fn attribute_to_string(this attribute: Attribute) {
  case attribute {
    External(target, path, value) ->
      "@external(" <> target <> ", \"" <> path <> "\", \"" <> value <> "\")"
    Generate(expression) ->
      "@generate(" <> expression_to_string(expression) <> ")"
  }
}

pub type ImportItem {
  Type(name: String, alias: Option(String))
  Value(name: String, alias: Option(String))
}

fn import_item_to_string(this item: ImportItem) -> String {
  case item {
    Type(..) -> "type " <> item.name
    Value(..) -> item.name
  }
  <> case item.alias {
    None -> ""
    Some(alias) -> " as " <> alias
  }
}

pub type Import {
  Import(
    attributes: List(Attribute),
    module: String,
    unqualified: List(ImportItem),
    alias: Option(String),
  )
}

fn import_from_glance(glance_import: glance.Definition(glance.Import)) -> Import {
  let unqualified_from_glance = fn(import_, constructor) {
    let glance.UnqualifiedImport(name:, alias:) = import_
    constructor(name, alias)
  }

  let attributes = glance_import.attributes |> list.map(attribute_from_glance)
  let unqualified =
    [
      glance_import.definition.unqualified_types
        |> list.map(unqualified_from_glance(_, Type)),
      glance_import.definition.unqualified_values
        |> list.map(unqualified_from_glance(_, Value)),
    ]
    |> list.flatten
  let alias = case glance_import.definition.alias {
    Some(glance.Named(s)) -> Some(s)
    Some(glance.Discarded(s)) -> Some("_" <> s)
    _ -> None
  }

  Import(
    attributes:,
    module: glance_import.definition.module,
    unqualified:,
    alias:,
  )
}

fn import_to_string(this import_: Import) -> String {
  let attributes =
    import_.attributes |> list.map(attribute_to_string) |> string.join("\n")
  let import_module = "import " <> import_.module
  let unqualified = case import_.unqualified {
    [] -> ""
    items -> {
      let items =
        list.sort(items, fn(a: ImportItem, b: ImportItem) {
          case a, b {
            Type(..), Value(..) -> Gt
            Value(..), Type(..) -> Lt
            Type(..), Type(..) | Value(..), Value(..) ->
              string.compare(a.name, b.name)
          }
        })
        |> list.map(import_item_to_string)

      ".{" <> string.join(items, ", ") <> "}"
    }
  }

  attributes
  <> "\n"
  <> import_module
  <> unqualified
  <> case import_.alias {
    None -> ""
    Some(alias) -> "as " <> alias
  }
}

pub type BinaryOperator {
  And
  Or
  Eq
  NotEq
  LtInt
  LtEqInt
  LtFloat
  LtEqFloat
  GtEqInt
  GtInt
  GtEqFloat
  GtFloat
  Pipe
  AddInt
  AddFloat
  SubInt
  SubFloat
  MultInt
  MultFloat
  DivInt
  DivFloat
  RemainderInt
  Concatenate
}

fn binary_operator_to_string(this binary_operator: BinaryOperator) -> String {
  case binary_operator {
    AddFloat -> "+."
    AddInt -> "+"
    And -> "&&"
    Concatenate -> "<>"
    DivFloat -> "/."
    DivInt -> "/"
    Eq -> "=="
    GtEqFloat -> ">=."
    GtEqInt -> ">="
    GtFloat -> ">."
    GtInt -> ">"
    LtEqFloat -> "<=."
    LtEqInt -> "<="
    LtFloat -> "<."
    LtInt -> "<"
    MultFloat -> "*."
    MultInt -> "*"
    NotEq -> "!="
    Or -> "||"
    Pipe -> "|>"
    RemainderInt -> "%"
    SubFloat -> "-."
    SubInt -> "-"
  }
}

fn binary_operator_from_glance(
  glance_binary_operator: glance.BinaryOperator,
) -> BinaryOperator {
  case glance_binary_operator {
    glance.AddFloat -> AddFloat
    glance.AddInt -> AddInt
    glance.And -> And
    glance.Concatenate -> Concatenate
    glance.DivFloat -> DivFloat
    glance.DivInt -> DivInt
    glance.Eq -> Eq
    glance.GtEqFloat -> GtEqFloat
    glance.GtEqInt -> GtEqInt
    glance.GtFloat -> GtFloat
    glance.GtInt -> GtInt
    glance.LtEqFloat -> LtEqFloat
    glance.LtEqInt -> LtEqInt
    glance.LtFloat -> LtFloat
    glance.LtInt -> LtInt
    glance.MultFloat -> MultFloat
    glance.MultInt -> MultInt
    glance.NotEq -> NotEq
    glance.Or -> Or
    glance.Pipe -> Pipe
    glance.RemainderInt -> RemainderInt
    glance.SubFloat -> SubFloat
    glance.SubInt -> SubInt
  }
}

pub type Expression {
  List(elements: List(Expression), rest: Option(Expression))
  String(String)
  Int(Int)
  Float(Float)
  Bool(Bool)
  Variable(module: Option(String), name: String)
  Call(function: Expression, arguments: List(#(String, Expression)))
  FieldAccess(container: Expression, field: String)
  RecordUpdate(
    module: Option(String),
    constructor: String,
    record: Expression,
    fields: List(#(String, Option(Expression))),
  )
  BinaryOperator(name: BinaryOperator, left: Expression, right: Expression)
  Fn(
    parameters: List(#(String, Option(Type))),
    return: Option(Type),
    body: List(Statement),
  )
  Tuple(List(Expression))
}

fn expression_from_glance(glance_expression: glance.Expression) -> Expression {
  let field_from_glance = fn(a) {
    case a {
      glance.LabelledField(l, e) -> #(l, expression_from_glance(e))
      glance.ShorthandField(l) -> #(l, Variable(None, l))
      glance.UnlabelledField(e) -> #("", expression_from_glance(e))
    }
  }

  case glance_expression {
    glance.BinaryOperator(name, left, right) ->
      BinaryOperator(
        binary_operator_from_glance(name),
        expression_from_glance(left),
        expression_from_glance(right),
      )
    glance.BitString(_) -> todo
    glance.Block(_) -> todo
    glance.Call(e, f) ->
      Call(expression_from_glance(e), list.map(f, field_from_glance))
    glance.Case(_, _) -> todo
    glance.FieldAccess(e, f) -> FieldAccess(expression_from_glance(e), f)
    glance.Float(f) -> {
      let assert Ok(f) = float.parse(f)
      Float(f)
    }
    glance.Fn(parameters, return, body) ->
      Fn(
        list.map(parameters, fn(a) {
          #(
            case a.name {
              glance.Discarded(s) -> "_" <> s
              glance.Named(s) -> s
            },
            option.map(a.type_, type_from_glance),
          )
        }),
        option.map(return, type_from_glance),
        list.map(body, statement_from_glance),
      )
    glance.FnCapture(_, _, _, _) -> todo
    glance.Int(i) -> {
      let assert Ok(i) = int.parse(i)
      Int(i)
    }
    glance.List(e, r) ->
      List(
        list.map(e, expression_from_glance),
        option.map(r, expression_from_glance),
      )
    glance.NegateBool(_) -> todo
    glance.NegateInt(_) -> todo
    glance.Panic(_) -> todo
    glance.RecordUpdate(module, constructor, record, fields) ->
      RecordUpdate(
        module:,
        constructor:,
        record: expression_from_glance(record),
        fields: list.map(fields, fn(a) {
          let glance.RecordUpdateField(label, item) = a
          #(label, option.map(item, expression_from_glance))
        }),
      )
    glance.String(s) -> String(s)
    glance.Todo(_) -> todo
    glance.Tuple(e) -> Tuple(list.map(e, expression_from_glance))
    glance.TupleIndex(_, _) -> todo
    glance.Variable(v) -> Variable(None, v)
  }
}

fn expression_to_string(this expression: Expression) -> String {
  case expression {
    Bool(v) -> bool.to_string(v)
    Call(e, a) ->
      expression_to_string(e)
      <> "("
      <> list.map(a, fn(a) {
        case a.0 {
          "" -> ""
          l -> l <> ": "
        }
        <> expression_to_string(a.1)
      })
      |> string.join(", ")
      <> ")"
    Float(v) -> float.to_string(v)
    Int(v) -> int.to_string(v)
    String(v) -> "\"" <> v <> "\""
    Variable(m, n) ->
      case m {
        None -> ""
        Some(m) -> m <> "."
      }
      <> n
    List(elements, rest) -> {
      let rest = case rest {
        None -> ""
        Some(e) -> ".." <> expression_to_string(e)
      }

      "["
      <> list.map(elements, expression_to_string) |> string.join(", ")
      <> rest
      <> "]"
    }
    FieldAccess(container, field) ->
      expression_to_string(container) <> "." <> field
    RecordUpdate(module, constructor, record, fields) ->
      case module {
        None -> ""
        Some(m) -> m <> "."
      }
      <> constructor
      <> "(.."
      <> expression_to_string(record)
      <> ", "
      <> list.map(fields, fn(a) {
        a.0
        <> ":"
        <> case a.1 {
          None -> ""
          Some(e) -> expression_to_string(e)
        }
      })
      |> string.join(", ")
      <> ")"
    BinaryOperator(name, left, right) ->
      expression_to_string(left)
      <> " "
      <> binary_operator_to_string(name)
      <> " "
      <> expression_to_string(right)
    Fn(parameters, return, body) ->
      "fn("
      <> list.map(parameters, fn(a) {
        a.0
        <> case a.1 {
          None -> ""
          Some(t) -> ": " <> type_to_string(t)
        }
      })
      |> string.join(", ")
      <> ") "
      <> case return {
        None -> ""
        Some(t) -> "-> " <> type_to_string(t)
      }
      <> " {\n"
      <> list.map(body, fn(a) { "  " <> statement_to_string(a) })
      |> string.join("\n")
      <> "\n}"
    Tuple(e) ->
      "#(" <> list.map(e, expression_to_string) |> string.join(", ") <> ")"
  }
}

pub type Constant {
  Constant(public: Bool, name: String, value: Expression)
}

fn constant_from_glance(
  glance_constant: glance.Definition(glance.Constant),
) -> Constant {
  let public = case glance_constant.definition.publicity {
    glance.Private -> False
    glance.Public -> True
  }

  Constant(
    public:,
    name: glance_constant.definition.name,
    value: expression_from_glance(glance_constant.definition.value),
  )
}

fn constant_to_string(this constant: Constant) -> String {
  "const "
  <> case constant.public {
    False -> ""
    True -> "pub "
  }
  <> constant.name
  <> " = "
  <> expression_to_string(constant.value)
}

pub type Type {
  Named(name: String, module: Option(String), parameters: List(Type))
}

fn type_from_glance(glance_type: glance.Type) -> Type {
  case glance_type {
    glance.FunctionType(_, _) -> todo
    glance.HoleType(_) -> todo
    glance.NamedType(name, module, parameters) ->
      Named(name, module, list.map(parameters, type_from_glance))
    glance.TupleType(_) -> todo
    glance.VariableType(_) -> todo
  }
}

pub fn type_to_string(this type_: Type) -> String {
  case type_ {
    Named(name, module, parameters) -> {
      let module = case module {
        None -> ""
        Some(m) -> m <> "."
      }
      let parameters = case parameters {
        [] -> ""
        _ ->
          "("
          <> list.map(parameters, type_to_string) |> string.join(", ")
          <> ")"
      }
      module <> name <> parameters
    }
  }
}

pub type Variant {
  Variant(name: String, fields: List(#(String, Option(Type))))
}

fn variant_from_glance(glance_variant: glance.Variant) -> Variant {
  let fields =
    glance_variant.fields
    |> list.map(fn(a) {
      case a {
        glance.LabelledVariantField(t, l) -> #(l, Some(type_from_glance(t)))
        glance.UnlabelledVariantField(t) -> #("", Some(type_from_glance(t)))
      }
    })

  Variant(name: glance_variant.name, fields:)
}

fn variant_to_string(this variant: Variant) -> String {
  let field_to_string = fn(a: #(String, Option(Type))) {
    case a.0, a.1 {
      "", None -> ""
      "", Some(t) -> type_to_string(t)
      l, None -> l
      l, Some(t) -> l <> ": " <> type_to_string(t)
    }
  }

  variant.name
  <> case variant.fields {
    [] -> ""
    f ->
      "("
      <> list.map(f, field_to_string)
      |> string.join(", ")
      <> ")"
  }
}

pub type CustomType {
  CustomType(
    public: Bool,
    opaque_: Bool,
    name: String,
    parameters: List(String),
    variants: List(Variant),
  )
}

fn custom_type_from_glance(
  glance_custom_type: glance.Definition(glance.CustomType),
) -> CustomType {
  let public = case glance_custom_type.definition.publicity {
    glance.Private -> False
    glance.Public -> True
  }
  let variants =
    glance_custom_type.definition.variants |> list.map(variant_from_glance)

  CustomType(
    public:,
    opaque_: glance_custom_type.definition.opaque_,
    name: glance_custom_type.definition.name,
    parameters: glance_custom_type.definition.parameters,
    variants:,
  )
}

fn custom_type_to_string(this custom_type: CustomType) {
  let publicity = case custom_type.public {
    False -> ""
    True -> "pub "
  }
  let opacity = case custom_type.opaque_ {
    False -> ""
    True -> "opaque "
  }
  let variants = case custom_type.variants {
    [] -> ""
    v ->
      "{\n"
      <> list.map(v, fn(a) { "  " <> variant_to_string(a) })
      |> string.join("\n")
      <> "\n}"
  }

  publicity
  <> opacity
  <> "type "
  <> custom_type.name
  <> case custom_type.parameters {
    [] -> " "
    p -> "(" <> string.join(p, ", ") <> ") "
  }
  <> variants
}

pub type AssignmentKind {
  Let
  Assert
}

pub type Pattern {
  PatternVariable(name: String)
}

fn pattern_to_string(this pattern: Pattern) -> String {
  case pattern {
    PatternVariable(name) -> name
  }
}

pub type Statement {
  Assignment(kind: AssignmentKind, pattern: Pattern, value: Expression)
  Expression(Expression)
}

fn statement_from_glance(glance_statement: glance.Statement) -> Statement {
  case glance_statement {
    glance.Assignment(kind, pattern, _, value) ->
      Assignment(
        case kind {
          glance.Assert -> Assert
          glance.Let -> Let
        },
        case pattern {
          glance.PatternVariable(name) -> PatternVariable(name)
          _ -> todo
        },
        expression_from_glance(value),
      )
    glance.Expression(e) -> Expression(expression_from_glance(e))
    glance.Use(_, _) -> todo
  }
}

fn statement_to_string(this statement: Statement) -> String {
  case statement {
    Expression(e) -> expression_to_string(e)
    Assignment(kind, pattern, value) ->
      assignment_to_string(kind, pattern, value)
  }
}

fn assignment_to_string(
  kind kind: AssignmentKind,
  pattern pattern: Pattern,
  value value: Expression,
) -> String {
  case kind {
    Assert -> "let assert "
    Let -> "let "
  }
  <> pattern_to_string(pattern)
  <> " = "
  <> expression_to_string(value)
}

pub type Function {
  Function(
    public: Bool,
    name: String,
    parameters: List(#(String, String, Option(Type))),
    return: Option(Type),
    body: List(Statement),
  )
}

fn function_from_glance(
  glance_function: glance.Definition(glance.Function),
) -> Function {
  let public = case glance_function.definition.publicity {
    glance.Private -> False
    glance.Public -> True
  }
  let parameters =
    glance_function.definition.parameters
    |> list.map(fn(a) {
      let label = option.unwrap(a.label, "")
      let name = case a.name {
        glance.Discarded(s) -> "_" <> s
        glance.Named(s) -> s
      }
      let type_ = option.map(a.type_, type_from_glance)

      #(label, name, type_)
    })

  Function(
    public:,
    name: glance_function.definition.name,
    parameters:,
    return: glance_function.definition.return |> option.map(type_from_glance),
    body: glance_function.definition.body |> list.map(statement_from_glance),
  )
}

fn function_to_string(this function: Function) {
  let params_to_string = fn(p: #(String, String, Option(Type))) {
    case p.0 {
      "" -> ""
      s -> s <> " "
    }
    <> p.1
    <> case p.2 {
      None -> ""
      Some(t) -> ": " <> type_to_string(t)
    }
  }

  let publicity = case function.public {
    False -> ""
    True -> "pub "
  }
  let return = case function.return {
    None -> ""
    Some(r) -> " -> " <> type_to_string(r)
  }
  let parameters =
    list.map(function.parameters, params_to_string)
    |> string.join(", ")
  let body =
    function.body
    |> list.map(fn(a) { "  " <> statement_to_string(a) })
    |> string.join("\n")

  publicity
  <> "fn "
  <> function.name
  <> "("
  <> parameters
  <> ")"
  <> return
  <> " {\n"
  <> body
  <> "\n}"
}
