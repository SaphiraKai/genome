import genome/ast.{
  type AssignmentKind, type Attribute, type BinaryOperator, type Clause,
  type Constant, type CustomType, type Expression, type Field, type Function,
  type FunctionParameter, type Import, type ImportType, type ImportValue,
  type Module, type Pattern, type Statement, type Type, type TypeAlias,
  type UsePattern, type Variant, type VariantField, AddFloat, AddInt, And,
  Assert, Assignment, AssignmentPattern, BinaryOperator, BitString, Block, Bool,
  Call, Case, Clear, Concatenate, ConcatenatePattern, Discarded, DivFloat,
  DivInt, Echo, Eq, Expression, External, FieldAccess, Float, FloatPattern, Fn,
  FnCapture, FunctionType, Genome, GtEqFloat, GtEqInt, GtFloat, GtInt, HoleType,
  Int, IntPattern, LabelledField, LabelledVariantField, Let, LetAssert, List,
  ListPattern, LtEqFloat, LtEqInt, LtFloat, LtInt, MultFloat, MultInt, Named,
  NamedType, NegateBool, NegateInt, NotEq, Opaque, Or, Panic, PatternVariant,
  Pipe, Private, Public, RecordUpdate, RemainderFloat, RemainderInt,
  ShorthandField, String, StringPattern, SubFloat, SubInt, Todo, Tuple,
  TupleIndex, TuplePattern, TupleType, UnlabelledField, UnlabelledVariantField,
  Use, Variable, VariablePattern, VariableType,
}
import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

pub fn module(this module: Module) -> String {
  let imports = module.imports |> list.map(import_) |> string.join("\n\n")
  let constants = module.constants |> list.map(constant) |> string.join("\n\n")
  let custom_types =
    module.custom_types |> list.map(custom_type) |> string.join("\n\n")
  let functions = module.functions |> list.map(function) |> string.join("\n\n")

  [imports, constants, custom_types, functions]
  |> list.map(string.to_option)
  |> option.values
  |> string.join("\n\n")
  <> "\n"
}

pub fn attribute(this attribute: Attribute) {
  case attribute {
    External(target:, path:, value:) ->
      "@external(" <> target <> ", \"" <> path <> "\", \"" <> value <> "\")"
    Genome(kind:, arguments:) ->
      "@genome("
      <> kind
      <> ", "
      <> string.join(list.map(arguments, expression), ", ")
      <> ")"
  }
}

pub fn import_type(this item: ImportType) -> String {
  "type "
  <> item.name
  <> case item.alias {
    None -> ""
    Some(alias) -> " as " <> alias
  }
}

pub fn import_value(this item: ImportValue) -> String {
  item.name
  <> case item.alias {
    None -> ""
    Some(alias) -> " as " <> alias
  }
}

pub fn import_(this import_: Import) -> String {
  let attributes =
    import_.attributes |> list.map(attribute) |> string.join("\n")
  let import_module = "import " <> import_.module
  let types =
    import_.unqualified_types
    |> list.sort(fn(a, b) { string.compare(a.name, b.name) })
    |> list.map(import_type)
  let values =
    import_.unqualified_values
    |> list.sort(fn(a, b) { string.compare(a.name, b.name) })
    |> list.map(import_value)

  let unqualified = case types, values {
    [], [] -> ""
    _, _ -> ".{" <> string.join(types |> list.append(values), ", ") <> "}"
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

pub fn binary_operator(this binary_operator: BinaryOperator) -> String {
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
    RemainderFloat -> "%."
    SubFloat -> "-."
    SubInt -> "-"
  }
}

pub fn clause(this clause: Clause) -> String {
  list.map(clause.patterns, list.map(_, pattern))
  |> list.map(string.join(_, ", "))
  |> string.join(" | ")
  <> case clause.guard {
    None -> ""
    Some(e) -> " if " <> expression(e)
  }
  <> " -> "
  <> expression(clause.body)
}

pub fn field(this field: Field(t), with inner: fn(t) -> String) -> String {
  case field {
    LabelledField(label, item) -> label <> ": " <> inner(item)
    UnlabelledField(item) -> inner(item)
    ShorthandField(label) -> label <> ":"
  }
}

pub fn expression(this expr: Expression) -> String {
  case expr {
    Bool(v) -> bool.to_string(v)
    Call(e, a) ->
      expression(e)
      <> "("
      <> list.map(a, field(_, expression))
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
        Some(e) -> ".." <> expression(e)
      }

      "[" <> list.map(elements, expression) |> string.join(", ") <> rest <> "]"
    }
    FieldAccess(container, field) -> expression(container) <> "." <> field
    RecordUpdate(module, constructor, record, fields) ->
      case module {
        None -> ""
        Some(m) -> m <> "."
      }
      <> constructor
      <> "(.."
      <> expression(record)
      <> ", "
      <> list.map(fields, fn(a) {
        a.0
        <> ":"
        <> case a.1 {
          None -> ""
          Some(e) -> expression(e)
        }
      })
      |> string.join(", ")
      <> ")"
    BinaryOperator(name, left, right) ->
      expression(left)
      <> " "
      <> binary_operator(name)
      <> " "
      <> expression(right)
    Fn(parameters, return, body) ->
      "fn("
      <> list.map(parameters, fn(a) {
        a.0
        <> case a.1 {
          None -> ""
          Some(t) -> ": " <> type_(t)
        }
      })
      |> string.join(", ")
      <> ") "
      <> case return {
        None -> ""
        Some(t) -> "-> " <> type_(t)
      }
      <> " {\n"
      <> list.map(body, fn(a) { "  " <> statement(a) })
      |> string.join("\n")
      <> "\n}"
    Tuple(e) -> "#(" <> list.map(e, expression) |> string.join(", ") <> ")"
    Case(subjects, clauses) ->
      "case "
      <> list.map(subjects, expression) |> string.join(", ")
      <> " {\n"
      <> list.map(clauses, fn(a) { "  " <> clause(a) })
      |> string.join("\n")
      <> "\n}"
    BitString(_) -> todo
    Block(body) ->
      "{\n"
      <> list.map(body, fn(a) { "  " <> statement(a) })
      |> string.join("\n")
      <> "\n}"
    Echo(expr) ->
      "echo"
      <> case expr {
        None -> ""
        Some(expr) -> " " <> expression(expr)
      }
    FnCapture(module, function, before, after) ->
      case module {
        None -> ""
        Some(module) -> module <> "."
      }
      <> expression(function)
      <> "("
      <> before
      |> list.map(field(_, expression))
      |> list.append(["_", ..list.map(after, field(_, expression))])
      |> string.join(", ")
      <> ")"
    NegateBool(value) -> "!" <> expression(value)
    NegateInt(value) -> "-" <> expression(value)
    Panic(message) ->
      "panic"
      <> case message {
        None -> ""
        Some(message) -> " as " <> expression(message)
      }
    Todo(message) ->
      "todo"
      <> case message {
        None -> ""
        Some(message) -> " as " <> expression(message)
      }
    TupleIndex(tuple, index) -> expression(tuple) <> "." <> int.to_string(index)
  }
}

pub fn use_pattern(use_pattern: UsePattern) -> String {
  pattern(use_pattern.pattern)
  <> case use_pattern.annotation {
    None -> ""
    Some(ty) -> " -> " <> type_(ty)
  }
}

pub fn constant(this constant: Constant) -> String {
  case constant.publicity {
    Private -> ""
    Public -> "pub "
  }
  <> "const "
  <> constant.name
  <> " = "
  <> expression(constant.value)
}

pub fn type_(this ty: Type) -> String {
  case ty {
    NamedType(name, module, parameters) -> {
      let module = case module {
        None -> ""
        Some(m) -> m <> "."
      }
      let parameters = case parameters {
        [] -> ""
        _ -> "(" <> list.map(parameters, type_) |> string.join(", ") <> ")"
      }
      module <> name <> parameters
    }
    FunctionType(parameters, return) ->
      "fn("
      <> list.map(parameters, type_) |> string.join(", ")
      <> ") -> "
      <> type_(return)
    HoleType(name) -> "_" <> name
    TupleType(elements) ->
      "#(" <> list.map(elements, type_) |> string.join(", ") <> ")"
    VariableType(name) -> name
  }
}

pub fn variant_field(variant_field: VariantField) {
  case variant_field {
    LabelledVariantField(label:, item:) -> label <> ": " <> type_(item)
    UnlabelledVariantField(item:) -> type_(item)
  }
}

pub fn variant(this variant: Variant) -> String {
  variant.name
  <> case variant.fields {
    [] -> ""
    f ->
      "("
      <> list.map(f, variant_field)
      |> string.join(", ")
      <> ")"
  }
}

pub fn custom_type(this custom_type: CustomType) {
  let publicity = case custom_type.publicity {
    Private -> ""
    Public -> "pub "
  }
  let opacity = case custom_type.opacity {
    Clear -> ""
    Opaque -> "opaque "
  }
  let variants = case custom_type.variants {
    [] -> ""
    v ->
      "{\n"
      <> list.map(v, fn(a) { "  " <> variant(a) })
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

pub fn pattern(this pat: Pattern) -> String {
  case pat {
    VariablePattern(name) -> name
    IntPattern(value) -> value
    AssignmentPattern(pat, name) -> pattern(pat) <> " as " <> name
    ConcatenatePattern(_prefix, _prefix_name, _rest_name) -> todo
    FloatPattern(value) -> value
    ListPattern(elements, tail) ->
      "["
      <> list.map(elements, pattern) |> string.join(", ")
      <> case tail {
        None -> ""
        Some(tail) -> ", .." <> pattern(tail)
      }
      <> "]"
    PatternVariant(module, constructor, arguments, with_spread) ->
      case module {
        None -> ""
        Some(module) -> module <> "."
      }
      <> constructor
      <> case arguments {
        [] -> ""
        arguments ->
          "("
          <> list.map(arguments, field(_, pattern)) |> string.join(", ")
          <> case with_spread {
            True -> ", .."
            _ -> ""
          }
          <> ")"
      }
    StringPattern(_) -> todo
    TuplePattern(_) -> todo
  }
}

pub fn statement(this statement: Statement) -> String {
  case statement {
    Expression(e) -> expression(e)
    Assignment(kind, pattern, value) -> assignment(kind, pattern, value)
    Use(patterns, value) ->
      "use "
      <> list.map(patterns, use_pattern) |> string.join(", ")
      <> " <- "
      <> expression(value)
    Assert(_, _) -> todo
  }
}

pub fn assignment(
  kind kind: AssignmentKind,
  pattern pat: Pattern,
  value value: Expression,
) -> String {
  case kind {
    LetAssert(_) -> "let assert "
    Let -> "let "
  }
  <> pattern(pat)
  <> " = "
  <> expression(value)
  <> case kind {
    LetAssert(Some(message)) -> "as " <> expression(message)
    _ -> ""
  }
}

pub fn function(this function: Function) {
  let params = fn(p: FunctionParameter) {
    case p.label {
      None -> ""
      Some(label) -> label <> " "
    }
    <> case p.name {
      Discarded(name) -> "_" <> name
      Named(name) -> name
    }
    <> case p.type_ {
      None -> ""
      Some(t) -> ": " <> type_(t)
    }
  }

  let publicity = case function.publicity {
    Private -> ""
    Public -> "pub "
  }
  let return = case function.return {
    None -> ""
    Some(r) -> " -> " <> type_(r)
  }
  let parameters =
    list.map(function.parameters, params)
    |> string.join(", ")
  let body =
    function.body
    |> list.map(fn(a) { "  " <> statement(a) })
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

pub fn type_alias(type_alias: TypeAlias) -> String {
  case type_alias.publicity {
    Private -> "alias "
    Public -> "pub alias "
  }
  <> type_alias.name
  <> case type_alias.parameters {
    [] -> " "
    parameters -> "(" <> string.concat(parameters) <> ") "
  }
  <> "= "
  <> type_(type_alias.aliased)
}
