import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

import gig/typed_ast
import glance

import genome/ast.{
  type AssignmentName, type Attribute, type BinaryOperator, type Clause,
  type Constant, type CustomType, type Expression, type Field, type Function,
  type Import, type Module, type Pattern, type Statement, type Type,
  type TypeAlias, type UsePattern, type Variant, AddFloat, AddInt, And, Assert,
  Assignment, AssignmentPattern, BigOption, BinaryOperator, BitsOption, Block,
  BytesOption, Call, Case, Clause, Clear, Concatenate, ConcatenatePattern,
  Constant, CustomType, Discarded, DivFloat, DivInt, Echo, Eq, Expression,
  External, FieldAccess, Float, FloatOption, FloatPattern, Fn, FnCapture,
  Function, FunctionParameter, FunctionType, Genome, GtEqFloat, GtEqInt, GtFloat,
  GtInt, HoleType, Import, ImportType, ImportValue, Int, IntOption, IntPattern,
  LabelledField, Let, LetAssert, List, ListPattern, LittleOption, LtEqFloat,
  LtEqInt, LtFloat, LtInt, Module, MultFloat, MultInt, Named, NamedType,
  NativeOption, NegateBool, NegateInt, NotEq, Opaque, Or, Panic, Pipe, Private,
  Public, RecordUpdate, RemainderInt, ShorthandField, SignedOption, SizeOption,
  SizeValueOption, String, StringPattern, SubFloat, SubInt, Todo, Tuple,
  TupleIndex, TuplePattern, TupleType, TypeAlias, UnitOption, UnlabelledField,
  UnsignedOption, Use, UsePattern, Utf16CodepointOption, Utf16Option,
  Utf32CodepointOption, Utf32Option, Utf8CodepointOption, Utf8Option, Variable,
  VariablePattern, VariableType, Variant,
}

pub fn typed_module(
  glance_module: glance.Module,
  name: String,
  source: String,
) -> typed_ast.Context {
  typed_ast.infer_module(typed_ast.new_context(), glance_module, name, source)
}

pub fn module(glance_module: glance.Module) -> Module {
  let imports = glance_module.imports |> list.map(import_)
  let constants = glance_module.constants |> list.map(constant)
  let custom_types = glance_module.custom_types |> list.map(custom_type)
  let functions = glance_module.functions |> list.map(function)
  let type_aliases = glance_module.type_aliases |> list.map(type_alias)

  Module(imports:, constants:, custom_types:, functions:, type_aliases:)
}

pub fn append(this module: Module, with other: Module) -> Module {
  Module(
    imports: list.append(module.imports, other.imports),
    constants: list.append(module.constants, other.constants),
    custom_types: list.append(module.custom_types, other.custom_types),
    functions: list.append(module.functions, other.functions),
    type_aliases: list.append(module.type_aliases, other.type_aliases),
  )
}

pub fn attribute(glance_attribute: glance.Attribute) -> Attribute {
  case glance_attribute {
    glance.Attribute("genome", [glance.String(_, kind), ..arguments]) ->
      Genome(kind:, arguments: list.map(arguments, expression))
    glance.Attribute(
      "external",
      [
        glance.Variable(location: _, name: target),
        glance.String(location: _, value: path),
        glance.String(location: _, value:),
      ],
    ) -> External(target, path, value)
    glance.Attribute(..) -> {
      let msg = "invalid attribute: " <> string.inspect(glance_attribute)
      panic as msg
    }
  }
}

pub fn import_(glance_import: glance.Definition(glance.Import)) -> Import {
  let unqualified = fn(import_, constructor: fn(_, _) -> a) -> a {
    let glance.UnqualifiedImport(name:, alias:) = import_
    constructor(name, alias)
  }

  let attributes = glance_import.attributes |> list.map(attribute)
  let unqualified_types =
    glance_import.definition.unqualified_types
    |> list.map(unqualified(_, ImportType))
  let unqualified_values =
    glance_import.definition.unqualified_values
    |> list.map(unqualified(_, ImportValue))
  let alias = case glance_import.definition.alias {
    Some(glance.Named(s)) -> Some(s)
    Some(glance.Discarded(s)) -> Some("_" <> s)
    _ -> None
  }

  Import(
    attributes:,
    module: glance_import.definition.module,
    unqualified_values:,
    unqualified_types:,
    alias:,
  )
}

pub fn binary_operator(
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

pub fn clause(glance_clause: glance.Clause) -> Clause {
  let glance.Clause(patterns:, guard:, body:) = glance_clause

  Clause(
    patterns: list.map(patterns, list.map(_, pattern)),
    guard: option.map(guard, expression),
    body: expression(body),
  )
}

pub fn bitstring_segment_option(
  glance_bitstring_segment_option: glance.BitStringSegmentOption(t),
) {
  case glance_bitstring_segment_option {
    glance.BigOption -> BigOption
    glance.BitsOption -> BitsOption
    glance.BytesOption -> BytesOption
    glance.FloatOption -> FloatOption
    glance.IntOption -> IntOption
    glance.LittleOption -> LittleOption
    glance.NativeOption -> NativeOption
    glance.SignedOption -> SignedOption
    glance.SizeOption(i) -> SizeOption(i)
    glance.SizeValueOption(t) -> SizeValueOption(t)
    glance.UnitOption(i) -> UnitOption(i)
    glance.UnsignedOption -> UnsignedOption
    glance.Utf16CodepointOption -> Utf16CodepointOption
    glance.Utf16Option -> Utf16Option
    glance.Utf32CodepointOption -> Utf32CodepointOption
    glance.Utf32Option -> Utf32Option
    glance.Utf8CodepointOption -> Utf8CodepointOption
    glance.Utf8Option -> Utf8Option
  }
}

pub fn expression(glance_expression: glance.Expression) -> Expression {
  case glance_expression {
    glance.BinaryOperator(location: _, name:, left:, right:) ->
      BinaryOperator(binary_operator(name), expression(left), expression(right))
    glance.BitString(_, _segments) -> todo
    glance.Block(_, body) -> Block(list.map(body, statement))
    glance.Call(_, e, f) -> Call(expression(e), list.map(f, field_expression))
    glance.Case(location: _, subjects:, clauses:) ->
      Case(list.map(subjects, expression), list.map(clauses, clause))
    glance.FieldAccess(_, e, f) -> FieldAccess(expression(e), f)
    glance.Float(_, f) -> {
      let assert Ok(f) = float.parse(f)
      Float(f)
    }
    glance.Fn(_, parameters, return, body) ->
      Fn(
        list.map(parameters, fn(a) {
          #(
            case a.name {
              glance.Discarded(s) -> "_" <> s
              glance.Named(s) -> s
            },
            option.map(a.type_, type_),
          )
        }),
        option.map(return, type_),
        list.map(body, statement),
      )
    glance.FnCapture(_, label, function, before, after) ->
      FnCapture(
        label,
        expression(function),
        list.map(before, field_expression),
        list.map(after, field_expression),
      )
    glance.Int(_, i) -> {
      let assert Ok(i) = int.parse(i)
      Int(i)
    }
    glance.List(_, e, r) ->
      List(list.map(e, expression), option.map(r, expression))
    glance.NegateBool(_, value) -> NegateBool(expression(value))
    glance.NegateInt(_, value) -> NegateInt(expression(value))
    glance.Panic(_, message) -> Panic(option.map(message, expression))
    glance.RecordUpdate(_, module, constructor, record, fields) ->
      RecordUpdate(
        module:,
        constructor:,
        record: expression(record),
        fields: list.map(fields, fn(a) {
          let glance.RecordUpdateField(label, item) = a
          #(label, option.map(item, expression))
        }),
      )
    glance.String(_, s) -> String(s)
    glance.Todo(_, message) -> Todo(option.map(message, expression))
    glance.Tuple(_, e) -> Tuple(list.map(e, expression))
    glance.TupleIndex(_, tuple, index) -> TupleIndex(expression(tuple), index)
    glance.Variable(_, v) -> Variable(None, v)
    glance.Echo(_, expr) -> Echo(option.map(expr, expression))
  }
}

pub fn use_pattern(glance_use_pattern: glance.UsePattern) -> UsePattern {
  UsePattern(
    pattern: pattern(glance_use_pattern.pattern),
    annotation: option.map(glance_use_pattern.annotation, type_),
  )
}

pub fn constant(glance_constant: glance.Definition(glance.Constant)) -> Constant {
  let publicity = case glance_constant.definition.publicity {
    glance.Private -> Private
    glance.Public -> Public
  }

  Constant(
    publicity:,
    name: glance_constant.definition.name,
    annotation: option.map(glance_constant.definition.annotation, type_),
    value: expression(glance_constant.definition.value),
  )
}

pub fn type_(glance_type: glance.Type) -> Type {
  case glance_type {
    glance.FunctionType(_, parameters, return) ->
      FunctionType(list.map(parameters, type_), type_(return))
    glance.HoleType(_, name) -> HoleType(name)
    glance.NamedType(_, name, module, parameters) ->
      NamedType(name, module, list.map(parameters, type_))
    glance.TupleType(_, elements) -> TupleType(list.map(elements, type_))
    glance.VariableType(_, name) -> VariableType(name)
  }
}

pub fn variant(glance_variant: glance.Variant) -> Variant {
  let fields =
    glance_variant.fields
    |> list.map(fn(a) {
      case a {
        glance.LabelledVariantField(t, l) ->
          ast.LabelledVariantField(l, type_(t))
        glance.UnlabelledVariantField(t) -> ast.UnlabelledVariantField(type_(t))
      }
    })

  Variant(name: glance_variant.name, fields:)
}

pub fn custom_type(
  glance_custom_type: glance.Definition(glance.CustomType),
) -> CustomType {
  let publicity = case glance_custom_type.definition.publicity {
    glance.Private -> Private
    glance.Public -> Public
  }
  let opacity = case glance_custom_type.definition.opaque_ {
    True -> Opaque
    False -> Clear
  }
  let variants = glance_custom_type.definition.variants |> list.map(variant)

  CustomType(
    attributes: [],
    publicity:,
    opacity:,
    name: glance_custom_type.definition.name,
    parameters: glance_custom_type.definition.parameters,
    variants:,
  )
}

pub fn assignment_name(
  glance_assignment_name: glance.AssignmentName,
) -> AssignmentName {
  case glance_assignment_name {
    glance.Discarded(name) -> Discarded(name)
    glance.Named(name) -> Named(name)
  }
}

pub fn field(glance_field: glance.Field(t)) -> Field(t) {
  case glance_field {
    glance.LabelledField(label, item) -> LabelledField(label, item)
    glance.ShorthandField(label) -> ShorthandField(label)
    glance.UnlabelledField(item) -> UnlabelledField(item)
  }
}

pub fn field_pattern(
  glance_field: glance.Field(glance.Pattern),
) -> Field(Pattern) {
  case glance_field {
    glance.LabelledField(label, item) -> LabelledField(label, pattern(item))
    glance.ShorthandField(label) -> ShorthandField(label)
    glance.UnlabelledField(item) -> UnlabelledField(pattern(item))
  }
}

pub fn field_expression(
  glance_field: glance.Field(glance.Expression),
) -> Field(Expression) {
  case glance_field {
    glance.LabelledField(label, item) -> LabelledField(label, expression(item))
    glance.ShorthandField(label) -> ShorthandField(label)
    glance.UnlabelledField(item) -> UnlabelledField(expression(item))
  }
}

pub fn pattern(glance_pattern: glance.Pattern) -> Pattern {
  case glance_pattern {
    glance.PatternVariable(_, name) -> VariablePattern(name)
    glance.PatternAssignment(_, pat, name) ->
      AssignmentPattern(pattern(pat), name)
    glance.PatternBitString(_, _) -> todo
    glance.PatternConcatenate(_, prefix, prefix_name, rest_name) ->
      ConcatenatePattern(
        prefix,
        option.map(prefix_name, assignment_name),
        assignment_name(rest_name),
      )
    glance.PatternVariant(_, module, constructor, arguments, with_spread) ->
      ast.PatternVariant(
        module,
        constructor,
        list.map(arguments, field_pattern),
        with_spread,
      )
    glance.PatternDiscard(_, name) -> VariablePattern("_" <> name)
    glance.PatternFloat(_, value) -> FloatPattern(value)
    glance.PatternInt(_, s) -> IntPattern(s)
    glance.PatternList(_, elements, tail) ->
      ListPattern(list.map(elements, pattern), option.map(tail, pattern))
    glance.PatternString(_, value) -> StringPattern(value)
    glance.PatternTuple(_, elements) ->
      TuplePattern(list.map(elements, pattern))
  }
}

pub fn statement(glance_statement: glance.Statement) -> Statement {
  case glance_statement {
    glance.Assignment(_, kind, pat, _, value) ->
      Assignment(
        case kind {
          glance.LetAssert(message) ->
            LetAssert(option.map(message, expression))
          glance.Let -> Let
        },
        pattern(pat),
        expression(value),
      )
    glance.Expression(e) -> Expression(expression(e))
    glance.Use(_, patterns, value) ->
      Use(list.map(patterns, use_pattern), expression(value))
    glance.Assert(_, expr, message) ->
      Assert(expression(expr), option.map(message, expression))
  }
}

pub fn function(glance_function: glance.Definition(glance.Function)) -> Function {
  let publicity = case glance_function.definition.publicity {
    glance.Private -> Private
    glance.Public -> Public
  }
  let parameters =
    glance_function.definition.parameters
    |> list.map(fn(a) {
      let label = a.label
      let name = assignment_name(a.name)
      let type_ = option.map(a.type_, type_)

      FunctionParameter(label:, name:, type_:)
    })

  Function(
    attributes: glance_function.attributes |> list.map(attribute),
    publicity:,
    name: glance_function.definition.name,
    parameters:,
    return: glance_function.definition.return |> option.map(type_),
    body: glance_function.definition.body |> list.map(statement),
  )
}

pub fn type_alias(
  glance_type_alias: glance.Definition(glance.TypeAlias),
) -> TypeAlias {
  let publicity = case glance_type_alias.definition.publicity {
    glance.Private -> Private
    glance.Public -> Public
  }

  TypeAlias(
    name: glance_type_alias.definition.name,
    publicity:,
    parameters: glance_type_alias.definition.parameters,
    aliased: type_(glance_type_alias.definition.aliased),
  )
}
