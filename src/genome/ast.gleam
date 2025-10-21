import gleam/list
import gleam/option.{type Option, None}

import justin

pub type Publicity {
  Private
  Public
}

pub type Opacity {
  Clear
  Opaque
}

pub type Module {
  Module(
    imports: List(Import),
    constants: List(Constant),
    type_aliases: List(TypeAlias),
    custom_types: List(CustomType),
    functions: List(Function),
  )
}

pub fn new() -> Module {
  Module([], [], [], [], [])
}

pub type Attribute {
  External(target: String, path: String, value: String)
  Genome(kind: String, arguments: List(Expression))
}

pub type ImportType {
  ImportType(name: String, alias: Option(String))
}

pub type ImportValue {
  ImportValue(name: String, alias: Option(String))
}

pub type Import {
  Import(
    attributes: List(Attribute),
    module: String,
    unqualified_values: List(ImportValue),
    unqualified_types: List(ImportType),
    alias: Option(String),
  )
}

pub type TypeAlias {
  TypeAlias(
    name: String,
    publicity: Publicity,
    parameters: List(String),
    aliased: Type,
  )
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
  RemainderFloat
  Concatenate
}

pub type Clause {
  Clause(
    patterns: List(List(Pattern)),
    guard: Option(Expression),
    body: Expression,
  )
}

pub type BitStringSegmentOption(t) {
  BytesOption
  IntOption
  FloatOption
  BitsOption
  Utf8Option
  Utf16Option
  Utf32Option
  Utf8CodepointOption
  Utf16CodepointOption
  Utf32CodepointOption
  SignedOption
  UnsignedOption
  BigOption
  LittleOption
  NativeOption
  SizeValueOption(t)
  SizeOption(Int)
  UnitOption(Int)
}

pub type Expression {
  List(elements: List(Expression), rest: Option(Expression))
  String(String)
  Int(Int)
  Float(Float)
  Bool(Bool)
  Variable(module: Option(String), name: String)
  Call(function: Expression, arguments: List(Field(Expression)))
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
  Case(subjects: List(Expression), clauses: List(Clause))
  BitString(
    segments: List(#(Expression, List(BitStringSegmentOption(Expression)))),
  )
  Block(List(Statement))
  FnCapture(
    Option(String),
    Expression,
    List(Field(Expression)),
    List(Field(Expression)),
  )
  Todo(Option(Expression))
  TupleIndex(Expression, Int)
  Echo(Option(Expression))
  Panic(Option(Expression))
  NegateBool(Expression)
  NegateInt(Expression)
}

pub type Constant {
  Constant(
    publicity: Publicity,
    name: String,
    annotation: Option(Type),
    value: Expression,
  )
}

pub type AssignmentName {
  Discarded(String)
  Named(String)
}

pub type AssignmentKind {
  Let
  LetAssert(message: Option(Expression))
}

pub type Pattern {
  VariablePattern(name: String)
  IntPattern(value: String)
  AssignmentPattern(Pattern, String)
  ConcatenatePattern(String, option.Option(AssignmentName), AssignmentName)
  PatternVariant(option.Option(String), String, List(Field(Pattern)), Bool)
  FloatPattern(String)
  ListPattern(List(Pattern), option.Option(Pattern))
  StringPattern(String)
  TuplePattern(List(Pattern))
}

pub type UsePattern {
  UsePattern(pattern: Pattern, annotation: Option(Type))
}

pub type Field(t) {
  LabelledField(label: String, item: t)
  ShorthandField(label: String)
  UnlabelledField(item: t)
}

pub type Type {
  NamedType(name: String, module: Option(String), parameters: List(Type))
  FunctionType(List(Type), Type)
  HoleType(String)
  TupleType(List(Type))
  VariableType(String)
}

pub type Statement {
  Assignment(kind: AssignmentKind, pattern: Pattern, value: Expression)
  Use(patterns: List(UsePattern), value: Expression)
  Expression(Expression)
  Assert(Expression, Option(Expression))
}

pub type CustomType {
  CustomType(
    attributes: List(Attribute),
    publicity: Publicity,
    opacity: Opacity,
    name: String,
    parameters: List(String),
    variants: List(Variant),
  )
}

pub type VariantField {
  LabelledVariantField(label: String, item: Type)
  UnlabelledVariantField(item: Type)
}

pub type Variant {
  Variant(name: String, fields: List(VariantField))
}

pub type FunctionParameter {
  FunctionParameter(
    label: Option(String),
    name: AssignmentName,
    type_: Option(Type),
  )
}

pub type Function {
  Function(
    attributes: List(Attribute),
    publicity: Publicity,
    name: String,
    parameters: List(FunctionParameter),
    return: Option(Type),
    body: List(Statement),
  )
}

pub type Edit(a) {
  Edit(remove: fn(Module, a) -> Module, insert: fn(Module, a) -> Module)
}

pub fn function(name: String, body: List(Statement)) -> Function {
  Function(
    attributes: [],
    name:,
    publicity: Private,
    parameters: [],
    return: None,
    body:,
  )
}

pub fn constant(name: String, value: Expression) -> Constant {
  Constant(name:, publicity: Private, annotation: None, value:)
}

pub fn type_alias(name: String, aliased: Type) -> TypeAlias {
  TypeAlias(name:, publicity: Private, parameters: [], aliased:)
}

pub fn custom_type(name: String) -> CustomType {
  CustomType(
    attributes: [],
    name:,
    publicity: Private,
    opacity: Clear,
    parameters: [],
    variants: [],
  )
}

pub fn import_(module: String) -> Import {
  Import(
    attributes: [],
    module:,
    alias: None,
    unqualified_types: [],
    unqualified_values: [],
  )
}

pub fn variant_field(label label: String, item item: Type) -> VariantField {
  let label = justin.snake_case(label)

  case label {
    "" -> UnlabelledVariantField(item:)
    _ -> LabelledVariantField(label:, item:)
  }
}

pub fn variant(name name: String, fields fields: List(VariantField)) -> Variant {
  let name = justin.pascal_case(name)

  Variant(name:, fields:)
}

pub fn edit(
  in module: Module,
  at selector: fn(Module) -> #(List(a), Edit(a)),
  with transform: fn(a) -> a,
) -> Module {
  let #(values, Edit(remove:, insert:)) = selector(module)

  values
  |> list.fold(from: module, with: fn(module, value) {
    module |> remove(value) |> insert(value |> transform)
  })
}
