import glance.{type CustomType, type Variant, CustomType, Private, Public}

pub fn name(custom_type: CustomType, name: String) -> CustomType {
  CustomType(..custom_type, name:)
}

pub fn make_public(custom_type: CustomType) -> CustomType {
  CustomType(..custom_type, publicity: Public)
}

pub fn make_private(custom_type: CustomType) -> CustomType {
  CustomType(..custom_type, publicity: Private)
}

pub fn make_opaque(custom_type: CustomType) -> CustomType {
  CustomType(..custom_type, opaque_: True)
}

pub fn make_clear(custom_type: CustomType) -> CustomType {
  CustomType(..custom_type, opaque_: False)
}

pub fn parameters(
  custom_type: CustomType,
  parameters: List(String),
) -> CustomType {
  CustomType(..custom_type, parameters:)
}

pub fn variants(custom_type: CustomType, variants: List(Variant)) -> CustomType {
  CustomType(..custom_type, variants:)
}
