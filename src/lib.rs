use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use xml::reader::{EventReader, XmlEvent};

/// Pathref contains the special pathRef string syntax
#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
struct PathRef {
  ref_type: String,
  target_parent: String,
  target_type: String,
}

#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
struct StringEnumeration {
  value: String,
  access: String,
  optional: bool,
}

impl StringEnumeration {
  fn new(value: String, access: String, optional: bool) -> StringEnumeration {
    StringEnumeration {
      value: value,
      access: access,
      optional: optional,
    }
  }
}

/// Contains information on the syntax of a parameter
#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
struct StringSyntax {
  min_length: usize,
  max_length: usize,
  patterns: Vec<String>,                // must match one of
  enumerations: Vec<StringEnumeration>, // string - enumeration type
  pathref: Option<PathRef>,
}

impl StringSyntax {
  fn set_min_length(&mut self, min_length: usize) {
    self.min_length = min_length;
  }
  fn set_max_length(&mut self, max_length: usize) {
    self.max_length = max_length;
  }
  fn push_pattern(&mut self, new_pattern: String) {
    self.patterns.push(new_pattern);
  }
  fn push_enumeration(&mut self, value: String, access: String, optional: bool) {
    self
      .enumerations
      .push(StringEnumeration::new(value, access, optional));
  }
}

/// Contains information on the syntax of a parameter
#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
struct BooleanSyntax {
  default: bool,
}

impl BooleanSyntax {
  fn set_default(&mut self, default: bool) {
    self.default = default;
  }
}

#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
struct IntSyntax<T> {
  min_inclusive: T,
  max_inclusive: T,
  unit: String,
}

impl<T> IntSyntax<T> {
  fn set_min_inclusive(&mut self, min_inclusive: T) {
    self.min_inclusive = min_inclusive;
  }
  fn set_max_inclusive(&mut self, max_inclusive: T) {
    self.max_inclusive = max_inclusive;
  }
  fn set_unit(&mut self, unit: String) {
    self.unit = unit;
  }
}

#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
struct HexSyntax {
  min_length: usize,
  max_length: usize,
}

impl HexSyntax {
  fn set_min_length(&mut self, min_length: usize) {
    self.min_length = min_length;
  }
  fn set_max_length(&mut self, max_length: usize) {
    self.max_length = max_length;
  }
}

#[derive(Debug, Eq, PartialEq, Hash, Default, Clone)]
struct Base64Syntax {
  min_length: usize,
  max_length: usize,
}

impl Base64Syntax {
  fn set_min_length(&mut self, min_length: usize) {
    self.min_length = min_length;
  }
  fn set_max_length(&mut self, max_length: usize) {
    self.max_length = max_length;
  }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
enum ParameterSyntax {
  None,
  String(StringSyntax),
  HexBinary(HexSyntax),
  Base64(Base64Syntax),
  DateTime,
  Integer(IntSyntax<i32>),
  UnsignedInt(IntSyntax<u32>),
  Long(IntSyntax<i64>),
  UnsignedLong(IntSyntax<u64>),
  Boolean(BooleanSyntax),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
enum ParseState {
  None,
  InString,
  InBoolean,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct DataType {
  name: Option<String>,
  description: Option<String>,
  is_list: bool,
  list_min: usize,
  list_max: usize,
  syntax: ParameterSyntax,
}

impl DataType {
  fn new(name: Option<String>, description: Option<String>) -> Self {
    DataType {
      name: name,
      description: description,
      syntax: ParameterSyntax::None,
      is_list: false,
      list_min: 0,
      list_max: 0,
    }
  }
  fn set_description(&mut self, description: Option<String>) {
    self.description = description;
  }
  fn set_name(&mut self, name: String) {
    self.name = Some(name);
  }
  fn get_name(&mut self) -> Option<String> {
    return self.name.clone();
  }
  fn set_list(&mut self, list: bool) {
    self.is_list = list;
  }
  fn set_list_min_items(&mut self, min: usize) {
    self.list_min = min;
  }
  fn set_list_max_items(&mut self, max: usize) {
    self.list_max = max;
  }
  fn set_syntax(&mut self, syntax: ParameterSyntax) {
    self.syntax = syntax;
  }
  fn get_syntax(&mut self) -> ParameterSyntax {
    return self.syntax.clone();
  }
}

/// Contains the parameter attributes from the xml spec
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct ParameterAttributes {
  access: Option<String>,
  version: Option<String>,
  active_notify: Option<String>,
  description: Option<String>,
  forced_inform: bool,
  is_list: bool,
  list_min: usize,
  list_max: usize,
  syntax: ParameterSyntax,
}

impl ParameterAttributes {
  /// Used to create a new ParameterAttribute struct
  fn new(access: Option<String>, version: Option<String>, active_notify: Option<String>) -> Self {
    ParameterAttributes {
      access: access,
      version: version,
      active_notify: active_notify,
      description: None,
      syntax: ParameterSyntax::None,
      forced_inform: false,
      is_list: false,
      list_min: 0,
      list_max: 0,
    }
  }
  /// Used when a description for a parameter is found in the spec
  fn set_description(&mut self, description: Option<String>) {
    self.description = description;
  }
  /// used when a syntax has been compiled from the spec to attach it to the
  /// attribute for which it was collected
  fn set_syntax(&mut self, syntax: ParameterSyntax) {
    self.syntax = syntax;
  }
  fn set_forced_inform(&mut self, forced_inform: bool) {
    self.forced_inform = forced_inform;
  }
  /// used when a "list" tag is found within a syntax tag
  fn set_list(&mut self, list: bool) {
    self.is_list = list;
  }
  fn set_list_min_items(&mut self, min: usize) {
    self.list_min = min;
  }
  fn set_list_max_items(&mut self, max: usize) {
    self.list_max = max;
  }
  fn set_access(&mut self, access: String) {
    self.access = Some(access);
  }
  fn set_version(&mut self, version: String) {
    self.version = Some(version);
  }
  fn set_active_notify(&mut self, active_notify: String) {
    self.active_notify = Some(active_notify);
  }
}

/// Used to parse the specification given by the filename
/// Returns a parsed set of ParameterAttributes. ie.
/// "model_version", HashMap<"parametername", ParameterAttributes>
pub fn parse_spec(
  spec_filename: String,
) -> Result<HashMap<String, HashMap<String, ParameterAttributes>>, Box<dyn std::error::Error>> {
  let file = File::open(spec_filename).unwrap();
  let file_buf = BufReader::new(file);
  let mut result: HashMap<String, HashMap<String, ParameterAttributes>> = HashMap::new();
  let mut data_types: HashMap<String, DataType> = HashMap::new();
  let parser = EventReader::new(file_buf);

  let mut current_model_name: Option<String> = None;
  let mut current_object_name: Option<String> = None;
  let mut current_parameter_name: Option<String> = None;
  let mut current_syntax: ParameterSyntax = ParameterSyntax::None;
  let mut current_parameter_attributes: ParameterAttributes =
    ParameterAttributes::new(None, None, None);
  let mut current_string_syntax: StringSyntax = StringSyntax::default();
  let mut current_boolean_syntax: BooleanSyntax = BooleanSyntax::default();
  let mut state = ParseState::None;

  let mut path = vec![];
  let mut current_datatype = DataType::new(None, None);

  for e in parser {
    match e {
      Ok(XmlEvent::StartElement {
        ref name,
        ref attributes,
        ..
      }) => {
        path.push(name.local_name.to_string());
        let path_pattern: Vec<&str> = path.iter().map(AsRef::as_ref).collect();

        match &path_pattern[..] {
          ["document", "dataType"] => {
            current_datatype = DataType::new(None, None);
            current_datatype.syntax = ParameterSyntax::None;

            if let Some(datatype_name) = extract_attribute(attributes, "name") {
              current_datatype.set_name(datatype_name);
            }
            if let Some(datatype_base) = extract_attribute(attributes, "base") {
              // use syntax from base

              // find base dataType and grab the syntax from there
              if data_types.contains_key(&datatype_base) {
                if let Some(base_type) = data_types.get(&datatype_base) {
                  let syn = base_type.syntax.clone();
                  current_datatype.set_syntax(syn);
                }
              }
            }
          }
          ["document", "dataType", "list"] => {
            current_datatype.set_list(true);
            if let Some(min_items) = extract_attribute(attributes, "minItems") {
              current_datatype.set_list_min_items(min_items.parse::<usize>().unwrap());
            }
            if let Some(max_items) = extract_attribute(attributes, "maxItems") {
              current_datatype.set_list_max_items(max_items.parse::<usize>().unwrap());
            }
          }
          ["document", "dataType", "dateTime"] => {
            current_datatype.syntax = ParameterSyntax::DateTime;
          }
          ["document", "dataType", "long"] => {
            current_datatype.syntax = ParameterSyntax::Long(IntSyntax::<i64>::default());
          }
          ["document", "dataType", "unsignedLong"] => {
            current_datatype.syntax = ParameterSyntax::UnsignedLong(IntSyntax::<u64>::default());
          }
          ["document", "dataType", "int"] => {
            current_datatype.syntax = ParameterSyntax::Integer(IntSyntax::<i32>::default());
          }
          ["document", "dataType", "unsignedInt"] => {
            current_datatype.syntax = ParameterSyntax::UnsignedInt(IntSyntax::<u32>::default());
          }
          ["document", "dataType", "string"] => {
            current_string_syntax = StringSyntax::default();
            state = ParseState::InString;
          }
          ["document", "dataType", "string", "size"] => {
            if let Some(min_length) = extract_attribute(attributes, "minLength") {
              if state == ParseState::InString {
                current_string_syntax.set_min_length(min_length.parse::<usize>().unwrap());
              }
            }
            if let Some(max_length) = extract_attribute(attributes, "maxLength") {
              if state == ParseState::InString {
                current_string_syntax.set_max_length(max_length.parse::<usize>().unwrap());
              }
            }
          }
          ["document", "dataType", "string", "pattern"] => {
            if let Some(pattern) = extract_attribute(attributes, "value") {
              if state == ParseState::InString {
                current_string_syntax.push_pattern(pattern);
              }
            }
          }
          ["document", "dataType", "string", "enumeration"] => {
            if let Some(enum_value) = extract_attribute(attributes, "value") {
              let a = extract_attribute(attributes, "access");
              let access = if a.is_some() {
                a.unwrap()
              } else {
                "readWrite".to_string()
              };
              let o = extract_attribute(attributes, "optional");
              let optional = if o.is_some() {
                o.unwrap() == "true".to_string()
              } else {
                false
              };
              current_string_syntax.push_enumeration(enum_value, access, optional);
            }
          }
          ["document", "dataType", "size"] => {
            // override of existing string syntax
            if let Some(min_length) = extract_attribute(attributes, "minLength") {
              let cur_syn = current_datatype.get_syntax();
              match cur_syn {
                ParameterSyntax::String(css) => {
                  let mut new_css = css.clone();
                  new_css.set_min_length(min_length.parse::<usize>().unwrap());
                  current_datatype.set_syntax(ParameterSyntax::String(new_css));
                }
                _ => {}
              }
            }
            if let Some(max_length) = extract_attribute(attributes, "maxLength") {
              let cur_syn = current_datatype.get_syntax();
              match cur_syn {
                ParameterSyntax::String(css) => {
                  let mut new_css = css.clone();
                  new_css.set_max_length(max_length.parse::<usize>().unwrap());
                  current_datatype.set_syntax(ParameterSyntax::String(new_css));
                }
                _ => {}
              }
            }
          }
          ["document", "dataType", "pattern"] => {
            // override of existing string patterns, just add this to the existing list of patterns
            if let Some(pattern) = extract_attribute(attributes, "value") {
              let cur_syn = current_datatype.get_syntax();
              match cur_syn {
                ParameterSyntax::String(css) => {
                  let mut new_css = css.clone();
                  new_css.push_pattern(pattern);
                  current_datatype.set_syntax(ParameterSyntax::String(new_css));
                }
                _ => {}
              }
            }
          }
          ["document", "model"] => {
            if let Some(current_model) = extract_attribute(attributes, "name") {
              current_model_name = Some(current_model.clone());
              result.insert(current_model, HashMap::new());
            }
          }
          ["document", "model", "object"] => {
            current_object_name = extract_attribute(attributes, "name");
          }
          ["document", "model", "object", "parameter"] => {
            current_parameter_attributes = ParameterAttributes::new(None, None, None);

            // now extract the attribute values
            if let Some(parameter_name) = extract_attribute(attributes, "name") {
              current_parameter_name = Some(format!(
                "{}{}",
                current_object_name.clone().unwrap(),
                parameter_name
              ));
            }

            if let Some(parameter_access) = extract_attribute(attributes, "access") {
              current_parameter_attributes.set_access(parameter_access);
            }
            if let Some(parameter_version) = extract_attribute(attributes, "version") {
              current_parameter_attributes.set_version(parameter_version);
            }
            if let Some(parameter_active_notify) = extract_attribute(attributes, "active_notify") {
              current_parameter_attributes.set_active_notify(parameter_active_notify);
            }
            if let Some(forced_inform) = extract_attribute(attributes, "forcedInform") {
              current_parameter_attributes.set_forced_inform(forced_inform == String::from("true"));
            }
          }

          ["document", "model", "object", "parameter", "syntax"] => {
            current_syntax = ParameterSyntax::None;
          }
          ["document", "model", "object", "parameter", "syntax", "dateTime"] => {
            current_syntax = ParameterSyntax::DateTime;
          }
          ["document", "model", "object", "parameter", "syntax", "long"] => {
            current_syntax = ParameterSyntax::Long(IntSyntax::<i64>::default());
          }
          ["document", "model", "object", "parameter", "syntax", "unsignedLong"] => {
            current_syntax = ParameterSyntax::UnsignedLong(IntSyntax::<u64>::default());
          }
          ["document", "model", "object", "parameter", "syntax", "int"] => {
            current_syntax = ParameterSyntax::Integer(IntSyntax::<i32>::default());
          }
          ["document", "model", "object", "parameter", "syntax", "unsignedInt"] => {
            current_syntax = ParameterSyntax::UnsignedInt(IntSyntax::<u32>::default());
          }
          ["document", "model", "object", "parameter", "syntax", "int", "range"] => {
            let mut int_syn = IntSyntax::<i32>::default();
            if let Some(min) = extract_attribute(attributes, "minInclusive") {
              int_syn.set_min_inclusive(min.parse::<i32>().unwrap());
            }
            if let Some(max) = extract_attribute(attributes, "maxInclusive") {
              int_syn.set_max_inclusive(max.parse::<i32>().unwrap());
            }
            current_syntax = ParameterSyntax::Integer(int_syn);
          }

          ["document", "model", "object", "parameter", "syntax", "long", "range"] => {
            match current_syntax {
              ParameterSyntax::Long(long_syntax) => {
                let mut long_syn = long_syntax.clone();
                if let Some(min) = extract_attribute(attributes, "minInclusive") {
                  long_syn.set_min_inclusive(min.parse::<i64>().unwrap());
                }
                if let Some(max) = extract_attribute(attributes, "maxInclusive") {
                  long_syn.set_max_inclusive(max.parse::<i64>().unwrap());
                }
                current_syntax = ParameterSyntax::Long(long_syn);
              }
              _ => {}
            }
          }
          ["document", "model", "object", "parameter", "syntax", "long", "units"] => {
            match current_syntax {
              ParameterSyntax::Long(long_syntax) => {
                let mut long_syn = long_syntax.clone();
                if let Some(unit_name) = extract_attribute(attributes, "value") {
                  long_syn.set_unit(unit_name);
                }
                current_syntax = ParameterSyntax::Long(long_syn);
              }
              _ => {}
            }
          }
          ["document", "model", "object", "parameter", "syntax", "hexBinary"] => {
            current_syntax = ParameterSyntax::HexBinary(HexSyntax::default());
          }
          ["document", "model", "object", "parameter", "syntax", "hexBinary", "size"] => {
            let mut hex_syn = HexSyntax::default();
            if let Some(min) = extract_attribute(attributes, "minLength") {
              hex_syn.set_min_length(min.parse::<usize>().unwrap());
            }
            if let Some(max) = extract_attribute(attributes, "maxLength") {
              hex_syn.set_max_length(max.parse::<usize>().unwrap());
            }
            current_syntax = ParameterSyntax::HexBinary(hex_syn);
          }
          ["document", "model", "object", "parameter", "syntax", "base64"] => {
            current_syntax = ParameterSyntax::Base64(Base64Syntax::default());
          }
          ["document", "model", "object", "parameter", "syntax", "base64", "size"] => {
            let mut b64_syn = Base64Syntax::default();
            if let Some(min) = extract_attribute(attributes, "minLength") {
              b64_syn.set_min_length(min.parse::<usize>().unwrap());
            }
            if let Some(max) = extract_attribute(attributes, "maxLength") {
              b64_syn.set_max_length(max.parse::<usize>().unwrap());
            }
            current_syntax = ParameterSyntax::Base64(b64_syn);
          }
          ["document", "model", "object", "parameter", "syntax", "boolean"] => {
            current_boolean_syntax = BooleanSyntax::default();
            state = ParseState::InBoolean;
          }
          ["document", "model", "object", "parameter", "syntax", "default"] => {
            if let Some(val) = extract_attribute(attributes, "value") {
              if state == ParseState::InBoolean {
                current_boolean_syntax.set_default(val == "true");
              }
            }
          }
          ["document", "model", "object", "parameter", "syntax", "string"] => {
            current_string_syntax = StringSyntax::default();
            state = ParseState::InString;
          }
          ["document", "model", "object", "parameter", "syntax", "string", "size"] => {
            if let Some(min_length) = extract_attribute(attributes, "minLength") {
              if state == ParseState::InString {
                current_string_syntax.set_min_length(min_length.parse::<usize>().unwrap());
              }
            }
            if let Some(max_length) = extract_attribute(attributes, "maxLength") {
              if state == ParseState::InString {
                current_string_syntax.set_max_length(max_length.parse::<usize>().unwrap());
              }
            }
          }
          ["document", "model", "object", "parameter", "syntax", "string", "pattern"] => {
            if let Some(pattern) = extract_attribute(attributes, "value") {
              if state == ParseState::InString {
                current_string_syntax.push_pattern(pattern);
              }
            }
          }
          ["document", "model", "object", "parameter", "syntax", "string", "enumeration"] => {
            if let Some(enum_value) = extract_attribute(attributes, "value") {
              let a = extract_attribute(attributes, "access");
              let access = if a.is_some() {
                a.unwrap()
              } else {
                "readWrite".to_string()
              };
              let o = extract_attribute(attributes, "optional");
              let optional = if o.is_some() {
                o.unwrap() == "true".to_string()
              } else {
                false
              };
              current_string_syntax.push_enumeration(enum_value, access, optional);
            }
          }
          ["document", "model", "object", "parameter", "syntax", "list"] => {
            current_parameter_attributes.set_list(true);
            if let Some(min_items) = extract_attribute(attributes, "minItems") {
              current_parameter_attributes.set_list_min_items(min_items.parse::<usize>().unwrap());
            }
            if let Some(max_items) = extract_attribute(attributes, "maxItems") {
              current_parameter_attributes.set_list_max_items(max_items.parse::<usize>().unwrap());
            }
          }
          ["document", "model", "object", "parameter", "syntax", "dataType"] => {
            // a reference to a dataType entry
            if let Some(r) = extract_attribute(attributes, "ref") {
              if data_types.contains_key(&r) {
                if let Some(base_type) = data_types.get(&r) {
                  current_syntax = base_type.syntax.clone();
                }
              }
            }
          }

          _ => {}
        }
      }
      Ok(XmlEvent::EndElement { name: _ }) => {
        let path_pattern: Vec<&str> = path.iter().map(AsRef::as_ref).collect();

        match &path_pattern[..] {
          ["document", "dataType"] => {
            if state == ParseState::InString {
              current_datatype.syntax = ParameterSyntax::String(current_string_syntax.clone());
              state = ParseState::None;
            }
            // we are done collecting elements for the current_datatype, now
            // stuff it into the data_types HashMap
            data_types.insert(
              current_datatype.get_name().unwrap(),
              current_datatype.clone(),
            );
          }
          ["document", "model", "object", "parameter"] => {
            if let Some(m) = result.get_mut(&current_model_name.clone().unwrap()) {
              if let Some(pn) = current_parameter_name {
                // println!("matched pn {:?}", current_parameter_attributes);
                m.insert(pn, current_parameter_attributes.clone());
              }
            }
            current_parameter_name = None;
          }
          ["document", "model"] => {
            current_model_name = None;
          }
          ["document", "model", "object", "parameter", "syntax"] => {
            current_syntax = match state {
              ParseState::InBoolean => ParameterSyntax::Boolean(current_boolean_syntax.clone()),
              ParseState::InString => ParameterSyntax::String(current_string_syntax.clone()),
              _ => current_syntax,
            };
            current_parameter_attributes.set_syntax(current_syntax.clone());
            state = ParseState::None;
          }
          _ => {}
        }
        path.pop();
      }
      Ok(XmlEvent::Characters(ref s)) => {
        let path_pattern: Vec<&str> = path.iter().map(AsRef::as_ref).collect();
        match &path_pattern[..] {
          ["document", "dataType", "description"] => {
            current_datatype.set_description(Some(s.to_string()));
          }
          ["document", "model", "object", "parameter", "description"] => {
            current_parameter_attributes.set_description(Some(s.to_string()));
          }

          _ => {}
        }
      }
      Err(e) => {
        println!("XML parse error: {}", e);
        break;
      }
      _ => {}
    }
  }

  Ok(result)
}

/// Extracts the value of a given attributes
fn extract_attribute(
  attributes: &Vec<xml::attribute::OwnedAttribute>,
  attrib_name: &str,
) -> Option<String> {
  let f = attributes
    .iter()
    .filter(|&x| x.name.local_name == attrib_name)
    .next();
  match f {
    Some(e) => Some(e.value.to_string()),
    None => None,
  }
}

#[cfg(test)]
#[macro_use]
extern crate lazy_static;

#[cfg(test)]
mod tests {
  use super::*;

  lazy_static! {
    static ref SPEC: HashMap<String, HashMap<String, ParameterAttributes>> =
      parse_spec("./tr-181-2-13-0-cwmp-full.xml".to_string()).unwrap();
  }

  #[test]
  fn parse_string_list_1() {
    assert_eq!(SPEC.len(), 1);
    // check specific syntaxes, as many as you have energy for...
    assert_eq!(SPEC.contains_key("Device:2.13"), true);
    let a = SPEC.get("Device:2.13");
    assert_eq!(a.is_some(), true);
    if let Some(attr) = a {
      assert_eq!(attr.keys().len(), 4525);
      // start asserting syntaxes

      assert!(attr.contains_key("Device.DeviceInfo.DeviceCategory"));
      if let Some(dc) = attr.get("Device.DeviceInfo.DeviceCategory") {
        assert!(dc.is_list);
        assert_eq!(dc.syntax, ParameterSyntax::String(StringSyntax::default()));
      }
    }
  }

  #[test]
  fn basetype_reference_parsed_1() {
    let mut vca_ssyn = StringSyntax::default();
    vca_ssyn.set_max_length(64);
    perform_test(
      "Device.DeviceInfo.VendorConfigFile.{i}.Alias",
      ParameterSyntax::String(vca_ssyn),
    );
  }

  #[test]
  fn string_enumeration_parsed_1() {
    let mut exp_tssyn = StringSyntax::default();
    exp_tssyn.push_enumeration("Disabled".to_string(), "readWrite".to_string(), false);
    exp_tssyn.push_enumeration("Enabled".to_string(), "readWrite".to_string(), false);
    exp_tssyn.push_enumeration("Error".to_string(), "readWrite".to_string(), false);

    perform_test(
      "Device.DeviceInfo.TemperatureStatus.TemperatureSensor.{i}.Status",
      ParameterSyntax::String(exp_tssyn),
    );
  }

  #[test]
  fn boolean_parsed_1() {
    perform_test(
      "Device.Security.Certificate.{i}.Enable",
      ParameterSyntax::Boolean(BooleanSyntax::default()),
    );
  }

  #[test]
  fn long_parsed_1() {
    let mut exp_syn = IntSyntax::<i64>::default();
    exp_syn.set_min_inclusive(-1);
    exp_syn.set_unit("seconds".to_string());

    perform_test(
      "Device.XMPP.Connection.{i}.KeepAliveInterval",
      ParameterSyntax::Long(exp_syn),
    );
  }

  #[test]
  fn ipaddress_parsed_1() {
    // an IP address, ie. a string with a specific length and pattern
    let mut exp_ssyn = StringSyntax::default();
    exp_ssyn.push_pattern("".to_string());
    exp_ssyn.push_pattern(
      "((25[0-5]|2[0-4][0-9]|[01]?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9]?[0-9])"
        .to_string(),
    );
    exp_ssyn.set_max_length(15);
    perform_test(
      "Device.DHCPv4.Server.Pool.{i}.MinAddress",
      ParameterSyntax::String(exp_ssyn),
    );
  }

  fn perform_test(param_name: &str, expected_syntax: ParameterSyntax) {
    let a = SPEC.get("Device:2.13");
    if let Some(attr) = a {
      assert!(attr.contains_key(&param_name.to_string()));

      if let Some(pa) = attr.get(&param_name.to_string()) {
        assert_eq!(pa.syntax, expected_syntax);
      }
    }
  }
}
