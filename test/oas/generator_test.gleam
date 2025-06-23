import birdie
import gleam/dict
import gleam/option.{None, Some}
import non_empty_list.{NonEmptyList}
import oas
import oas/generator as gen

const just_string = oas.String(None, None, None, None, False, None, None, False)

const nullable_string = oas.String(
  None,
  None,
  None,
  None,
  True,
  None,
  None,
  False,
)

const just_integer = oas.Integer(
  None,
  None,
  None,
  None,
  None,
  False,
  None,
  None,
  False,
)

fn array(items) {
  oas.Array(None, None, False, items, False, None, None, False)
}

fn object(params, required) {
  oas.Object(
    dict.from_list(params),
    required,
    None,
    None,
    0,
    False,
    None,
    None,
    False,
  )
}

fn dict(of) {
  oas.Object(dict.new(), [], Some(of), None, 0, False, None, None, False)
}

fn ref(thing) {
  oas.Ref("#/components/schemas/" <> thing, None, None)
}

fn schema(schemas) {
  gen.gen_schema_file(dict.from_list(schemas), "myservice")
}

pub fn nil_alias_test() {
  schema([#("other_nil", oas.Null(None, None, False))])
  |> birdie.snap(title: "nil_alias_test")
}

// Simple things don't become new custom types
pub fn simple_string_schema_test() {
  schema([#("my_string", just_string)])
  |> birdie.snap(title: "simple string schema test")
}

pub fn always_test() {
  schema([#("yes", oas.AlwaysPasses)])
  |> birdie.snap(title: "always_test")
}

pub fn never_test() {
  schema([#("yes", oas.AlwaysFails)])
  |> birdie.snap(title: "never_test")
}

// pub fn simple_nullable_schema_test() {
//   schema([#("my_string", just_string)])
//   |> birdie.snap(title: "simple_nullable_schema_test")
// }

pub fn array_of_primitive_test() {
  schema([#("my_array", array(oas.Inline(just_integer)))])
  |> birdie.snap(title: "array_of_primitive_test")
}

pub fn nested_array_of_primitive_test() {
  schema([#("my_array", array(oas.Inline(array(oas.Inline(just_integer)))))])
  |> birdie.snap(title: "nested_array_of_primitive_test")
}

pub fn array_of_references_test() {
  schema([#("my_array", array(ref("totals")))])
  |> birdie.snap(title: "array_of_references_test")
}

pub fn inline_object_schema_test() {
  let parameters = [
    #("name", oas.Inline(just_string)),
    #("age", oas.Inline(just_integer)),
    #("active", oas.Inline(oas.Boolean(False, None, None, False))),
  ]
  schema([#("user", object(parameters, []))])
  |> birdie.snap(title: "inline_object_schema_test")
}

pub fn required_fields_test() {
  let parameters = [#("name", oas.Inline(just_string))]
  schema([#("user", object(parameters, ["name"]))])
  |> birdie.snap(title: "required_fields_test")
}

pub fn nullable_fields_test() {
  let parameters = [#("name", oas.Inline(nullable_string))]
  schema([#("user", object(parameters, ["name"]))])
  |> birdie.snap(title: "nullable_fields_test")
}

pub fn ref_object_schema_test() {
  schema([
    #("fullname", just_string),
    #("user", object([#("name", ref("fullname"))], ["name"])),
  ])
  |> birdie.snap(title: "ref_object_schema_test")
}

pub fn nested_object_test() {
  schema([
    #(
      "big_box",
      object(
        [
          #(
            "little_box",
            oas.Inline(object([#("fish", oas.Inline(just_string))], ["fish"])),
          ),
        ],
        ["little_box"],
      ),
    ),
    #(
      "big_bird",
      object(
        [
          #(
            "little_bird",
            oas.Inline(object([#("fish", oas.Inline(just_integer))], ["fish"])),
          ),
        ],
        ["little_bird"],
      ),
    ),
  ])
  |> birdie.snap(title: "nested_object_test")
}

pub fn pure_dictionary_test() {
  schema([#("Bag", dict(oas.Inline(just_integer)))])
  |> birdie.snap(title: "pure_dictionary_test")
}

pub fn nested_dictionary_test() {
  schema([#("Bag", dict(oas.Inline(dict(oas.Inline(just_integer)))))])
  |> birdie.snap(title: "nested_dictionary_test")
}

pub fn object_and_additional_test() {
  schema([
    #(
      "Preference",
      oas.Object(
        dict.from_list([#("colour", oas.Inline(just_string))]),
        ["colour"],
        Some(oas.Inline(just_integer)),
        None,
        0,
        False,
        None,
        None,
        False,
      ),
    ),
  ])
  |> birdie.snap(title: "object_and_additional_test")
}

pub fn empty_object_is_dictionary_of_anything_test() {
  schema([#("Preference", object([], []))])
  |> birdie.snap(title: "empty_object_is_dictionary_of_anything_test")
}

pub fn allof_named_test() {
  schema([
    #("A", object([#("a", oas.Inline(just_string))], ["a"])),
    #("B", object([#("b", oas.Inline(just_string))], ["b"])),
    #("Both", oas.AllOf(NonEmptyList(ref("A"), [ref("B")]))),
  ])
  |> birdie.snap(title: "allof_named_test")
}

pub fn single_allof_test() {
  schema([#("Box", oas.AllOf(NonEmptyList(oas.Inline(just_string), [])))])
  |> birdie.snap(title: "single_allof_test")
}

pub fn unsupported_anyof_test() {
  schema([
    #(
      "Box",
      oas.AnyOf(
        NonEmptyList(oas.Inline(just_string), [oas.Inline(just_integer)]),
      ),
    ),
  ])
  |> birdie.snap(title: "unsupported_anyof_test")
}
