import birdie
import castor
import gleam/dict
import gleam/json
import gleam/option.{None, Some}
import gleeunit/should
import non_empty_list.{NonEmptyList}
import oas/generator as gen

const nullable_string = castor.String(
  None,
  None,
  None,
  None,
  True,
  None,
  None,
  False,
)

fn object(params, required) {
  castor.Object(
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
  castor.Object(dict.new(), [], Some(of), None, 0, False, None, None, False)
}

fn ref(thing) {
  castor.Ref("#/components/schemas/" <> thing, None, None)
}

fn schema(schemas) {
  gen.gen_schema_file(dict.from_list(schemas))
  |> gen.run_single_location("#/components/schemas/")
  |> should.be_ok
}

pub fn nil_alias_test() {
  schema([#("other_nil", castor.Null(None, None, False))])
  |> birdie.snap(title: "nil_alias_test")
}

// Simple things don't become new custom types
pub fn simple_string_schema_test() {
  schema([#("my_string", castor.string())])
  |> birdie.snap(title: "simple string schema test")
}

pub fn always_test() {
  schema([#("yes", castor.AlwaysPasses)])
  |> birdie.snap(title: "always_test")
}

pub fn never_test() {
  schema([#("yes", castor.AlwaysFails)])
  |> birdie.snap(title: "never_test")
}

// pub fn simple_nullable_schema_test() {
//   schema([#("my_string", castor.string())])
//   |> birdie.snap(title: "simple_nullable_schema_test")
// }

pub fn array_of_primitive_test() {
  schema([
    #("my_array", castor.array(castor.Inline(castor.integer()))),
  ])
  |> birdie.snap(title: "array_of_primitive_test")
}

pub fn nested_array_of_primitive_test() {
  schema([
    #(
      "my_array",
      castor.array(castor.Inline(castor.array(castor.Inline(castor.integer())))),
    ),
  ])
  |> birdie.snap(title: "nested_array_of_primitive_test")
}

pub fn array_of_references_test() {
  schema([#("my_array", castor.array(ref("totals")))])
  |> birdie.snap(title: "array_of_references_test")
}

pub fn inline_object_schema_test() {
  let parameters = [
    #("name", castor.Inline(castor.string())),
    #("age", castor.Inline(castor.integer())),
    #("active", castor.Inline(castor.Boolean(False, None, None, False))),
  ]
  schema([#("user", object(parameters, []))])
  |> birdie.snap(title: "inline_object_schema_test")
}

pub fn required_fields_test() {
  let parameters = [#("name", castor.Inline(castor.string()))]
  schema([#("user", object(parameters, ["name"]))])
  |> birdie.snap(title: "required_fields_test")
}

pub fn nullable_fields_test() {
  let parameters = [#("name", castor.Inline(nullable_string))]
  schema([#("user", object(parameters, ["name"]))])
  |> birdie.snap(title: "nullable_fields_test")
}

pub fn ref_object_schema_test() {
  schema([
    #("fullname", castor.string()),
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
            castor.Inline(
              object([#("fish", castor.Inline(castor.string()))], [
                "fish",
              ]),
            ),
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
            castor.Inline(
              object([#("fish", castor.Inline(castor.integer()))], [
                "fish",
              ]),
            ),
          ),
        ],
        ["little_bird"],
      ),
    ),
  ])
  |> birdie.snap(title: "nested_object_test")
}

pub fn empty_object_test() {
  let assert Ok(castor.Object(properties:, ..)) =
    json.parse("{\"type\":\"object\"}", castor.decoder())
  assert dict.is_empty(properties)

  schema([
    #("empty", object([], [])),
  ])
  |> birdie.snap(title: "empty_object_test")
}

pub fn duplicate_anon_object_test() {
  let anon =
    castor.Inline(
      object([#("flavour", castor.Inline(castor.string()))], [
        "flavour",
      ]),
    )
  schema([
    #(
      "big_box",
      object(
        [
          #("ingredients", castor.Inline(castor.array(anon))),
          #("favourite", anon),
        ],
        ["ingredients", "favourite"],
      ),
    ),
  ])
  |> birdie.snap(title: "duplicate_anon_object_test")
}

pub fn pure_dictionary_test() {
  schema([#("Bag", dict(castor.Inline(castor.integer())))])
  |> birdie.snap(title: "pure_dictionary_test")
}

pub fn nested_dictionary_test() {
  schema([
    #("Bag", dict(castor.Inline(dict(castor.Inline(castor.integer()))))),
  ])
  |> birdie.snap(title: "nested_dictionary_test")
}

pub fn object_and_additional_test() {
  schema([
    #(
      "Preference",
      castor.Object(
        dict.from_list([#("colour", castor.Inline(castor.string()))]),
        ["colour"],
        Some(castor.Inline(castor.integer())),
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

pub fn explicitly_no_additional_test() {
  schema([
    #(
      "Preference",
      castor.Object(
        dict.from_list([#("colour", castor.Inline(castor.string()))]),
        ["colour"],
        Some(castor.Inline(castor.AlwaysFails)),
        None,
        0,
        False,
        None,
        None,
        False,
      ),
    ),
  ])
  |> birdie.snap(title: "explicitly_no_additional_test")
}

pub fn empty_object_is_dictionary_of_anything_test() {
  // An empty object in a specification is considered a AlwayPass type
  let assert Ok(castor.AlwaysPasses) = json.parse("{}", castor.decoder())

  schema([#("Preference", castor.AlwaysPasses)])
  |> birdie.snap(title: "empty_object_is_dictionary_of_anything_test")
}

pub fn allof_named_test() {
  schema([
    #("A", object([#("a", castor.Inline(castor.string()))], ["a"])),
    #("B", object([#("b", castor.Inline(castor.string()))], ["b"])),
    #("Both", castor.AllOf(NonEmptyList(ref("A"), [ref("B")]))),
  ])
  |> birdie.snap(title: "allof_named_test")
}

pub fn single_allof_test() {
  schema([
    #("Box", castor.AllOf(NonEmptyList(castor.Inline(castor.string()), []))),
  ])
  |> birdie.snap(title: "single_allof_test")
}

pub fn unsupported_anyof_test() {
  schema([
    #(
      "Box",
      castor.AnyOf(
        NonEmptyList(castor.Inline(castor.string()), [
          castor.Inline(castor.integer()),
        ]),
      ),
    ),
  ])
  |> birdie.snap(title: "unsupported_anyof_test")
}

pub fn enum_of_strings_test() {
  schema([
    #("Box", castor.enum_of_strings(NonEmptyList("a", ["b", "c"]))),
  ])
  |> birdie.snap(title: "enum_of_strings_test")
}
