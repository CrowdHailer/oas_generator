//// This module builds ASTs from schemas.
//// It does not do any file generation

import glance
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import oas
import oas/generator/ast
import oas/generator/lift
import oas/generator/misc

/// From a dictionary of schemas generate all custom types, type aliases, encoders and decoders. 
pub fn generate(schemas) {
  let module = misc.Schema
  let #(internal, named) =
    list.map_fold(schemas |> dict.to_list, [], fn(acc, entry) {
      let #(name, schema) = entry
      let #(top, _nullable, acc) = lift.do_lift(oas.Inline(schema), acc)
      #(acc, #(name, top))
    })
  let named =
    list.append(
      named,
      internal
        |> list.reverse
        |> list.index_map(fn(fields, index) {
          #("internal_" <> int.to_string(index), lift.Compound(fields))
        }),
    )
  list.fold(named, #([], [], []), fn(acc, entry) {
    let #(name, top) = entry
    let #(custom_types, type_aliases, fns) = acc
    let fns =
      list.append(fns, [
        to_encode_fn(#(name, top), module),
        to_decode_fn(#(name, top), module),
      ])
    let name = ast.name_for_gleam_type(name)
    let #(custom_types, type_aliases) = case top {
      lift.Named(name) -> {
        let type_ = to_type(lift.Named(name), module)
        #(custom_types, [alias(name, type_), ..type_aliases])
      }
      lift.Primitive(primitive) -> {
        let type_ = to_type(lift.Primitive(primitive), module)
        #(custom_types, [alias(name, type_), ..type_aliases])
      }
      lift.Array(items) -> {
        let type_ = to_type(lift.Array(items), module)
        #(custom_types, [alias(name, type_), ..type_aliases])
      }
      lift.Tuple(items) -> {
        let type_ = to_type(lift.Tuple(items), module)
        #(custom_types, [alias(name, type_), ..type_aliases])
      }
      lift.Compound(lift.Fields(properties, additional, required)) -> {
        let type_ = custom_type(name, properties, additional, required, module)
        #([type_, ..custom_types], type_aliases)
      }
      lift.Dictionary(values) -> {
        let type_ = to_type(lift.Dictionary(values), module)
        #(custom_types, [alias(name, type_), ..type_aliases])
      }
      lift.Unsupported -> {
        let type_ = to_type(lift.Unsupported, module)
        #(custom_types, [alias(name, type_), ..type_aliases])
      }
    }
    #(custom_types, type_aliases, fns)
  })
}

/// Generate a custom type from an object type.
/// TODO make private
pub fn custom_type(name, properties, additional, required, module) {
  let fields =
    list.map(properties, fn(property) {
      let #(key, #(schema, nullable)) = property
      let type_ = to_type(schema, module)

      glance.LabelledVariantField(
        case !list.contains(required, key) || nullable {
          False -> type_
          True -> glance.NamedType("Option", None, [type_])
        },
        ast.name_for_gleam_field_or_var(key),
      )
    })
  let fields = case additional {
    Some(schema) -> {
      let key = "additionalProperties"
      let type_ = to_type(schema, module)
      let extra =
        glance.LabelledVariantField(
          glance.NamedType("Dict", Some("dict"), [
            glance.NamedType("String", None, []),
            type_,
          ]),
          ast.name_for_gleam_field_or_var(key),
        )
      list.append(fields, [extra])
    }

    None -> fields
  }
  let name = ast.name_for_gleam_type(name)
  glance.CustomType(name, glance.Public, False, [], [
    glance.Variant(name, fields),
  ])
}

/// Generate type aliases from OpenAPI lifted schemas.
/// All objects are converted to names
fn to_type(lifted, module) {
  case lifted {
    lift.Named("#/components/schemas/" <> inner) -> {
      let mod = case module {
        misc.Schema -> None
        _ -> Some("schema")
      }
      glance.NamedType(ast.name_for_gleam_type(inner), mod, [])
    }
    lift.Named(ref) ->
      panic as { "not referencing schema component ref: " <> ref }
    lift.Primitive(primitive) -> {
      case primitive {
        lift.Boolean -> glance.NamedType("Bool", None, [])
        lift.Integer -> glance.NamedType("Int", None, [])
        lift.Number -> glance.NamedType("Float", None, [])
        lift.String -> glance.NamedType("String", None, [])
        lift.Null -> glance.NamedType("Nil", None, [])
        lift.Always -> glance.NamedType("Json", Some("json"), [])
        lift.Never -> glance.NamedType("Never", Some("utils"), [])
      }
    }
    lift.Array(items) ->
      glance.NamedType("List", None, [to_type(items, module)])
    lift.Tuple(items) -> glance.TupleType(list.map(items, to_type(_, module)))
    lift.Compound(index) -> {
      let type_ = "Internal" <> int.to_string(index)
      glance.NamedType(type_, None, [])
    }
    lift.Dictionary(values) ->
      glance.NamedType("Dict", Some("dict"), [
        glance.NamedType("String", None, []),
        to_type(values, module),
      ])
    lift.Unsupported -> glance.NamedType("Json", Some("json"), [])
  }
}

/// This handles top to encoder
/// TODO make this private
pub fn to_encode_fn(entry, module) {
  let #(name, top) = entry
  let type_ = ast.name_for_gleam_type(name)

  let arg = glance.Variable("data")
  let exp = case top {
    lift.Named(n) ->
      to_encoder(lift.Named(n), module)
      |> glance.Call([glance.UnlabelledField(arg)])
    lift.Primitive(lift.Null) -> ast.call0("json", "null")
    lift.Primitive(lift.Always) -> arg
    lift.Primitive(lift.Never) ->
      glance.Panic(Some(glance.String("never value cannot be encoded")))
    lift.Primitive(p) ->
      to_encoder(lift.Primitive(p), module)
      |> glance.Call([glance.UnlabelledField(arg)])
    lift.Array(items) -> {
      ast.call2("json", "array", arg, to_encoder(items, module))
    }
    lift.Tuple(items) -> encode_tuple_body(items, arg, module)
    lift.Compound(lift.Fields(properties, additional, required)) -> {
      fields_to_encode_body(properties, required, additional, module)
    }
    lift.Dictionary(values) ->
      ast.call2("utils", "dict", arg, to_encoder(values, module))
    lift.Unsupported -> arg
  }

  let ignored = case top {
    lift.Primitive(lift.Null) | lift.Primitive(lift.Never) -> True
    lift.Compound(lift.Fields(properties, None, _)) -> list.is_empty(properties)
    _ -> False
  }
  let assignment = case ignored {
    True -> glance.Discarded("data")
    False -> glance.Named("data")
  }

  glance.Function(
    name: encode_fn(name),
    publicity: glance.Public,
    parameters: [
      glance.FunctionParameter(
        None,
        assignment,
        // could annotate with type returned from to custom type
        Some(glance.NamedType(type_, None, [])),
      ),
    ],
    return: None,
    body: [glance.Expression(exp)],
    location: glance.Span(0, 0),
  )
}

pub fn fields_to_encode_body(properties, required, additional, module) {
  ast.call1(
    "utils",
    "object",
    glance.List(
      list.map(properties, fn(property) {
        let #(key, #(schema, nullable)) = property
        let arg = ast.access("data", ast.name_for_gleam_field_or_var(key))

        let cast = to_encoder(schema, module)
        let value = case !list.contains(required, key) || nullable {
          False -> glance.Call(cast, [glance.UnlabelledField(arg)])
          True -> ast.call2("json", "nullable", arg, cast)
        }
        glance.Tuple([glance.String(key), value])
      }),
      case additional {
        Some(values) ->
          Some(ast.call1(
            "dict",
            "to_list",
            ast.call2(
              "dict",
              "map_values",
              ast.access("data", "additional_properties"),
              glance.Fn(
                [
                  glance.FnParameter(glance.Discarded("key"), None),
                  glance.FnParameter(glance.Named("value"), None),
                ],
                None,
                [
                  glance.Expression(
                    glance.Call(to_encoder(values, module), [
                      glance.UnlabelledField(glance.Variable("value")),
                    ]),
                  ),
                ],
              ),
            ),
          ))
        None -> None
      },
    ),
  )
}

/// This handles a lifted encoder
pub fn to_encoder(lifted, module) {
  case lifted {
    lift.Named("#/components/schemas/" <> inner) -> {
      let func = encode_fn(inner)
      case module {
        misc.Schema -> glance.Variable(func)
        _ -> ast.access("schema", func)
      }
    }
    lift.Named(..) -> panic as "unexpected ref"
    lift.Primitive(lift.Boolean) -> ast.access("json", "bool")
    lift.Primitive(lift.Integer) -> ast.access("json", "int")
    lift.Primitive(lift.Number) -> ast.access("json", "float")
    lift.Primitive(lift.String) -> ast.access("json", "string")
    lift.Primitive(lift.Null) ->
      glance.Fn(
        [
          glance.FnParameter(
            glance.Discarded(""),
            Some(glance.NamedType("Nil", None, [])),
          ),
        ],
        None,
        [glance.Expression(ast.call0("json", "null"))],
      )
    lift.Primitive(lift.Always) ->
      glance.Fn([glance.FnParameter(glance.Named("data"), None)], None, [
        glance.Expression(glance.Variable("data")),
      ])
    lift.Primitive(lift.Never) ->
      glance.Fn([glance.FnParameter(glance.Discarded("data"), None)], None, [
        glance.Expression(
          glance.Panic(Some(glance.String("never value cannot be encoded"))),
        ),
      ])
    lift.Array(items) -> {
      glance.FnCapture(None, ast.access("json", "array"), [], [
        glance.UnlabelledField(to_encoder(items, module)),
      ])
    }
    lift.Tuple(items) ->
      glance.Fn([glance.FnParameter(glance.Named("data"), None)], None, [
        glance.Expression(encode_tuple_body(
          items,
          glance.Variable("data"),
          module,
        )),
      ])
    lift.Compound(index) ->
      glance.Variable(encode_fn("internal_" <> int.to_string(index)))
    lift.Dictionary(values) ->
      glance.FnCapture(None, ast.access("utils", "dict"), [], [
        glance.UnlabelledField(to_encoder(values, module)),
      ])
    lift.Unsupported ->
      glance.Fn([glance.FnParameter(glance.Named("data"), None)], None, [
        glance.Expression(glance.Variable("data")),
      ])
  }
}

fn encode_tuple_body(items, arg, module) {
  ast.call1(
    "utils",
    "merge",
    glance.List(
      list.index_map(items, fn(item, index) {
        glance.Call(to_encoder(item, module), [
          glance.UnlabelledField(glance.TupleIndex(arg, index)),
        ])
      }),
      None,
    ),
  )
}

pub fn encode_fn(name) {
  ast.name_for_gleam_field_or_var(name <> "_encode")
}

// TODO make this private
pub fn to_decode_fn(entry, module) {
  let #(name, top) = entry

  let body = gen_top_decoder_needs_name(name, top, module)
  glance.Function(
    name: decoder(name),
    publicity: glance.Public,
    parameters: [],
    return: None,
    body:,
    location: glance.Span(0, 0),
  )
}

// TODO make this private
pub fn gen_top_decoder_needs_name(name, top, module) {
  case top {
    lift.Named(n) -> [glance.Expression(to_decoder(lift.Named(n), module))]
    lift.Primitive(p) -> [
      glance.Expression(to_decoder(lift.Primitive(p), module)),
    ]
    lift.Array(items) -> [
      glance.Expression(to_decoder(lift.Array(items), module)),
    ]
    lift.Tuple(items) ->
      case to_decoder(lift.Tuple(items), module) {
        glance.Block(statements) -> statements
        other -> [glance.Expression(other)]
      }
    lift.Compound(lift.Fields(properties, additional, required)) -> {
      let type_ = ast.name_for_gleam_type(name)
      let zipped =
        list.map(properties, fn(property) {
          let #(key, #(schema, nullable)) = property
          let is_optional = !list.contains(required, key) || nullable
          let field_decoder = to_decoder(schema, module)
          #(
            glance.Use(
              [glance.PatternVariable(ast.name_for_gleam_field_or_var(key))],
              case is_optional {
                False ->
                  ast.call2(
                    "decode",
                    "field",
                    glance.String(key),
                    field_decoder,
                  )
                True ->
                  glance.Call(ast.access("decode", "optional_field"), [
                    glance.UnlabelledField(glance.String(key)),
                    glance.UnlabelledField(glance.Variable("None")),
                    glance.UnlabelledField(ast.call1(
                      "decode",
                      "optional",
                      field_decoder,
                    )),
                  ])
              },
            ),
            glance.LabelledField(
              ast.name_for_gleam_field_or_var(key),
              glance.Variable(ast.name_for_gleam_field_or_var(key)),
            ),
          )
        })
      let zipped = case additional {
        Some(schema) -> {
          let key = "additionalProperties"
          list.append(zipped, [
            #(
              glance.Use(
                [glance.PatternVariable(ast.name_for_gleam_field_or_var(key))],
                ast.call2(
                  "utils",
                  "decode_additional",
                  glance.List(
                    list.map(properties, fn(p) {
                      let #(k, _) = p
                      glance.String(k)
                    }),
                    None,
                  ),
                  to_decoder(schema, module),
                ),
              ),
              glance.LabelledField(
                ast.name_for_gleam_field_or_var(key),
                glance.Variable(ast.name_for_gleam_field_or_var(key)),
              ),
            ),
          ])
        }
        None -> zipped
      }
      let #(fields, cons) = list.unzip(zipped)
      let final =
        glance.Expression(
          ast.call1("decode", "success", case list.length(cons) {
            0 -> glance.Variable(type_)
            _ -> glance.Call(glance.Variable(type_), cons)
          }),
        )
      list.append(fields, [final])
    }
    lift.Dictionary(values) -> [
      glance.Expression(to_decoder(lift.Dictionary(values), module)),
    ]
    lift.Unsupported -> [
      glance.Expression(to_decoder(lift.Unsupported, module)),
    ]
  }
}

// TODO make private
pub fn to_decoder(lifted, module) {
  case lifted {
    lift.Named("#/components/schemas/" <> name) -> {
      let func = ast.name_for_gleam_field_or_var(name <> "_decoder")
      case module {
        misc.Schema -> glance.Call(glance.Variable(func), [])
        _ -> ast.call0("schema", func)
      }
    }
    lift.Named(ref) ->
      panic as { "not referencing schema component ref: " <> ref }
    lift.Primitive(primitive) ->
      case primitive {
        lift.Boolean -> ast.access("decode", "bool")
        lift.Integer -> ast.access("decode", "int")
        lift.Number -> ast.access("decode", "float")
        lift.String -> ast.access("decode", "string")
        lift.Null -> always_decode()
        lift.Always -> ast.call0("utils", "dynamic_to_json")
        lift.Never -> never_decode()
      }
    lift.Tuple(items) -> {
      let uses =
        list.index_map(items, fn(item, index) {
          glance.Use(
            [glance.PatternVariable("e" <> int.to_string(index))],
            ast.call1("decode", "then", to_decoder(item, module)),
          )
        })
      let vars =
        list.index_map(items, fn(_, index) {
          glance.Variable("e" <> int.to_string(index))
        })

      glance.Block(
        list.append(uses, [
          glance.Expression(ast.call1("decode", "success", glance.Tuple(vars))),
        ]),
      )
    }
    lift.Array(items) -> ast.call1("decode", "list", to_decoder(items, module))
    lift.Compound(index) -> {
      let func = "internal_" <> int.to_string(index) <> "_decoder"
      glance.Call(glance.Variable(func), [])
    }
    lift.Dictionary(values) ->
      ast.call2(
        "decode",
        "dict",
        ast.access("decode", "string"),
        to_decoder(values, module),
      )
    lift.Unsupported -> ast.call0("utils", "dynamic_to_json")
  }
}

fn decoder(name) {
  ast.name_for_gleam_field_or_var(name <> "_decoder")
}

/// Not used anywhere else
fn alias(to, type_) {
  glance.TypeAlias(to, glance.Public, [], type_)
}

fn always_decode() {
  ast.call2(
    "decode",
    "new_primitive_decoder",
    glance.String("Nil"),
    glance.Fn([glance.FnParameter(glance.Discarded(""), None)], None, [
      glance.Expression(
        glance.Call(glance.Variable("Ok"), [
          glance.UnlabelledField(glance.Variable("Nil")),
        ]),
      ),
    ]),
  )
}

fn never_decode() {
  ast.call2(
    "decode",
    "new_primitive_decoder",
    glance.String("Never"),
    glance.Fn([glance.FnParameter(glance.Discarded(""), None)], None, [
      glance.Expression(
        glance.Panic(
          Some(glance.String("tried to decode a never decode value")),
        ),
      ),
    ]),
  )
}
