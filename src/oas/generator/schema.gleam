//// This module builds ASTs from schemas.
//// It does not do any file generation

import glance
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import oas/generator/ast
import oas/generator/lift
import oas/generator/lookup as l
import oas/json_schema

/// From a dictionary of schemas generate all custom types, type aliases, encoders and decoders. 
pub fn generate(schemas) {
  let #(internal, named) =
    list.map_fold(schemas |> dict.to_list, [], fn(acc, entry) {
      let #(name, schema) = entry
      let #(top, _nullable, acc) = lift.do_lift(json_schema.Inline(schema), acc)
      #(acc, #(name, top))
    })
  let named =
    list.append(
      named,
      internal
        |> list.reverse
        |> list.map(fn(entry) {
          let #(hash, fields) = entry
          #(lift.content_id_to_fn_prefix(hash), lift.Compound(fields))
        }),
    )
  l.fold(named, #([], [], []), fn(acc, entry) {
    let #(name, top) = entry
    let #(custom_types, type_aliases, fns) = acc
    use enc_fn <- l.then(to_encode_fn(#(name, top)))
    use dec_fn <- l.then(to_decode_fn(#(name, top)))
    let fns = list.append(fns, [enc_fn, dec_fn])
    let name = ast.name_for_gleam_type(name)
    use #(custom_types, type_aliases) <- l.then(case top {
      lift.Named(name) -> {
        use type_ <- l.then(to_type(lift.Named(name)))
        l.Done(#(custom_types, [alias(name, type_), ..type_aliases]))
      }
      lift.Primitive(primitive) -> {
        use type_ <- l.then(to_type(lift.Primitive(primitive)))
        l.Done(#(custom_types, [alias(name, type_), ..type_aliases]))
      }
      lift.Array(items) -> {
        use type_ <- l.then(to_type(lift.Array(items)))
        l.Done(#(custom_types, [alias(name, type_), ..type_aliases]))
      }
      lift.Tuple(items) -> {
        use type_ <- l.then(to_type(lift.Tuple(items)))
        l.Done(#(custom_types, [alias(name, type_), ..type_aliases]))
      }
      lift.Compound(lift.Fields(properties, additional, required)) -> {
        use type_ <- l.then(custom_type(name, properties, additional, required))
        l.Done(#([type_, ..custom_types], type_aliases))
      }
      lift.Dictionary(values) -> {
        use type_ <- l.then(to_type(lift.Dictionary(values)))
        l.Done(#(custom_types, [alias(name, type_), ..type_aliases]))
      }
      lift.Unsupported -> {
        use type_ <- l.then(to_type(lift.Unsupported))
        l.Done(#(custom_types, [alias(name, type_), ..type_aliases]))
      }
    })
    l.Done(#(custom_types, type_aliases, fns))
  })
}

/// Generate a custom type from an object type.
/// TODO make private
pub fn custom_type(name, properties, additional, required) {
  use fields <- l.then(
    l.seq(
      list.map(properties, fn(property) {
        let #(key, #(schema, nullable)) = property
        use type_ <- l.then(to_type(schema))

        glance.LabelledVariantField(
          case !list.contains(required, key) || nullable {
            False -> type_
            True -> glance.NamedType("Option", None, [type_])
          },
          ast.name_for_gleam_field_or_var(key),
        )
        |> l.Done
      }),
    ),
  )
  use fields <- l.then(case additional {
    Some(schema) -> {
      let key = "additionalProperties"
      use type_ <- l.then(to_type(schema))
      let extra =
        glance.LabelledVariantField(
          glance.NamedType("Dict", Some("dict"), [
            glance.NamedType("String", None, []),
            type_,
          ]),
          ast.name_for_gleam_field_or_var(key),
        )
      l.Done(list.append(fields, [extra]))
    }

    None -> l.Done(fields)
  })
  let name = ast.name_for_gleam_type(name)
  l.Done(
    glance.CustomType(name, glance.Public, False, [], [
      glance.Variant(name, fields),
    ]),
  )
}

/// Generate type aliases from OpenAPI lifted schemas.
/// All objects are converted to names
fn to_type(lifted) -> l.Lookup(glance.Type) {
  case lifted {
    lift.Named(ref) -> {
      use mod, name <- l.Lookup(ref)
      l.Done(glance.NamedType(ast.name_for_gleam_type(name), mod, []))
    }
    lift.Primitive(primitive) -> {
      case primitive {
        lift.Boolean -> glance.NamedType("Bool", None, [])
        lift.Integer -> glance.NamedType("Int", None, [])
        lift.Number -> glance.NamedType("Float", None, [])
        lift.String -> glance.NamedType("String", None, [])
        lift.Null -> glance.NamedType("Nil", None, [])
        lift.Always -> glance.NamedType("Any", Some("utils"), [])
        lift.Never -> glance.NamedType("Never", Some("utils"), [])
      }
      |> l.Done
    }
    lift.Array(items) -> {
      use items <- l.then(to_type(items))
      l.Done(glance.NamedType("List", None, [items]))
    }
    lift.Tuple(items) -> {
      use items <- l.then(l.seq(list.map(items, to_type)))
      l.Done(glance.TupleType(items))
    }
    lift.Compound(id) -> {
      let type_ = lift.content_id_to_type(id)
      l.Done(glance.NamedType(type_, None, []))
    }
    lift.Dictionary(values) -> {
      use values <- l.then(to_type(values))
      l.Done(
        glance.NamedType("Dict", Some("dict"), [
          glance.NamedType("String", None, []),
          values,
        ]),
      )
    }
    lift.Unsupported -> l.Done(glance.NamedType("Any", Some("utils"), []))
  }
}

/// This handles top to encoder
/// TODO make this private
pub fn to_encode_fn(entry) {
  let #(name, top) = entry
  let type_ = ast.name_for_gleam_type(name)
  do_to_encode_fn(name, type_, top)
}

pub fn do_to_encode_fn(name, type_, top) {
  let arg = glance.Variable("data")
  use exp <- l.then(case top {
    lift.Named(n) -> {
      use top <- l.then(to_encoder(lift.Named(n)))
      glance.Call(top, [glance.UnlabelledField(arg)]) |> l.Done
    }
    lift.Primitive(lift.Null) -> ast.call0("json", "null") |> l.Done
    lift.Primitive(lift.Never) ->
      glance.Panic(Some(glance.String("never value cannot be encoded")))
      |> l.Done
    lift.Primitive(p) -> {
      use top <- l.then(to_encoder(lift.Primitive(p)))
      glance.Call(top, [glance.UnlabelledField(arg)]) |> l.Done
    }
    lift.Array(items) -> {
      use items <- l.then(to_encoder(items))
      ast.call2("json", "array", arg, items) |> l.Done
    }
    lift.Tuple(items) -> encode_tuple_body(items, arg)
    lift.Compound(lift.Fields(properties, additional, required)) -> {
      fields_to_encode_body(properties, required, additional)
    }
    lift.Dictionary(values) -> {
      use values <- l.then(to_encoder(values))
      ast.call2("utils", "dict", arg, values) |> l.Done
    }
    lift.Unsupported -> {
      use top <- l.then(to_encoder(lift.Unsupported))
      glance.Call(top, [glance.UnlabelledField(arg)]) |> l.Done
    }
  })

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
  |> l.Done
}

pub fn fields_to_encode_body(properties, required, additional) {
  use additional <- l.then(case additional {
    Some(values) -> {
      use values <- l.then(to_encoder(values))
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
                glance.Call(values, [
                  glance.UnlabelledField(glance.Variable("value")),
                ]),
              ),
            ],
          ),
        ),
      ))
      |> l.Done
    }
    None -> l.Done(None)
  })

  use properties <- l.then(
    l.seq(
      list.map(properties, fn(property) {
        let #(key, #(schema, nullable)) = property
        let arg = ast.access("data", ast.name_for_gleam_field_or_var(key))

        use cast <- l.then(to_encoder(schema))
        let value = case !list.contains(required, key) || nullable {
          False -> glance.Call(cast, [glance.UnlabelledField(arg)])
          True -> ast.call2("json", "nullable", arg, cast)
        }
        l.Done(glance.Tuple([glance.String(key), value]))
      }),
    ),
  )
  ast.call1("utils", "object", glance.List(properties, additional))
  |> l.Done
}

/// This handles a lifted encoder
pub fn to_encoder(lifted: lift.Lifted) -> l.Lookup(glance.Expression) {
  case lifted {
    lift.Named(ref) -> {
      use mod, name <- l.Lookup(ref)
      let func = encode_fn(name)
      case mod {
        None -> glance.Variable(func)
        Some(module) -> ast.access(module, func)
      }
      |> l.Done
    }
    lift.Primitive(lift.Boolean) -> ast.access("json", "bool") |> l.Done
    lift.Primitive(lift.Integer) -> ast.access("json", "int") |> l.Done
    lift.Primitive(lift.Number) -> ast.access("json", "float") |> l.Done
    lift.Primitive(lift.String) -> ast.access("json", "string") |> l.Done
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
      |> l.Done
    lift.Primitive(lift.Always) -> ast.access("utils", "any_to_json") |> l.Done
    lift.Primitive(lift.Never) ->
      glance.Fn([glance.FnParameter(glance.Discarded("data"), None)], None, [
        glance.Expression(
          glance.Panic(Some(glance.String("never value cannot be encoded"))),
        ),
      ])
      |> l.Done
    lift.Array(items) -> {
      use items <- l.then(to_encoder(items))
      glance.FnCapture(None, ast.access("json", "array"), [], [
        glance.UnlabelledField(items),
      ])
      |> l.Done
    }
    lift.Tuple(items) -> {
      use exp <- l.then(encode_tuple_body(items, glance.Variable("data")))
      glance.Fn([glance.FnParameter(glance.Named("data"), None)], None, [
        glance.Expression(exp),
      ])
      |> l.Done
    }
    lift.Compound(some_name) ->
      glance.Variable(lift.content_id_to_fn_prefix(some_name) <> "_encode")
      |> l.Done
    lift.Dictionary(values) -> {
      use values <- l.then(to_encoder(values))
      glance.FnCapture(None, ast.access("utils", "dict"), [], [
        glance.UnlabelledField(values),
      ])
      |> l.Done
    }
    lift.Unsupported -> ast.access("utils", "any_to_json") |> l.Done
  }
}

fn encode_tuple_body(items, arg) {
  use items <- l.then(
    l.seq(
      list.index_map(items, fn(item, index) {
        use item <- l.then(to_encoder(item))
        glance.Call(item, [
          glance.UnlabelledField(glance.TupleIndex(arg, index)),
        ])
        |> l.Done
      }),
    ),
  )
  ast.call1("utils", "merge", glance.List(items, None)) |> l.Done
}

pub fn encode_fn(name) {
  ast.name_for_gleam_field_or_var(name <> "_encode")
}

// TODO make this private
pub fn to_decode_fn(entry) {
  let #(name, top) = entry

  use body <- l.then(gen_top_decoder_needs_name(name, top))
  glance.Function(
    name: decoder(name),
    publicity: glance.Public,
    parameters: [],
    return: None,
    body:,
    location: glance.Span(0, 0),
  )
  |> l.Done
}

// TODO make this private
pub fn gen_top_decoder_needs_name(name, top) {
  case top {
    lift.Named(n) -> {
      use exp <- l.then(to_decoder(lift.Named(n)))
      [glance.Expression(exp)] |> l.Done
    }
    lift.Primitive(p) -> {
      use exp <- l.then(to_decoder(lift.Primitive(p)))
      [glance.Expression(exp)] |> l.Done
    }
    lift.Array(items) -> {
      use items <- l.then(to_decoder(lift.Array(items)))
      [
        glance.Expression(items),
      ]
      |> l.Done
    }
    lift.Tuple(items) -> {
      use items <- l.then(to_decoder(lift.Tuple(items)))
      case items {
        glance.Block(statements) -> statements
        other -> [glance.Expression(other)]
      }
      |> l.Done
    }
    lift.Compound(lift.Fields(properties, additional, required)) -> {
      let type_ = ast.name_for_gleam_type(name)
      use zipped <- l.then(
        l.seq(
          list.map(properties, fn(property) {
            let #(key, #(schema, nullable)) = property
            let is_optional = !list.contains(required, key) || nullable
            use field_decoder <- l.then(to_decoder(schema))
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
            |> l.Done
          }),
        ),
      )
      use zipped <- l.then(case additional {
        Some(schema) -> {
          use schema <- l.then(to_decoder(schema))
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
                  schema,
                ),
              ),
              glance.LabelledField(
                ast.name_for_gleam_field_or_var(key),
                glance.Variable(ast.name_for_gleam_field_or_var(key)),
              ),
            ),
          ])
          |> l.Done
        }
        None -> zipped |> l.Done
      })
      let #(fields, cons) = list.unzip(zipped)
      let final =
        glance.Expression(
          ast.call1("decode", "success", case list.length(cons) {
            0 -> glance.Variable(type_)
            _ -> glance.Call(glance.Variable(type_), cons)
          }),
        )
      list.append(fields, [final]) |> l.Done
    }
    lift.Dictionary(values) -> {
      use exp <- l.then(to_decoder(lift.Dictionary(values)))
      [glance.Expression(exp)] |> l.Done
    }
    lift.Unsupported -> {
      use exp <- l.then(to_decoder(lift.Unsupported))
      [glance.Expression(exp)] |> l.Done
    }
  }
}

// TODO make private
pub fn to_decoder(lifted) -> l.Lookup(glance.Expression) {
  case lifted {
    lift.Named(ref) -> {
      use mod, name <- l.Lookup(ref)
      let func = ast.name_for_gleam_field_or_var(name <> "_decoder")
      case mod {
        None -> glance.Call(glance.Variable(func), [])
        Some(module) -> ast.call0(module, func)
      }
      |> l.Done
    }
    lift.Primitive(primitive) ->
      case primitive {
        lift.Boolean -> ast.access("decode", "bool")
        lift.Integer -> ast.access("decode", "int")
        lift.Number -> ast.access("decode", "float")
        lift.String -> ast.access("decode", "string")
        lift.Null -> always_decode()
        lift.Always -> ast.call0("utils", "any_decoder")
        lift.Never -> never_decode()
      }
      |> l.Done
    lift.Tuple(items) -> {
      use uses <- l.then(
        l.seq(
          list.index_map(items, fn(item, index) {
            use item <- l.then(to_decoder(item))
            glance.Use(
              [glance.PatternVariable("e" <> int.to_string(index))],
              ast.call1("decode", "then", item),
            )
            |> l.Done
          }),
        ),
      )
      let vars =
        list.index_map(items, fn(_, index) {
          glance.Variable("e" <> int.to_string(index))
        })

      glance.Block(
        list.append(uses, [
          glance.Expression(ast.call1("decode", "success", glance.Tuple(vars))),
        ]),
      )
      |> l.Done
    }
    lift.Array(items) -> {
      use items <- l.then(to_decoder(items))
      ast.call1("decode", "list", items) |> l.Done
    }
    lift.Compound(id) -> {
      let func = lift.content_id_to_fn_prefix(id) <> "_decoder"
      glance.Call(glance.Variable(func), []) |> l.Done
    }
    lift.Dictionary(values) -> {
      use values <- l.then(to_decoder(values))
      ast.call2("decode", "dict", ast.access("decode", "string"), values)
      |> l.Done
    }
    lift.Unsupported -> ast.call0("utils", "any_decoder") |> l.Done
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
