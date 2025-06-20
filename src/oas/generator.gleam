import glance
import glance_printer
import gleam/bit_array
import gleam/dict
import gleam/dynamic/decode
import gleam/http
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result.{try}
import gleam/string
import justin
import oas
import oas/generator/lift
import simplifile
import snag

pub type Module {
  Schema
  Operations
}

fn concat_path(match, root) {
  let #(str, rev) =
    list.fold(match, #(root, []), fn(acc, segment) {
      let #(current, prev) = acc
      case segment {
        oas.FixedSegment(s) -> #(current <> "/" <> s, prev)
        oas.MatchSegment(name, schema) -> {
          case schema {
            oas.String(..) -> {
              let prev = [glance.String(current <> "/"), ..prev]
              let prev = [
                glance.Variable(name_for_gleam_field_or_var(name)),
                ..prev
              ]
              #("", prev)
            }
            oas.Integer(..) -> {
              let prev = [glance.String(current <> "/"), ..prev]
              let prev = [
                call1(
                  "int",
                  "to_string",
                  glance.Variable(name_for_gleam_field_or_var(name)),
                ),
                ..prev
              ]
              #("", prev)
            }
            _ -> {
              let prev = [glance.String(current <> "/"), ..prev]
              let prev = [
                glance.Variable(name_for_gleam_field_or_var(name)),
                ..prev
              ]
              #("", prev)
            }
          }
        }
      }
    })
  let parts = case str {
    "" -> rev
    _ -> [glance.String(str), ..rev]
  }
  let assert Ok(a) =
    list.reduce(parts, fn(post, pre) {
      glance.BinaryOperator(glance.Concatenate, pre, post)
    })
  a
}

fn alias(to, type_) {
  glance.TypeAlias(to, glance.Public, [], type_)
}

fn encode_fn(name) {
  name_for_gleam_field_or_var(name <> "_encode")
}

fn noop1(message) {
  glance.Fn([glance.FnParameter(glance.Discarded(""), None)], None, [
    glance.Expression(glance.Panic(Some(glance.String(message)))),
  ])
}

fn to_encoder(lifted) {
  case lifted {
    lift.Named("#/components/schemas/" <> inner) ->
      glance.Variable(encode_fn(inner))
    lift.Named(..) -> panic as "unexpected ref"
    lift.Primitive(lift.Boolean) -> access("json", "bool")
    lift.Primitive(lift.Integer) -> access("json", "int")
    lift.Primitive(lift.Number) -> access("json", "float")
    lift.Primitive(lift.String) -> access("json", "string")
    lift.Primitive(lift.Null) ->
      glance.Fn(
        [
          glance.FnParameter(
            glance.Discarded(""),
            Some(glance.NamedType("Nil", None, [])),
          ),
        ],
        None,
        [glance.Expression(call0("json", "null"))],
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
      call2("json", "array", glance.Variable("_"), to_encoder(items))
    }
    lift.Tuple(items) ->
      glance.Fn([glance.FnParameter(glance.Named("data"), None)], None, [
        glance.Expression(encode_tuple_body(items, glance.Variable("data"))),
      ])
    lift.Compound(index) ->
      glance.Variable(encode_fn("internal_" <> int.to_string(index)))
    lift.Dictionary(values) ->
      call2("utils", "dict", glance.Variable("_"), to_encoder(values))
    lift.Unsupported ->
      glance.Fn([glance.FnParameter(glance.Named("data"), None)], None, [
        glance.Expression(glance.Variable("data")),
      ])
  }
}

fn schema_to_encoder(entry) {
  let #(name, top) = entry
  let type_ = name_for_gleam_type(name)

  let arg = glance.Variable("data")
  let exp = case top {
    lift.Named(n) ->
      to_encoder(lift.Named(n)) |> glance.Call([glance.UnlabelledField(arg)])
    lift.Primitive(lift.Null) -> call0("json", "null")
    lift.Primitive(lift.Always) -> arg
    lift.Primitive(lift.Never) ->
      glance.Panic(Some(glance.String("never value cannot be encoded")))
    lift.Primitive(p) ->
      to_encoder(lift.Primitive(p))
      |> glance.Call([glance.UnlabelledField(arg)])
    lift.Array(items) -> {
      call2("json", "array", arg, to_encoder(items))
    }
    lift.Tuple(items) -> encode_tuple_body(items, arg)
    lift.Compound(lift.Fields(properties, additional, required)) -> {
      call1(
        "json",
        "object",
        glance.List(
          list.map(properties, fn(property) {
            let #(key, #(schema, nullable)) = property
            let arg = access("data", name_for_gleam_field_or_var(key))

            let cast = to_encoder(schema)
            let value = case !list.contains(required, key) || nullable {
              False -> glance.Call(cast, [glance.UnlabelledField(arg)])
              True -> call2("json", "nullable", arg, cast)
            }
            glance.Tuple([glance.String(key), value])
          }),
          case additional {
            Some(values) ->
              Some(call1(
                "dict",
                "to_list",
                call2(
                  "dict",
                  "map_values",
                  access("data", "additional_properties"),
                  glance.Fn(
                    [
                      glance.FnParameter(glance.Discarded("key"), None),
                      glance.FnParameter(glance.Named("value"), None),
                    ],
                    None,
                    [
                      glance.Expression(
                        glance.Call(to_encoder(values), [
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
    lift.Dictionary(values) -> call2("utils", "dict", arg, to_encoder(values))
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

fn encode_tuple_body(items, arg) {
  call1(
    "utils",
    "merge",
    glance.List(
      list.index_map(items, fn(item, index) {
        glance.Call(to_encoder(item), [
          glance.UnlabelledField(glance.TupleIndex(arg, index)),
        ])
      }),
      None,
    ),
  )
}

fn decoder(name) {
  name_for_gleam_field_or_var(name <> "_decoder")
}

fn always_decode() {
  call2(
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
  call2(
    "decode",
    "new_primitive_decoder",
    glance.String("Never"),
    glance.Fn([glance.FnParameter(glance.Discarded(""), None)], None, [
      glance.Expression(
        glance.Call(glance.Variable("Error"), [
          glance.UnlabelledField(glance.Variable("Nil")),
        ]),
      ),
    ]),
  )
}

fn dynamic_decode() {
  call2(
    "decode",
    "new_primitive_decoder",
    glance.String("Dynamic"),
    glance.Fn([glance.FnParameter(glance.Named("raw"), None)], None, [
      glance.Expression(
        glance.Call(glance.Variable("Ok"), [
          glance.UnlabelledField(glance.Variable("raw")),
        ]),
      ),
    ]),
  )
}

fn schema_to_decode_fn(entry) {
  let #(name, top) = entry

  let body = gen_top_decoder_needs_name(name, top, Schema)
  glance.Function(
    name: decoder(name),
    publicity: glance.Public,
    parameters: [],
    return: None,
    body:,
    location: glance.Span(0, 0),
  )
}

fn gen_top_decoder_needs_name(name, top, module) {
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
      let type_ = name_for_gleam_type(name)
      let zipped =
        list.map(properties, fn(property) {
          let #(key, #(schema, nullable)) = property
          let is_optional = !list.contains(required, key) || nullable
          let field_decoder = to_decoder(schema, module)
          #(
            glance.Use(
              [glance.PatternVariable(name_for_gleam_field_or_var(key))],
              case is_optional {
                False ->
                  call2("decode", "field", glance.String(key), field_decoder)
                True ->
                  glance.Call(access("decode", "optional_field"), [
                    glance.UnlabelledField(glance.String(key)),
                    glance.UnlabelledField(glance.Variable("None")),
                    glance.UnlabelledField(call1(
                      "decode",
                      "optional",
                      field_decoder,
                    )),
                  ])
              },
            ),
            glance.LabelledField(
              name_for_gleam_field_or_var(key),
              glance.Variable(name_for_gleam_field_or_var(key)),
            ),
          )
        })
      let zipped = case additional {
        Some(schema) -> {
          let key = "additionalProperties"
          list.append(zipped, [
            #(
              glance.Use(
                [glance.PatternVariable(name_for_gleam_field_or_var(key))],
                call2(
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
                name_for_gleam_field_or_var(key),
                glance.Variable(name_for_gleam_field_or_var(key)),
              ),
            ),
          ])
        }
        None -> zipped
      }
      let #(fields, cons) = list.unzip(zipped)
      let final =
        glance.Expression(
          call1("decode", "success", case list.length(cons) {
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

fn gen_schema(schemas) {
  let module = Schema
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
        schema_to_encoder(#(name, top)),
        schema_to_decode_fn(#(name, top)),
      ])
    let name = name_for_gleam_type(name)
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

fn custom_type(name, properties, additional, required, module) {
  let fields =
    list.map(properties, fn(property) {
      let #(key, #(schema, nullable)) = property
      let type_ = to_type(schema, module)

      glance.LabelledVariantField(
        case !list.contains(required, key) || nullable {
          False -> type_
          True -> glance.NamedType("Option", None, [type_])
        },
        name_for_gleam_field_or_var(key),
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
          name_for_gleam_field_or_var(key),
        )
      list.append(fields, [extra])
    }

    None -> fields
  }
  let name = name_for_gleam_type(name)
  glance.CustomType(name, glance.Public, False, [], [
    glance.Variant(name, fields),
  ])
}

fn to_type(lifted, module) {
  case lifted {
    lift.Named("#/components/schemas/" <> inner) -> {
      let mod = case module {
        Schema -> None
        _ -> Some("schema")
      }
      glance.NamedType(name_for_gleam_type(inner), mod, [])
    }
    lift.Named(ref) ->
      panic as { "not referencing schema component ref: " <> ref }
    lift.Primitive(primitive) -> {
      case primitive {
        lift.Boolean -> glance.NamedType("Bool", None, [])
        lift.Integer -> glance.NamedType("Int", None, [])
        lift.Number -> glance.NamedType("Float", None, [])
        lift.String -> glance.NamedType("String", None, [])
        lift.Null -> glance.NamedType("Null", None, [])
        lift.Always -> glance.NamedType("Dynamic", Some("dynamic"), [])
        lift.Never -> glance.NamedType("Never", None, [])
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
    lift.Unsupported -> glance.NamedType("Dynamic", Some("dynamic"), [])
  }
}

fn access(object_or_mod, field) {
  glance.FieldAccess(glance.Variable(object_or_mod), field)
}

fn call0(m, f) {
  glance.Call(access(m, f), [])
}

fn call1(m, f, a) {
  glance.Call(access(m, f), [glance.UnlabelledField(a)])
}

fn call2(m, f, a, b) {
  glance.Call(access(m, f), [
    glance.UnlabelledField(a),
    glance.UnlabelledField(b),
  ])
}

fn pipe(a, b) {
  glance.BinaryOperator(glance.Pipe, a, b)
}

fn query_to_parts(parameters, components: oas.Components) {
  list.map(parameters, fn(p) {
    let #(key, required, schema) = p
    let schema = oas.fetch_schema(schema, components.schemas)
    let key = name_for_gleam_field_or_var(key)
    let arg = glance.FunctionParameter(Some(key), glance.Named(key), None)
    let var = glance.Variable(key)

    let mapper = case schema {
      oas.Boolean(..) -> Some(access("bool", "to_string"))
      oas.Integer(..) -> Some(access("int", "to_string"))
      oas.Number(..) -> Some(access("float", "to_string"))
      oas.String(..) -> None
      _ -> Some(noop1("query parameter is not supported"))
    }
    let value = case required {
      True ->
        call1("option", "Some", case mapper {
          Some(m) -> glance.Call(m, [glance.UnlabelledField(var)])
          None -> var
        })
      False ->
        case mapper {
          Some(mapper) -> call2("option", "map", var, mapper)
          None -> var
        }
    }
    let tuple = glance.Tuple([glance.String(key), value])
    #(arg, tuple)
  })
  |> list.unzip
}

fn gen_method(method) {
  case method {
    http.Get -> access("http", "Get")
    http.Post -> access("http", "Post")
    http.Head -> access("http", "Head")
    http.Put -> access("http", "Put")
    http.Delete -> access("http", "Delete")
    http.Trace -> access("http", "Trace")
    http.Connect -> access("http", "Connect")
    http.Options -> access("http", "Options")
    http.Patch -> access("http", "Patch")
    http.Other(other) -> call1("http", "Other", glance.String(other))
  }
}

fn let_(var, value) {
  glance.Assignment(glance.Let, glance.PatternVariable(var), None, value)
}

fn path_args(match) {
  list.filter_map(match, fn(segment) {
    case segment {
      oas.FixedSegment(_) -> Error(Nil)
      oas.MatchSegment(name, _schema) ->
        Ok(glance.FunctionParameter(
          None,
          glance.Named(name_for_gleam_field_or_var(name)),
          None,
        ))
    }
  })
}

fn get_structure_json_media(content: dict.Dict(String, oas.MediaType)) {
  dict.fold(content, #([], []), fn(acc, key, value) {
    let #(known, unknown) = acc
    case key {
      "application/json" -> #([#(None, value), ..known], unknown)
      "application/" <> rest ->
        case string.split_once(rest, "+json") {
          Ok(#(pre, "")) -> #([#(Some(pre), value), ..known], unknown)
          _ -> #(known, [#(key, value), ..unknown])
        }
      _ -> #(known, [#(key, value), ..unknown])
    }
  })
}

fn gen_request_for_op(
  op_entry,
  pattern,
  path_parameters,
  components: oas.Components,
) {
  let #(method, op) = op_entry
  let oas.Operation(operation_id: id, parameters: op_parameters, ..) = op
  let id = name_for_gleam_field_or_var(id)

  let parameters = list.append(op_parameters, path_parameters)
  let parameters =
    list.map(parameters, oas.fetch_parameter(_, components.parameters))
  let #(q_args, q_params) =
    query_to_parts(oas.query_parameters(parameters), components)

  let match = case oas.gather_match(pattern, parameters, components) {
    Ok(match) -> match
    Error(_reason) -> {
      //  #(reason, pattern, parameters)
      panic as "could not find all parameters in match"
    }
  }

  let body = case op.request_body {
    Some(body) -> {
      let oas.RequestBody(content: content, ..) =
        oas.fetch_request_body(body, components.request_bodies)
      let #(known, unknown) = get_structure_json_media(content)
      case known, unknown {
        [#(_, oas.MediaType(schema))], _ ->
          case schema {
            oas.Ref(ref: "#/components/schemas/" <> name, ..) -> {
              let arg = name_for_gleam_field_or_var(name)
              let encode =
                call1(
                  "utils",
                  "json_to_bits",
                  call1("schema", encode_fn(name), glance.Variable(arg)),
                )
              Some(#(arg, encode))
            }
            _ -> {
              Some(#("data", glance.Variable("data")))
            }
          }
        // No content
        [], [] -> None
        [], [#(unknown, _)] -> {
          io.println("unknown content type: " <> unknown)
          None
        }
        _, _ -> {
          io.println("multiple content types not supported")
          None
        }
      }
    }
    None -> None
  }
  let parameters =
    path_args(match)
    |> list.append(case body {
      Some(#(arg, _)) -> [
        glance.FunctionParameter(None, glance.Named(arg), None),
      ]
      None -> []
    })
    |> list.append(q_args)

  let op_request = id <> "_request"
  let op_response = id <> "_response"

  let req_fn =
    glance.Function(
      name: op_request,
      publicity: glance.Public,
      parameters: [
        glance.FunctionParameter(None, glance.Named("base"), None),
        ..parameters
      ],
      return: None,
      body: [
        let_("method", gen_method(method)),
        let_("path", concat_path(match, "")),
        let_("query", glance.List(q_params, None)),
        ..case body {
          Some(#(_arg, encode)) -> [
            let_("body", encode),
            glance.Expression(pipe(
              glance.Variable("base"),
              pipe(
                call1("utils", "set_method", glance.Variable("method")),
                pipe(
                  call1("utils", "append_path", glance.Variable("path")),
                  pipe(
                    call1("utils", "set_query", glance.Variable("query")),
                    call2(
                      "utils",
                      "set_body",
                      glance.String("application/json"),
                      glance.Variable("body"),
                    ),
                  ),
                ),
              ),
            )),
          ]
          None -> [
            glance.Expression(pipe(
              glance.Variable("base"),
              pipe(
                call1("utils", "set_method", glance.Variable("method")),
                pipe(
                  call1("utils", "append_path", glance.Variable("path")),
                  call1("utils", "set_query", glance.Variable("query")),
                ),
              ),
            )),
          ]
        }
      ],
      location: glance.Span(0, 0),
    )
  let fn_ =
    glance.Function(
      name: id,
      publicity: glance.Public,
      parameters: [
        glance.FunctionParameter(None, glance.Named("token"), None),
        ..parameters
      ],
      return: None,
      body: [
        let_(
          "request",
          glance.Call(glance.Variable("base_request"), [
            glance.UnlabelledField(glance.Variable("token")),
          ]),
        ),
        let_(
          "request",
          glance.Call(access("operations", op_request), [
            glance.UnlabelledField(glance.Variable("request")),
            ..list.map(parameters, fn(p) {
              let assert glance.FunctionParameter(name: glance.Named(name), ..) =
                p
              glance.UnlabelledField(glance.Variable(name))
            })
          ]),
        ),
        glance.Use(
          [glance.PatternVariable("response")],
          call1("t", "do", call1("t", "fetch", glance.Variable("request"))),
        ),
        glance.Use(
          [glance.PatternVariable("data")],
          call1(
            "t",
            "try",
            glance.Call(glance.Variable("handle_errors"), [
              glance.UnlabelledField(call1(
                "operations",
                op_response,
                glance.Variable("response"),
              )),
            ]),
          ),
        ),
        glance.Expression(call1("t", "Done", glance.Variable("data"))),
      ],
      location: glance.Span(0, 0),
    )
  #(fn_, req_fn)
}

fn replace_gleam_keywords(key) {
  case key {
    "type" -> "type_"
    "auto" -> "auto_"
    "import" -> "import_"
    // used in places
    "base" -> "base_"
    "path" -> "path_"
    "method" -> "method_"
    "token" -> "token_"
    _ -> key
  }
  // case key {
  //   "_" <> key -> key
  //   _ -> key
  // }
  // // github is weird
}

fn replace_disallowed_charachters(in) {
  in
  |> string.replace("/", "_")
  |> string.replace("+", "_")
  // this is part of kebab casing
  // |> string.replace("-", "Minus")
}

fn prefix_numbers(in) {
  let needs_prefix =
    list.any(
      ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"],
      string.starts_with(in, _),
    )
  case needs_prefix {
    True -> "n" <> in
    False -> in
  }
}

fn prefix_signs(in) {
  let in = case string.starts_with(in, "-") {
    True -> "negative" <> in
    False -> in
  }
  case string.starts_with(in, "+") {
    True -> "positive" <> in
    False -> in
  }
}

pub fn name_for_gleam_type(in) {
  in
  |> prefix_signs
  |> replace_disallowed_charachters
  |> justin.pascal_case()
  |> prefix_numbers
}

pub fn name_for_gleam_field_or_var(in) {
  in
  |> prefix_signs
  // calling snake case might remove a leading `_` and expose numbers
  |> justin.snake_case()
  |> prefix_numbers
  |> replace_disallowed_charachters
  |> replace_gleam_keywords()
}

fn gen_content_handling(operation_id, content, wrapper, internal) {
  let module = Operations
  let #(known, unknown) = get_structure_json_media(content)
  case known, unknown {
    [#(_, oas.MediaType(schema))], _ -> {
      let #(lifted, _nullable, internal) = lift.do_lift(schema, internal)
      let #(decoder, resp_type) = case lifted {
        lift.Named(n) -> #(to_decoder(lift.Named(n), module), None)
        lift.Primitive(p) -> #(to_decoder(lift.Primitive(p), module), None)
        lift.Array(a) -> #(to_decoder(lift.Array(a), module), None)
        lift.Tuple(of) -> #(to_decoder(lift.Tuple(of), module), None)
        lift.Compound(lift.Fields(parameters, additional, required)) as top -> {
          let name = operation_id <> "_response"
          let type_ =
            custom_type(name, parameters, additional, required, Operations)
          let decoder =
            glance.Block(gen_top_decoder_needs_name(name, top, module))
          #(decoder, Some(type_))
        }
        lift.Dictionary(values) -> #(
          to_decoder(lift.Dictionary(values), module),
          None,
        )
        lift.Unsupported -> #(to_decoder(lift.Unsupported, module), None)
      }
      let action =
        call2("json", "parse_bits", glance.Variable("body"), decoder)
        |> pipe(call1("result", "map", glance.Variable(wrapper)))
      // True because the body is used

      #(#(action, resp_type, True), internal)
      // gen_json_content_handling(operation_id, schema, wrapper)
    }
    // No content
    [], [] -> #(just_return_ok_nil(wrapper), internal)
    [], [#(unknown, _)] -> {
      io.println("unknown content type: " <> unknown)
      #(just_return_ok_nil(wrapper), internal)
    }
    _, _ -> {
      io.println("multiple content types not supported")
      #(just_return_ok_nil(wrapper), internal)
    }
  }
}

fn to_decoder(lifted, module) {
  case lifted {
    lift.Named("#/components/schemas/" <> name) -> {
      let func = name_for_gleam_field_or_var(name <> "_decoder")
      case module {
        Schema -> glance.Call(glance.Variable(func), [])
        _ -> call0("schema", func)
      }
    }
    lift.Named(ref) ->
      panic as { "not referencing schema component ref: " <> ref }
    lift.Primitive(primitive) ->
      case primitive {
        lift.Boolean -> access("decode", "bool")
        lift.Integer -> access("decode", "int")
        lift.Number -> access("decode", "float")
        lift.String -> access("decode", "string")
        lift.Null -> always_decode()
        lift.Always -> dynamic_decode()
        lift.Never -> never_decode()
      }
    lift.Tuple(items) -> {
      let uses =
        list.index_map(items, fn(item, index) {
          glance.Use(
            [glance.PatternVariable("e" <> int.to_string(index))],
            call1("decode", "then", to_decoder(item, module)),
          )
        })
      let vars =
        list.index_map(items, fn(_, index) {
          glance.Variable("e" <> int.to_string(index))
        })

      glance.Block(
        list.append(uses, [
          glance.Expression(call1("decode", "success", glance.Tuple(vars))),
        ]),
      )
    }
    lift.Array(items) -> call1("decode", "list", to_decoder(items, module))
    lift.Compound(index) -> {
      let func = "internal_" <> int.to_string(index) <> "_decoder"
      glance.Call(glance.Variable(func), [])
    }
    lift.Dictionary(values) ->
      call2(
        "decode",
        "dict",
        access("decode", "string"),
        to_decoder(values, module),
      )
    lift.Unsupported -> dynamic_decode()
  }
}

fn just_return_ok_nil(wrapper) {
  let thing =
    glance.Call(glance.Variable(wrapper), [
      glance.UnlabelledField(glance.Variable("Nil")),
    ])
    |> pipe(glance.Variable("Ok"))
  #(thing, None, False)
}

fn status_range(responses, above_equal, bellow) {
  list.filter_map(responses, fn(response) {
    let #(status, response) = response
    case status {
      oas.Status(i) if above_equal <= i && i < bellow -> Ok(#(i, response))
      _ -> Error(Nil)
    }
  })
}

fn gen_response(operation, components: oas.Components, internal) {
  let #(_method, op) = operation
  let op: oas.Operation = op
  let responses = op.responses |> dict.to_list
  let default =
    list.find_map(responses, fn(response) {
      let #(status, response) = response
      case status {
        oas.Default -> Ok(response)
        _ -> Error(Nil)
      }
    })
  let #(#(default_branch, resp_type, used), internal) = case default {
    Ok(response) -> {
      let oas.Response(content: content, ..) =
        oas.fetch_response(response, components.responses)
      gen_content_handling(op.operation_id, content, "Error", internal)
    }
    Error(Nil) -> {
      case status_range(responses, 400, 600) {
        [#(status, first), ..rest] -> {
          case
            list.try_fold(rest, [status], fn(acc, this) {
              let #(status, this) = this
              case first, this {
                oas.Ref(ref: a, ..), oas.Ref(ref: b, ..) if a == b ->
                  Ok([status, ..acc])
                oas.Inline(oas.Response(content: a, ..)),
                  oas.Inline(oas.Response(content: b, ..))
                  if a == b
                -> Ok([status, ..acc])
                _, _ -> Error(Nil)
              }
            })
          {
            Ok(_statuses) -> {
              let oas.Response(content: content, ..) =
                oas.fetch_response(first, components.responses)
              gen_content_handling(op.operation_id, content, "Error", internal)
            }
            Error(_) -> {
              let branch =
                glance.Variable("response")
                |> pipe(glance.Variable("Error"))
                |> pipe(glance.Variable("Ok"))

              #(#(branch, None, False), internal)
            }
          }
        }
        [] -> {
          let branch =
            glance.Variable("response")
            |> pipe(glance.Variable("Error"))
            |> pipe(glance.Variable("Ok"))

          #(#(branch, None, False), internal)
        }
      }
    }
  }
  let default_clause =
    glance.Clause([[glance.PatternDiscard("")]], None, default_branch)
  let #(#(used, response_type, expected_clauses), internal) = case
    status_range(responses, 200, 300)
    |> list.sort(fn(ra, rb) { int.compare(ra.0, rb.0) })
  {
    [] -> #(#(used, resp_type, []), internal)
    [#(status, first), ..more] -> {
      case more {
        [] -> Nil
        _ -> {
          io.print("Doesn't support multiple ok statuses")
          Nil
        }
      }
      let oas.Response(content: content, ..) =
        oas.fetch_response(first, components.responses)
      let #(#(branch, resp_type, u), internal) =
        gen_content_handling(op.operation_id, content, "Ok", internal)
      let clause =
        glance.Clause(
          [[glance.PatternInt(int.to_string(status))]],
          None,
          branch,
        )
      #(#(used || u, resp_type, [clause]), internal)
    }
  }

  let response_handler =
    glance.Function(
      name: name_for_gleam_field_or_var(op.operation_id <> "_response"),
      publicity: glance.Public,
      parameters: [
        glance.FunctionParameter(None, glance.Named("response"), None),
      ],
      return: None,
      body: [
        glance.Assignment(
          glance.Let,
          glance.PatternConstructor(
            Some("response"),
            // This is broken in glance printer
            ".Response",
            [
              glance.ShorthandField("status"),
              ..case used {
                True -> [glance.ShorthandField("body")]
                False -> []
              }
            ],
            True,
          ),
          None,
          glance.Variable("response"),
        ),
        glance.Expression(glance.Case(
          [glance.Variable("status")],
          list.append(expected_clauses, [default_clause]),
        )),
      ],
      location: glance.Span(0, 0),
    )
  #(#(response_handler, response_type), internal)
}

// This returns functions for the top level and operations file
fn gen_fns(key, path_item: oas.PathItem, components, exclude, internal) {
  let operations =
    list.filter(path_item.operations, fn(op) {
      !list.contains(exclude, { op.1 }.operation_id)
    })

  list.map_fold(operations, internal, fn(internal, op) {
    let #(fn_, req_fn) =
      gen_request_for_op(op, key, path_item.parameters, components)
    let #(#(response_handler, response_type), internal) =
      gen_response(op, components, internal)
    #(internal, #(#([response_handler, req_fn], response_type), fn_))
  })
}

fn gen_operations(op, components, exclude, internal) {
  let #(key, path) = op

  gen_fns(key, path, components, exclude, internal)
}

fn defs(xs) {
  list.map(xs, glance.Definition([], _))
}

pub fn build(spec_src, project_path, provider, exclude) {
  let module_path = project_path <> "/src/" <> provider
  use file <- try(
    simplifile.read(spec_src)
    |> snag.map_error(simplifile.describe_error)
    |> snag.context("Could not read file " <> spec_src),
  )

  use spec <- try(
    json.parse(file, oas.decoder())
    |> snag.map_error(json_decode_error_to_string),
  )

  let #(operations, entry) =
    gen_operations_and_top_files(spec, provider, exclude)

  let operations_module_file = module_path <> "/operations.gleam"

  use Nil <- try(
    simplifile.write_bits(
      operations_module_file,
      operations |> bit_array.from_string,
    )
    |> snag.map_error(simplifile.describe_error)
    |> snag.context("Could not write file " <> operations_module_file),
  )

  use mod <- try(
    simplifile.read(module_path <> ".gleam")
    |> snag.map_error(simplifile.describe_error)
    |> snag.context("Could not read file " <> module_path <> ".gleam"),
  )

  let split = "// GENERATED ---"
  let pre = case string.split_once(mod, split) {
    Ok(#(pre, _)) -> pre
    Error(Nil) -> mod
  }

  let content = <<pre:utf8, split:utf8, "----------\n\n", entry:utf8>>

  use Nil <- try(
    simplifile.write_bits(module_path <> ".gleam", content)
    |> snag.map_error(simplifile.describe_error)
    |> snag.context("Could not write file " <> module_path <> ".gleam"),
  )

  let content =
    gen_schema_file(spec.components.schemas, provider)
    |> bit_array.from_string()

  let schema_module_file = module_path <> "/schema.gleam"

  use Nil <- try(
    simplifile.write_bits(schema_module_file, content)
    |> snag.map_error(simplifile.describe_error)
    |> snag.context("Could not write file " <> schema_module_file),
  )
  Ok(Nil)
}

pub fn gen_operations_and_top_files(spec: oas.Document, provider, exclude) {
  let paths = dict.to_list(spec.paths)
  let internal = []
  let #(internal, fs) =
    list.map_fold(paths, internal, fn(internal, path) {
      gen_operations(path, spec.components, exclude, internal)
    })
  let #(internal_types, internal_decoders) =
    list.index_map(internal, fn(fields, index) {
      let lift.Fields(properties, additional, required) = fields
      let name = "Internal" <> int.to_string(index)
      let type_ =
        custom_type(name, properties, additional, required, Operations)

      let encoder = schema_to_decode_fn(#(name, lift.Compound(fields)))
      #(type_, encoder)
    })
    |> list.unzip

  let #(operation_functions, top) = list.unzip(list.flatten(fs))
  let #(operation_functions, operation_types) = list.unzip(operation_functions)
  let operation_types =
    list.filter_map(operation_types, option.to_result(_, Nil))
  let operation_functions = list.flatten(operation_functions)

  let modules = [
    provider <> "/utils",
    provider <> "/schema",
    "gleam/http",
    "gleam/http/response",
    "gleam/int",
    "gleam/float",
    "gleam/json",
    "gleam/dynamic/decode",
    "gleam/result",
    "gleam/bool",
    // "gleam/option",
  ]
  let operations =
    glance.Module(
      defs([
        glance.Import(
          "gleam/option",
          None,
          [glance.UnqualifiedImport("Option", None)],
          [glance.UnqualifiedImport("None", None)],
        ),
        ..list.map(modules, glance.Import(_, None, [], []))
      ]),
      defs(list.append(internal_types, operation_types)),
      [],
      [],
      list.map(
        list.append(internal_decoders, operation_functions),
        glance.Definition([], _),
      ),
    )
    |> glance_printer.print

  let entry =
    glance.Module([], [], [], [], list.map(top, glance.Definition([], _)))
    |> glance_printer.print
  #(operations, entry)
}

pub fn gen_schema_file(schemas, provider) {
  let #(custom_types, type_aliases, functions) = gen_schema(schemas)

  glance.Module(
    [
      glance.Definition([], glance.Import("gleam/dynamic/decode", None, [], [])),
      glance.Definition([], glance.Import("gleam/dynamic", None, [], [])),
      glance.Definition([], glance.Import("gleam/json", None, [], [])),
      glance.Definition([], glance.Import(provider <> "/utils", None, [], [])),
      glance.Definition(
        [],
        glance.Import(
          "gleam/option",
          None,
          [glance.UnqualifiedImport("Option", None)],
          [glance.UnqualifiedImport("None", None)],
        ),
      ),
    ],
    defs(custom_types),
    defs(type_aliases),
    [],
    defs(functions),
  )
  |> glance_printer.print
}

pub fn json_decode_error_to_string(error: json.DecodeError) -> String {
  case error {
    json.UnexpectedEndOfInput -> "UnexpectedEndOfInput"
    json.UnexpectedByte(str) -> "UnexpectedByte " <> str
    json.UnexpectedSequence(str) -> "UnexpectedSequence " <> str
    json.UnableToDecode(errors) ->
      "UnableToDecode " <> print_decode_errors(errors)
  }
}

fn print_decode_errors(errors) {
  list.map(errors, fn(error) {
    let decode.DecodeError(expected:, found:, path:) = error
    "expected "
    <> expected
    <> " found "
    <> found
    <> " at "
    <> string.join(path, "/")
  })
  |> string.join("\n")
}
