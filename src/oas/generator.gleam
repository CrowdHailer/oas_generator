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
import simplifile
import snag

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

fn is_optional(property, required) {
  let #(key, schema) = property
  !list.contains(required, key) || is_nullable(schema)
}

fn gen_object(properties, required, name, module) {
  let fields =
    list.map(properties, fn(property) {
      let #(key, schema) = property
      let type_ = case schema {
        oas.Ref(ref: "#/components/schemas/" <> name, ..) -> {
          glance.NamedType(name_for_gleam_type(name), module, [])
        }
        oas.Ref(..) -> panic as "unsupported ref"
        oas.Inline(schema) -> {
          case schema {
            oas.Boolean(..) -> glance.NamedType("Bool", None, [])
            oas.Integer(..) -> glance.NamedType("Int", None, [])
            oas.Number(..) -> glance.NamedType("Float", None, [])
            oas.String(..) -> glance.NamedType("String", None, [])
            oas.Null(..) -> glance.NamedType("Nil", None, [])
            oas.Array(items:, ..) -> array_type(items, module)
            oas.Object(..) -> glance.NamedType("Nil", None, [])
            oas.AnyOf(..) | oas.AllOf(..) | oas.OneOf(..) ->
              glance.NamedType("Nil", None, [])
            oas.AlwaysPasses -> glance.NamedType("Dynamic", Some("dynamic"), [])
            oas.AlwaysFails -> glance.NamedType("Nil", None, [])
          }
        }
      }

      glance.LabelledVariantField(
        case is_optional(property, required) {
          False -> type_
          True -> glance.NamedType("Option", None, [type_])
        },
        name_for_gleam_field_or_var(key),
      )
    })
  let name = name_for_gleam_type(name)
  glance.CustomType(name, glance.Public, False, [], [
    glance.Variant(name, fields),
  ])
}

fn alias(to, type_) {
  glance.TypeAlias(to, glance.Public, [], type_)
}

// Error is alias
fn schema_to_type(name, schema) {
  let type_ = name_for_gleam_type(name)
  case schema {
    oas.Boolean(..) -> Error(alias(type_, glance.NamedType("Bool", None, [])))
    oas.Integer(..) -> Error(alias(type_, glance.NamedType("Int", None, [])))
    oas.Number(..) -> Error(alias(type_, glance.NamedType("Float", None, [])))
    oas.String(..) -> Error(alias(type_, glance.NamedType("String", None, [])))
    oas.Null(..) -> Error(alias(type_, glance.NamedType("Nil", None, [])))
    oas.Array(items:, ..) -> Error(alias(type_, array_type(items, None)))

    oas.Object(properties:, required:, ..) ->
      Ok(gen_object(dict.to_list(properties), required, type_, None))
    oas.AllOf(..) -> Error(alias(type_, glance.NamedType("Nil", None, [])))
    oas.AnyOf(_items) -> Error(alias(type_, glance.NamedType("Nil", None, [])))
    oas.OneOf(..) -> Error(alias(type_, glance.NamedType("Nil", None, [])))
    oas.AlwaysPasses(..) ->
      Error(alias(type_, glance.NamedType("Dynamic", Some("dynamic"), [])))
    oas.AlwaysFails -> Error(alias(type_, glance.NamedType("Nil", None, [])))
  }
}

fn array_type(items, module) {
  let inner = case items {
    oas.Ref(ref: "#/components/schemas/" <> inner, ..) ->
      glance.NamedType(name_for_gleam_type(inner), module, [])
    oas.Ref(..) -> panic as "unexpected ref"
    oas.Inline(inner) ->
      case inner {
        oas.Boolean(..) -> glance.NamedType("Bool", None, [])
        oas.Integer(..) -> glance.NamedType("Int", None, [])
        oas.Number(..) -> glance.NamedType("Float", None, [])
        oas.String(..) -> glance.NamedType("String", None, [])
        oas.Null(..) -> glance.NamedType("Nil", None, [])
        oas.Array(items:, ..) -> array_type(items, module)
        oas.Object(..) -> glance.NamedType("Nil", None, [])
        oas.AnyOf(..) -> glance.NamedType("Nil", None, [])
        oas.AllOf(..) -> glance.NamedType("Nil", None, [])
        oas.OneOf(..) -> glance.NamedType("Nil", None, [])
        oas.AlwaysPasses -> glance.NamedType("Dynamic", Some("dynamic"), [])
        oas.AlwaysFails(..) -> glance.NamedType("Nil", None, [])
      }
  }
  glance.NamedType("List", None, [inner])
}

fn encode_fn(name) {
  name_for_gleam_field_or_var(name <> "_encode")
}

fn noop1(message) {
  glance.Fn([glance.FnParameter(glance.Discarded(""), None)], None, [
    glance.Expression(glance.Panic(Some(glance.String(message)))),
  ])
}

fn schema_to_encoder(entry) {
  let #(name, schema) = entry
  let type_ = name_for_gleam_type(name)

  let exp = case schema {
    oas.Boolean(..) -> call1("json", "bool", glance.Variable("data"))
    oas.Integer(..) -> call1("json", "int", glance.Variable("data"))
    oas.Number(..) -> call1("json", "float", glance.Variable("data"))
    oas.String(..) -> call1("json", "string", glance.Variable("data"))
    oas.Null(..) -> access("json", "null")
    oas.Array(items:, ..) -> array_encoder(items, Some("data"))

    oas.Object(properties:, required:, ..) -> {
      let properties = dict.to_list(properties)
      call1(
        "json",
        "object",
        glance.List(
          list.map(properties, fn(p) {
            let #(key, schema) = p

            glance.Tuple([
              glance.String(key),
              {
                let arg = access("data", name_for_gleam_field_or_var(key))
                let cast = case schema {
                  oas.Ref(ref: "#/components/schemas/" <> named, ..) ->
                    glance.Variable(encode_fn(named))
                  oas.Ref(ref:, ..) -> noop1("unknown ref name: " <> ref)
                  oas.Inline(oas.Boolean(..)) -> access("json", "bool")
                  oas.Inline(oas.Integer(..)) -> access("json", "int")
                  oas.Inline(oas.Number(..)) -> access("json", "float")
                  oas.Inline(oas.String(..)) -> access("json", "string")
                  oas.Inline(oas.Null(..)) -> noop1("encode null in field")
                  oas.Inline(oas.Array(items:, ..)) ->
                    array_encoder(items, None)
                  oas.Inline(oas.Object(..)) ->
                    noop1("Literal object inside field")
                  oas.Inline(oas.AllOf(_items)) -> noop1("AllOf inside field")
                  oas.Inline(oas.AnyOf(_items)) -> noop1("AnyOf inside field")
                  oas.Inline(oas.OneOf(_items)) -> noop1("OneOf inside field")
                  oas.Inline(oas.AlwaysPasses) ->
                    access("utils", "dynamic_to_json")
                  oas.Inline(oas.AlwaysFails) ->
                    noop1("AlwaysFails inside field")
                }

                case is_optional(p, required) {
                  False -> glance.Call(cast, [glance.UnlabelledField(arg)])
                  True -> call2("json", "nullable", arg, cast)
                }
              },
            ])
          }),
          None,
        ),
      )
    }
    oas.AllOf(..) -> glance.Panic(Some(glance.String("AllOf")))
    oas.AnyOf(..) -> glance.Panic(Some(glance.String("AnyOf")))
    oas.OneOf(..) -> glance.Panic(Some(glance.String("OneOf")))
    oas.AlwaysPasses ->
      call1("utils", "dynamic_to_json", glance.Variable("data"))
    oas.AlwaysFails -> glance.Panic(Some(glance.String("Aloas.AlwaysFails")))
  }
  let ignored = case schema {
    oas.Null(..) -> True
    oas.Object(properties:, ..) -> dict.is_empty(properties)
    oas.AllOf(..) | oas.AnyOf(..) | oas.OneOf(..) -> True
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

fn array_encoder(items, top_level) {
  let mapper = case items {
    oas.Ref(ref: "#/components/schemas/" <> inner, ..) ->
      glance.Variable(encode_fn(inner))
    oas.Ref(..) -> panic as "unexpected ref"
    oas.Inline(items) ->
      case items {
        oas.Boolean(..) -> access("json", "bool")
        oas.Integer(..) -> access("json", "int")
        oas.Number(..) -> access("json", "float")
        oas.String(..) -> access("json", "string")
        oas.Null(..) -> noop1("null in array")
        oas.Array(items:, ..) -> array_encoder(items, None)
        oas.Object(..) -> noop1("object in array")
        oas.AllOf(..) -> noop1("Alloas.AllOf in array")
        oas.AnyOf(..) -> noop1("Anyoas.AnyOf in array")
        oas.OneOf(..) -> noop1("Oneoas.OneOf in array")
        oas.AlwaysPasses(..) -> access("utils", "dynamic_to_json")
        oas.AlwaysFails(..) -> noop1("AlwaysFails in array")
      }
  }
  let arg = case top_level {
    Some(var) -> glance.Variable(var)
    None -> glance.Variable("_")
  }
  call2("json", "array", arg, mapper)
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

fn is_nullable(schema) {
  case schema {
    oas.Ref(..) -> False
    oas.Inline(schema) ->
      case schema {
        oas.Boolean(nullable:, ..) -> nullable
        oas.Integer(nullable:, ..) -> nullable
        oas.Number(nullable:, ..) -> nullable
        oas.String(nullable:, ..) -> nullable
        oas.Null(..) -> False
        oas.Array(nullable:, ..) -> nullable
        oas.Object(nullable:, ..) -> nullable
        oas.AllOf(_) -> False
        oas.AnyOf(_) -> False
        oas.OneOf(_) -> False
        oas.AlwaysPasses(..) -> False
        oas.AlwaysFails(..) -> False
      }
  }
}

fn schema_to_decode_fn(entry) {
  let #(name, schema) = entry

  let body = schema_to_decoder(name, schema, None)
  glance.Function(
    name: decoder(name),
    publicity: glance.Public,
    parameters: [],
    return: None,
    body:,
    location: glance.Span(0, 0),
  )
}

fn schema_to_decoder(name, schema, module) {
  let type_ = name_for_gleam_type(name)
  case schema {
    oas.Boolean(..) -> [glance.Expression(access("decode", "bool"))]
    oas.Integer(..) -> [glance.Expression(access("decode", "int"))]
    oas.Number(..) -> [glance.Expression(access("decode", "float"))]
    oas.String(..) -> [glance.Expression(access("decode", "string"))]
    oas.Null(..) -> [glance.Expression(always_decode())]
    oas.Array(items:, ..) -> [glance.Expression(array_decoder(items, module))]
    oas.Object(properties:, required:, ..) -> {
      let properties = dict.to_list(properties)
      let #(fields, cons) =
        list.map(properties, fn(property) {
          let #(key, schema) = property

          let field_decoder = case schema {
            oas.Ref(ref: "#/components/schemas/" <> s, ..) ->
              case module {
                Some(m) -> call0(m, decoder(s))
                None -> glance.Call(glance.Variable(decoder(s)), [])
              }
            oas.Ref(..) -> panic as "unknown ref"
            oas.Inline(oas.Boolean(..)) -> access("decode", "bool")
            oas.Inline(oas.Integer(..)) -> access("decode", "int")
            oas.Inline(oas.Number(..)) -> access("decode", "float")
            oas.Inline(oas.String(..)) -> access("decode", "string")
            oas.Inline(oas.Null(..)) -> always_decode()
            oas.Inline(oas.Array(items:, ..)) -> array_decoder(items, module)
            oas.Inline(oas.Object(..)) -> always_decode()
            oas.Inline(oas.AllOf(..)) -> always_decode()
            oas.Inline(oas.AnyOf(..)) -> always_decode()
            oas.Inline(oas.OneOf(..)) -> always_decode()
            oas.Inline(oas.AlwaysPasses) -> dynamic_decode()
            oas.Inline(oas.AlwaysFails) -> always_decode()
          }
          let is_optional = !list.contains(required, key) || is_nullable(schema)
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
        |> list.unzip

      let final =
        glance.Expression(
          call1("decode", "success", case list.length(cons) {
            0 -> glance.Variable(type_)
            _ -> glance.Call(glance.Variable(type_), cons)
          }),
        )
      list.append(fields, [final])
    }
    oas.AllOf(..) -> [
      glance.Expression(glance.Panic(Some(glance.String("AllOf")))),
    ]
    oas.AnyOf(..) -> [
      glance.Expression(glance.Panic(Some(glance.String("AnyOf")))),
    ]
    oas.OneOf(..) -> [
      glance.Expression(glance.Panic(Some(glance.String("OneOf")))),
    ]
    oas.AlwaysPasses(..) -> [glance.Expression(dynamic_decode())]
    oas.AlwaysFails(..) -> [
      glance.Expression(glance.Panic(Some(glance.String("Alwoas.AlwaysFails")))),
    ]
  }
}

fn array_decoder(items, module) {
  let exp = case items {
    oas.Ref(ref: "#/components/schemas/" <> inner, ..) ->
      case module {
        Some(m) -> call0(m, decoder(inner))
        None -> glance.Call(glance.Variable(decoder(inner)), [])
      }
    oas.Ref(..) -> panic as "what is this s ref"
    oas.Inline(inner) -> {
      case inner {
        oas.Boolean(..) -> access("decode", "bool")
        oas.Integer(..) -> access("decode", "int")
        oas.Number(..) -> access("decode", "float")
        oas.String(..) -> access("decode", "string")
        oas.Null(..) -> always_decode()
        oas.Array(items:, ..) -> array_decoder(items, module)
        oas.Object(..) -> always_decode()
        oas.AllOf(..) -> always_decode()
        oas.AnyOf(..) -> always_decode()
        oas.OneOf(..) -> always_decode()
        oas.AlwaysPasses(..) -> dynamic_decode()
        oas.AlwaysFails(..) -> always_decode()
      }
    }
  }
  call1("decode", "list", exp)
}

fn gen_schema(schemas) {
  dict.fold(schemas, #([], [], []), fn(acc, name, schema) {
    let #(custom_types, type_aliases, fns) = acc
    let fns =
      list.append(fns, [
        schema_to_encoder(#(name, schema)),
        schema_to_decode_fn(#(name, schema)),
      ])
    let #(custom_types, type_aliases) = case schema_to_type(name, schema) {
      Error(alias) -> #(custom_types, [alias, ..type_aliases])
      Ok(custom_type) -> #([custom_type, ..custom_types], type_aliases)
    }
    #(custom_types, type_aliases, fns)
  })
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
  |> justin.pascal_case()
  |> prefix_numbers
  |> replace_disallowed_charachters
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

fn gen_content_handling(operation_id, content, wrapper) {
  let #(known, unknown) = get_structure_json_media(content)
  case known, unknown {
    [#(_, oas.MediaType(schema))], _ ->
      gen_json_content_handling(operation_id, schema, wrapper)
    // No content
    [], [] -> just_return_ok_nil(wrapper)
    [], [#(unknown, _)] -> {
      io.println("unknown content type: " <> unknown)
      just_return_ok_nil(wrapper)
    }
    _, _ -> {
      io.println("multiple content types not supported")
      just_return_ok_nil(wrapper)
    }
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

fn gen_json_content_handling(operation_id, schema, wrapper) {
  let #(resp_type, decoder) = case schema {
    oas.Inline(oas.Array(
      items: oas.Ref(
        ref: "#/components/schemas/" <> name,
        ..,
      ),
      ..,
    )) -> #(
      None,
      call1(
        "decode",
        "list",
        call0("schema", name_for_gleam_field_or_var(name <> "_decoder")),
      ),
    )
    oas.Ref(ref: "#/components/schemas/" <> name, ..) -> #(
      None,
      call0("schema", name_for_gleam_field_or_var(name <> "_decoder")),
    )
    oas.Inline(oas.Object(properties:, required:, ..) as schema) -> {
      let name = operation_id <> "_response"
      let resp_type =
        gen_object(dict.to_list(properties), required, name, Some("schema"))

      let decoder =
        schema_to_decoder(name, schema, Some("schema"))
        |> glance.Block()
      #(Some(resp_type), decoder)
    }
    oas.Inline(oas.AlwaysPasses) -> #(None, dynamic_decode())
    _ -> {
      #(
        None,
        call2(
          "decode",
          "failure",
          glance.Variable("Nil"),
          glance.String("Unsupported schema"),
        ),
      )
    }
  }
  let action =
    call2("json", "parse_bits", glance.Variable("body"), decoder)
    |> pipe(call1("result", "map", glance.Variable(wrapper)))
  // True because the body is used
  #(action, resp_type, True)
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

fn gen_response(operation, components: oas.Components) {
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
  let #(default_branch, resp_type, used) = case default {
    Ok(response) -> {
      let oas.Response(content: content, ..) =
        oas.fetch_response(response, components.responses)
      gen_content_handling(op.operation_id, content, "Error")
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
              gen_content_handling(op.operation_id, content, "Error")
            }
            Error(_) -> {
              let branch =
                glance.Variable("response")
                |> pipe(glance.Variable("Error"))
                |> pipe(glance.Variable("Ok"))

              #(branch, None, False)
            }
          }
        }
        [] -> {
          let branch =
            glance.Variable("response")
            |> pipe(glance.Variable("Error"))
            |> pipe(glance.Variable("Ok"))

          #(branch, None, False)
        }
      }
    }
  }
  let default_clause =
    glance.Clause([[glance.PatternDiscard("")]], None, default_branch)
  let #(used, response_type, expected_clauses) = case
    status_range(responses, 200, 300)
    |> list.sort(fn(ra, rb) { int.compare(ra.0, rb.0) })
  {
    [] -> #(used, resp_type, [])
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
      let #(branch, resp_type, u) =
        gen_content_handling(op.operation_id, content, "Ok")
      let clause =
        glance.Clause(
          [[glance.PatternInt(int.to_string(status))]],
          None,
          branch,
        )
      #(used || u, resp_type, [clause])
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
  #(response_handler, response_type)
}

// This returns functions for the top level and operations file
fn gen_fns(key, path_item: oas.PathItem, components, exclude) {
  let operations =
    list.filter(path_item.operations, fn(op) {
      !list.contains(exclude, { op.1 }.operation_id)
    })

  list.map(operations, fn(op) {
    let #(fn_, req_fn) =
      gen_request_for_op(op, key, path_item.parameters, components)
    let #(response_handler, response_type) = gen_response(op, components)
    #(#([response_handler, req_fn], response_type), fn_)
  })
}

fn gen_ops(op, components, exclude) {
  let #(key, path) = op

  gen_fns(key, path, components, exclude)
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
  let fs = list.flat_map(paths, gen_ops(_, spec.components, exclude))
  let #(operation_functions, top) = list.unzip(fs)
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
      defs(operation_types),
      [],
      [],
      list.map(operation_functions, glance.Definition([], _)),
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
