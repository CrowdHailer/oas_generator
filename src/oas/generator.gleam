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
import oas
import oas/generator/ast
import oas/generator/lift
import oas/generator/misc
import oas/generator/schema
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
                glance.Variable(ast.name_for_gleam_field_or_var(name)),
                ..prev
              ]
              #("", prev)
            }
            oas.Integer(..) -> {
              let prev = [glance.String(current <> "/"), ..prev]
              let prev = [
                ast.call1(
                  "int",
                  "to_string",
                  glance.Variable(ast.name_for_gleam_field_or_var(name)),
                ),
                ..prev
              ]
              #("", prev)
            }
            _ -> {
              let prev = [glance.String(current <> "/"), ..prev]
              let prev = [
                glance.Variable(ast.name_for_gleam_field_or_var(name)),
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

fn noop1(message) {
  glance.Fn([glance.FnParameter(glance.Discarded(""), None)], None, [
    glance.Expression(glance.Panic(Some(glance.String(message)))),
  ])
}

fn query_to_parts(parameters, components: oas.Components) {
  list.map(parameters, fn(p) {
    let #(key, required, schema) = p
    let schema = oas.fetch_schema(schema, components.schemas)
    let key = ast.name_for_gleam_field_or_var(key)
    let arg = glance.FunctionParameter(Some(key), glance.Named(key), None)
    let var = glance.Variable(key)

    let mapper = case schema {
      oas.Boolean(..) -> Some(ast.access("bool", "to_string"))
      oas.Integer(..) -> Some(ast.access("int", "to_string"))
      oas.Number(..) -> Some(ast.access("float", "to_string"))
      oas.String(..) -> None
      _ -> Some(noop1("query parameter is not supported"))
    }
    let value = case required {
      True ->
        ast.call1("option", "Some", case mapper {
          Some(m) -> glance.Call(m, [glance.UnlabelledField(var)])
          None -> var
        })
      False ->
        case mapper {
          Some(mapper) -> ast.call2("option", "map", var, mapper)
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
    http.Get -> ast.access("http", "Get")
    http.Post -> ast.access("http", "Post")
    http.Head -> ast.access("http", "Head")
    http.Put -> ast.access("http", "Put")
    http.Delete -> ast.access("http", "Delete")
    http.Trace -> ast.access("http", "Trace")
    http.Connect -> ast.access("http", "Connect")
    http.Options -> ast.access("http", "Options")
    http.Patch -> ast.access("http", "Patch")
    http.Other(other) -> ast.call1("http", "Other", glance.String(other))
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
          glance.Named(ast.name_for_gleam_field_or_var(name)),
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
  let id = ast.name_for_gleam_field_or_var(id)

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
              let arg = ast.name_for_gleam_field_or_var(name)
              let encode =
                ast.call1(
                  "utils",
                  "json_to_bits",
                  ast.call1(
                    "schema",
                    schema.encode_fn(name),
                    glance.Variable(arg),
                  ),
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
            glance.Expression(ast.pipe(
              glance.Variable("base"),
              ast.pipe(
                ast.call1("utils", "set_method", glance.Variable("method")),
                ast.pipe(
                  ast.call1("utils", "append_path", glance.Variable("path")),
                  ast.pipe(
                    ast.call1("utils", "set_query", glance.Variable("query")),
                    ast.call2(
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
            glance.Expression(ast.pipe(
              glance.Variable("base"),
              ast.pipe(
                ast.call1("utils", "set_method", glance.Variable("method")),
                ast.pipe(
                  ast.call1("utils", "append_path", glance.Variable("path")),
                  ast.call1("utils", "set_query", glance.Variable("query")),
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
          glance.Call(ast.access("operations", op_request), [
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
          ast.call1(
            "t",
            "do",
            ast.call1("t", "fetch", glance.Variable("request")),
          ),
        ),
        glance.Use(
          [glance.PatternVariable("data")],
          ast.call1(
            "t",
            "try",
            glance.Call(glance.Variable("handle_errors"), [
              glance.UnlabelledField(ast.call1(
                "operations",
                op_response,
                glance.Variable("response"),
              )),
            ]),
          ),
        ),
        glance.Expression(ast.call1("t", "Done", glance.Variable("data"))),
      ],
      location: glance.Span(0, 0),
    )
  #(fn_, req_fn)
}

fn gen_content_handling(operation_id, content, wrapper, internal) {
  let module = misc.Operations
  let #(known, unknown) = get_structure_json_media(content)
  case known, unknown {
    [#(_, oas.MediaType(schema))], _ -> {
      // TODO use a better schema function as this needs keeping in track for dictionaries etc
      let #(lifted, _nullable, internal) = lift.do_lift(schema, internal)
      let #(decoder, resp_type) = case lifted {
        lift.Named(n) -> #(schema.to_decoder(lift.Named(n), module), None)
        lift.Primitive(p) -> #(
          schema.to_decoder(lift.Primitive(p), module),
          None,
        )
        lift.Array(a) -> #(schema.to_decoder(lift.Array(a), module), None)
        lift.Tuple(of) -> #(schema.to_decoder(lift.Tuple(of), module), None)
        lift.Compound(lift.Fields(parameters, additional, required)) as top -> {
          let name = operation_id <> "_response"
          let type_ =
            schema.custom_type(
              name,
              parameters,
              additional,
              required,
              misc.Operations,
            )
          let decoder =
            glance.Block(schema.gen_top_decoder_needs_name(name, top, module))
          #(decoder, Some(type_))
        }
        lift.Dictionary(values) -> #(
          schema.to_decoder(lift.Dictionary(values), module),
          None,
        )
        lift.Unsupported -> #(schema.to_decoder(lift.Unsupported, module), None)
      }
      let action =
        ast.call2("json", "parse_bits", glance.Variable("body"), decoder)
        |> ast.pipe(ast.call1("result", "map", glance.Variable(wrapper)))
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

fn just_return_ok_nil(wrapper) {
  let thing =
    glance.Call(glance.Variable(wrapper), [
      glance.UnlabelledField(glance.Variable("Nil")),
    ])
    |> ast.pipe(glance.Variable("Ok"))
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
                |> ast.pipe(glance.Variable("Error"))
                |> ast.pipe(glance.Variable("Ok"))

              #(#(branch, None, False), internal)
            }
          }
        }
        [] -> {
          let branch =
            glance.Variable("response")
            |> ast.pipe(glance.Variable("Error"))
            |> ast.pipe(glance.Variable("Ok"))

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
      name: ast.name_for_gleam_field_or_var(op.operation_id <> "_response"),
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
      // TODO use a better schema function as this needs keeping in track for dictionaries etc
      let type_ =
        schema.custom_type(
          name,
          properties,
          additional,
          required,
          misc.Operations,
        )

      let encoder = schema.to_decode_fn(#(name, lift.Compound(fields)))
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
  let #(custom_types, type_aliases, functions) = schema.generate(schemas)

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
