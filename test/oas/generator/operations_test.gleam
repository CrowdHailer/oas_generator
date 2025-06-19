import birdie
import gleam/dict
import gleam/http
import gleam/option.{None}
import oas
import oas/generator as gen

const just_string = oas.String(None, None, None, None, False, None, None, False)

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

const no_info = oas.Info("title", None, None, None, None, None, "3.1.0")

fn operation(id, parameters, request_body, responses) {
  oas.Operation(
    [],
    None,
    None,
    id,
    parameters,
    request_body,
    dict.from_list(responses),
  )
}

fn get(id, parameters, request_body, responses) {
  #(http.Get, operation(id, parameters, request_body, responses))
}

fn path(p, items) {
  #(p, oas.PathItem(None, None, [], items))
}

fn json(schema) {
  oas.Inline(oas.Response(
    None,
    dict.new(),
    dict.from_list([#("application/json", oas.MediaType(schema))]),
  ))
}

fn ref(schema) {
  oas.Ref("#/components/schemas/" <> schema, None, None)
}

pub fn single_response_test() {
  let paths =
    dict.from_list([
      path("/users", [
        get("get_users", [], None, [#(oas.Status(200), json(ref("account")))]),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "get_test")
}

pub fn single_inline_object_response_test() {
  let paths =
    dict.from_list([
      path("/users", [
        get("get_users", [], None, [
          #(
            oas.Status(200),
            json(
              oas.Inline(
                object(
                  [
                    #("items", oas.Inline(array(oas.Inline(just_string)))),
                    #("thing", ref("thing")),
                  ],
                  ["items"],
                ),
              ),
            ),
          ),
        ]),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "single_inline_object_response_test")
}

pub fn nested_object_in_response_test() {
  let paths =
    dict.from_list([
      path("/users", [
        get("users/get_users", [], None, [
          #(
            oas.Status(200),
            json(
              oas.Inline(
                object(
                  [
                    #(
                      "metadata",
                      oas.Inline(
                        object([#("param", oas.Inline(just_string))], ["param"]),
                      ),
                    ),
                  ],
                  ["metadata"],
                ),
              ),
            ),
          ),
        ]),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "nested_object_in_response_test")
}

pub fn array_parameters_response_test() {
  let paths =
    dict.from_list([
      path("/users", [
        get(
          "get_users",
          [
            oas.Inline(oas.QueryParameter(
              "tags",
              None,
              False,
              oas.Inline(array(oas.Inline(just_string))),
            )),
          ],
          None,
          [#(oas.Status(200), json(ref("account")))],
        ),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "array_parameters_response_test")
}

pub fn no_content_response_test() {
  let paths =
    dict.from_list([
      path("/users", [
        get("get_users", [], None, [
          #(
            oas.Status(204),
            oas.Inline(oas.Response(None, dict.new(), dict.new())),
          ),
        ]),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "no_content_response")
}

pub fn multiple_error_response_test() {
  let paths =
    dict.from_list([
      path("/users", [
        get("get_users", [], None, [
          #(oas.Status(200), json(ref("account"))),
          #(oas.Status(400), json(ref("error"))),
          #(oas.Status(401), json(ref("error"))),
        ]),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "multiple_error_response_test")
}

// If error response structure looks the same
// put under error

pub fn alway_pass_test() {
  let paths =
    dict.from_list([
      path("/users", [
        get("get_users", [], None, [
          #(oas.Status(200), json(oas.Inline(oas.AlwaysPasses))),
        ]),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "alway_pass_test")
}

pub fn alway_field_pass_test() {
  let paths =
    dict.from_list([
      path("/users", [
        get("get_users", [], None, [
          #(
            oas.Status(200),
            json(
              oas.Inline(
                object([#("meta", oas.Inline(oas.AlwaysPasses))], ["meta"]),
              ),
            ),
          ),
        ]),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "alway_field_pass_test")
}
