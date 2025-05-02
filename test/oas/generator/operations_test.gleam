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
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice")
  birdie.snap(ops, "get_test")
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
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice")
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
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice")
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
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice")
  birdie.snap(ops, "multiple_error_response_test")
}
// If error response structure looks the same
// put under error
