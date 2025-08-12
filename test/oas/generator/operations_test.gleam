import birdie
import gleam/dict
import gleam/http
import gleam/option.{None, Some}
import oas
import oas/generator as gen
import oas/json_schema

fn array(items) {
  json_schema.Array(None, None, False, items, False, None, None, False)
}

fn object(params, required) {
  json_schema.Object(
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
  json_schema.Object(
    dict.new(),
    [],
    Some(of),
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

fn get(id, parameters, responses) {
  #(http.Get, operation(id, parameters, None, responses))
}

fn post(id, parameters, request_body, responses) {
  #(http.Post, operation(id, parameters, Some(request_body), responses))
}

fn path(p, items) {
  #(p, oas.PathItem(None, None, [], items))
}

fn json_response(schema) {
  json_schema.Inline(oas.Response(None, dict.new(), just_json(schema)))
}

fn empty_response() {
  json_schema.Inline(oas.Response(None, dict.new(), dict.new()))
}

fn just_json(schema) {
  dict.from_list([#("application/json", oas.MediaType(Some(schema)))])
}

fn ref(schema) {
  json_schema.Ref("#/components/schemas/" <> schema, None, None)
}

pub fn single_response_test() {
  let paths =
    dict.from_list([
      path("/users", [
        get("get_users", [], [#(oas.Status(200), json_response(ref("account")))]),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "get_test")
}

pub fn just_string_request_test() {
  let paths =
    dict.from_list([
      path("/number", [
        post(
          "upload-number",
          [],
          json_schema.Inline(oas.RequestBody(
            None,
            just_json(json_schema.Inline(json_schema.integer())),
            True,
          )),
          [#(oas.Status(204), empty_response())],
        ),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "just_string_request_test")
}

pub fn object_request_test() {
  let paths =
    dict.from_list([
      path("/params", [
        post(
          "set_params",
          [],
          json_schema.Inline(oas.RequestBody(
            None,
            just_json(
              json_schema.Inline(
                object(
                  [
                    #("size", json_schema.Inline(json_schema.integer())),
                    #(
                      "shape",
                      json_schema.Ref("#/components/schemas/Shape", None, None),
                    ),
                  ],
                  [],
                ),
              ),
            ),
            True,
          )),
          [#(oas.Status(204), empty_response())],
        ),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "object_request_test")
}

pub fn nested_object_request_test() {
  let paths =
    dict.from_list([
      path("/params", [
        post(
          "set_params",
          [],
          json_schema.Inline(oas.RequestBody(
            None,
            just_json(
              json_schema.Inline(
                array(
                  json_schema.Inline(
                    object(
                      [
                        #("id", json_schema.Inline(json_schema.integer())),
                        #(
                          "flavour",
                          json_schema.Ref(
                            "#/components/schemas/Flavour",
                            None,
                            None,
                          ),
                        ),
                      ],
                      ["id", "flavour"],
                    ),
                  ),
                ),
              ),
            ),
            True,
          )),
          [#(oas.Status(204), empty_response())],
        ),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "nested_object_request_test")
}

pub fn dictionary_request_test() {
  let paths =
    dict.from_list([
      path("/params", [
        post(
          "set_params",
          [],
          json_schema.Inline(oas.RequestBody(
            None,
            just_json(
              json_schema.Inline(
                dict(json_schema.Inline(json_schema.integer())),
              ),
            ),
            True,
          )),
          [#(oas.Status(204), empty_response())],
        ),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "dictionary_request_test")
}

pub fn request_body_named_test() {
  let paths =
    dict.from_list([
      path("/params", [
        post(
          "set_params",
          [],
          json_schema.Inline(oas.RequestBody(
            None,
            just_json(json_schema.Ref("#/components/schemas/params", None, None)),
            True,
          )),
          [],
        ),
      ]),
    ])
  let components =
    oas.Components(dict.new(), dict.new(), dict.new(), dict.new())

  let doc = oas.Document("", no_info, None, [], paths, components)
  let #(ops, _) = gen.gen_operations_and_top_files(doc, "myservice", [])
  birdie.snap(ops, "request_body_named_test")
}

pub fn single_inline_object_response_test() {
  let paths =
    dict.from_list([
      path("/users", [
        get("get_users", [], [
          #(
            oas.Status(200),
            json_response(
              json_schema.Inline(
                object(
                  [
                    #(
                      "items",
                      json_schema.Inline(
                        array(json_schema.Inline(json_schema.string())),
                      ),
                    ),
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
        get("users/get_users", [], [
          #(
            oas.Status(200),
            json_response(
              json_schema.Inline(
                object(
                  [
                    #(
                      "metadata",
                      json_schema.Inline(
                        object(
                          [#("param", json_schema.Inline(json_schema.string()))],
                          [
                            "param",
                          ],
                        ),
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
            json_schema.Inline(oas.QueryParameter(
              "tags",
              None,
              False,
              json_schema.Inline(
                array(json_schema.Inline(json_schema.string())),
              ),
            )),
          ],
          [#(oas.Status(200), json_response(ref("account")))],
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
        get("get_users", [], [#(oas.Status(204), empty_response())]),
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
        get("get_users", [], [
          #(oas.Status(200), json_response(ref("account"))),
          #(oas.Status(400), json_response(ref("error"))),
          #(oas.Status(401), json_response(ref("error"))),
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
        get("get_users", [], [
          #(
            oas.Status(200),
            json_response(json_schema.Inline(json_schema.AlwaysPasses)),
          ),
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
        get("get_users", [], [
          #(
            oas.Status(200),
            json_response(
              json_schema.Inline(
                object(
                  [#("meta", json_schema.Inline(json_schema.AlwaysPasses))],
                  [
                    "meta",
                  ],
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
  birdie.snap(ops, "alway_field_pass_test")
}
