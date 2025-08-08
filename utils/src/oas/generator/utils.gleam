import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/dynamic/decode
import gleam/http/request
import gleam/javascript/array
import gleam/json
import gleam/list
import gleam/option.{None, Some}

pub fn set_method(request, method) {
  request.set_method(request, method)
}

pub fn append_path(request, path) {
  request.set_path(request, request.path <> path)
}

pub fn set_query(request, query) {
  let query =
    list.filter_map(query, fn(q) {
      let #(k, v) = q
      case v {
        Some(v) -> Ok(#(k, v))
        None -> Error(Nil)
      }
    })
  case query {
    [] -> request
    _ -> request.set_query(request, query)
  }
}

pub fn set_body(request, mime, content) {
  request
  |> request.prepend_header("content-type", mime)
  |> request.set_body(content)
}

pub type Any {
  Object(Dict(String, Any))
  Array(List(Any))
  Boolean(Bool)
  Integer(Int)
  Number(Float)
  String(String)
  Null
}

pub fn any_decoder() {
  use <- decode.recursive
  decode.one_of(decode.map(decode.dict(decode.string, any_decoder()), Object), [
    decode.list(any_decoder()) |> decode.map(Array),
    decode.bool |> decode.map(Boolean),
    decode.int |> decode.map(Integer),
    decode.float |> decode.map(Number),
    decode.map(decode.optional(decode.string), fn(decoded) {
      case decoded {
        Some(str) -> String(str)
        None -> Null
      }
    }),
  ])
}

pub fn json_to_bits(json) {
  json
  |> json.to_string
  |> bit_array.from_string
}

pub fn any_to_json(any) {
  case any {
    Object(dict) -> json.dict(dict, fn(x) { x }, any_to_json)
    Array(list) -> json.array(list, any_to_json)
    Boolean(bool) -> json.bool(bool)
    Integer(int) -> json.int(int)
    Number(float) -> json.float(float)
    String(string) -> json.string(string)
    Null -> json.null()
  }
}

pub fn any_to_dynamic(any) {
  case any {
    Object(entries) ->
      dynamic.properties(
        entries
        |> dict.to_list
        |> list.map(fn(entry) {
          let #(key, value) = entry
          #(dynamic.string(key), any_to_dynamic(value))
        }),
      )
    Array(items) -> dynamic.list(list.map(items, any_to_dynamic))
    Boolean(bool) -> dynamic.bool(bool)
    Integer(int) -> dynamic.int(int)
    Number(float) -> dynamic.float(float)
    String(string) -> dynamic.string(string)
    Null -> dynamic.nil()
  }
}

pub type Never {
  Never(Never)
}

pub fn dict(dict, values) {
  json.dict(dict, fn(x) { x }, values)
}

@external(javascript, "./ffi.mjs", "merge")
fn do_merge(items: array.Array(json.Json)) -> json.Json

pub fn merge(items) {
  do_merge(array.from_list(items))
}

pub fn decode_additional(_except, _decoder, next) {
  // use r <- decode.then(decode.dict(decode.string, decoder))
  // let additional = dict.drop(r, except)
  // TODO
  use additional <- decode.then(decode.success(dict.new()))
  next(additional)
}

pub fn object(entries: List(#(String, json.Json))) {
  list.filter(entries, fn(entry) {
    let #(_, v) = entry
    v != json.null()
  })
  |> json.object
}
