import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import non_empty_list
import oas

// direct references
pub fn references_from_paths(paths: dict.Dict(String, oas.PathItem)) {
  dict.fold(paths, #(set.new(), set.new(), set.new()), fn(acc, _key, path) {
    list.fold(path.operations, acc, fn(acc, op) {
      let #(requests, responses, direct) = acc

      let #(_method, op) = op
      let acc = case op.request_body {
        Some(oas.Inline(request_body)) -> {
          let direct =
            references_from_media_options(request_body.content, direct)
          #(requests, responses, direct)
        }
        Some(oas.Ref(ref:, ..)) -> {
          let requests = set.insert(requests, ref)
          #(requests, responses, direct)
        }
        None -> acc
      }
      dict.fold(op.responses, acc, fn(acc, _status, response) {
        let #(requests, responses, direct) = acc
        case response {
          oas.Inline(response) -> {
            let direct = references_from_media_options(response.content, direct)
            #(requests, responses, direct)
          }
          oas.Ref(ref:, ..) -> {
            let responses = set.insert(responses, ref)
            #(requests, responses, direct)
          }
        }
      })
    })
  })
}

pub fn references_from_request_bodies(
  request_bodies: dict.Dict(String, oas.Ref(oas.RequestBody)),
) {
  dict.fold(request_bodies, set.new(), fn(acc, _key, request_body) {
    case request_body {
      oas.Ref(ref:, ..) -> todo
      oas.Inline(request_body) ->
        references_from_media_options(request_body.content, acc)
    }
  })
}

fn references_from_media_options(media: dict.Dict(String, oas.MediaType), acc) {
  dict.fold(media, acc, fn(acc, _key, media) {
    let oas.MediaType(schema) = media
    do_references_from_schema(schema, acc)
  })
}

fn do_references_from_schema(schema, acc) {
  case schema {
    oas.Ref(ref:, ..) -> set.insert(acc, ref)
    oas.Inline(schema) ->
      case schema {
        oas.Boolean(..)
        | oas.Integer(..)
        | oas.Number(..)
        | oas.String(..)
        | oas.Null(..)
        | oas.AlwaysPasses
        | oas.AlwaysFails -> acc
        oas.Array(items:, ..) -> do_references_from_schema(items, acc)
        oas.Object(properties:, additional_properties:, ..) -> {
          let acc = case additional_properties {
            Some(inner) -> do_references_from_schema(inner, acc)
            None -> acc
          }
          dict.fold(properties, acc, fn(acc, _key, schema) {
            do_references_from_schema(schema, acc)
          })
        }
        oas.AllOf(inner) | oas.AnyOf(inner) | oas.OneOf(inner) ->
          list.fold(non_empty_list.to_list(inner), acc, fn(acc, schema) {
            do_references_from_schema(schema, acc)
          })
      }
  }
}
