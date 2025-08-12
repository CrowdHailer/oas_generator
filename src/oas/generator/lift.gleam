import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import non_empty_list.{NonEmptyList}
import oas/generator/utils
import oas/json_schema

pub type Schema(t) {
  Named(String)
  Primitive(Primitive)
  Array(Lifted)
  Tuple(List(Lifted))
  Compound(t)
  Dictionary(Lifted)
  Unsupported
}

pub type Lifted =
  Schema(Int)

pub type Top =
  Schema(Fields)

pub type Fields {
  Fields(
    named: List(#(String, #(Lifted, Bool))),
    additional: Option(Lifted),
    required: List(String),
  )
}

pub type Primitive {
  Boolean
  Integer
  Number
  String
  Null
  Always
  Never
}

// Can totally manually pass in config i.e. encoded/decode or even name 
// pub fn lift(schema) {
//   do_lift(schema, [])
// }

// pub type Return {
//   // Nil because we wouldn't use Compout
//   Done(Schema(Int))
//   Name(Fields, fn(Schema(Int)) -> Return)
// }

// fn recur(schema) -> Return {
//   case schema {
//     oas.Ref(ref:, ..) -> Done(Named(ref))
//     oas.Inline(schema) ->
//       case schema {
//         oas.Boolean(..) -> Done(Primitive(Boolean))
//         oas.Array(items:, ..) ->
//           case recur(items) {
//             Done(items) -> Done(Array(items))
//             Name(inner, f) -> Name(inner, fn(ref) { todo })
//           }
//         _ -> todo
//       }
//     _ -> todo
//   }
// }

// fn then(r, g) {
//   case r {
//     Done(schema) -> Done(schema)
//     Name(fields, f) -> Name(fields, fn(schema) { then(g(schema), f) })
//   }
// }

fn not_top(top: Top, acc) -> #(Lifted, _) {
  case top {
    Compound(fields) -> {
      let id = list.length(acc)
      let acc = [fields, ..acc]
      #(Compound(id), acc)
    }
    Named(name) -> #(Named(name), acc)
    Primitive(primitive) -> #(Primitive(primitive), acc)
    Array(items) -> #(Array(items), acc)
    Tuple(items) -> #(Tuple(items), acc)
    Dictionary(values) -> #(Dictionary(values), acc)
    Unsupported -> #(Unsupported, acc)
  }
}

// nullable is ignored at the moment
pub fn do_lift(schema, acc) -> #(Top, Bool, List(_)) {
  case schema {
    json_schema.Ref(ref:, ..) -> #(Named(ref), False, acc)
    json_schema.Inline(schema) ->
      case schema {
        json_schema.Boolean(nullable:, ..) -> #(
          Primitive(Boolean),
          nullable,
          acc,
        )
        json_schema.Integer(nullable:, ..) -> #(
          Primitive(Integer),
          nullable,
          acc,
        )
        json_schema.Number(nullable:, ..) -> #(Primitive(Number), nullable, acc)
        json_schema.String(nullable:, ..) -> #(Primitive(String), nullable, acc)
        json_schema.Null(..) -> #(Primitive(Null), False, acc)
        json_schema.Array(nullable:, items:, ..) -> {
          let #(top, _, acc) = do_lift(items, acc)
          let #(schema, acc) = not_top(top, acc)
          #(Array(schema), nullable, acc)
        }
        json_schema.Object(
          nullable:,
          properties:,
          required:,
          additional_properties:,
          ..,
        ) -> {
          case dict.is_empty(properties), additional_properties {
            True, Some(values) -> {
              let #(top, _, acc) = do_lift(values, acc)
              let #(schema, acc) = not_top(top, acc)
              #(Dictionary(schema), nullable, acc)
            }
            True, None -> #(Dictionary(Primitive(Always)), nullable, acc)
            _, _ -> {
              let #(acc, properties) =
                list.map_fold(
                  properties |> dict.to_list,
                  acc,
                  fn(acc, property) {
                    let #(field, schema) = property
                    let #(top, nullable, acc) = do_lift(schema, acc)
                    let #(schema, acc) = not_top(top, acc)

                    #(acc, #(field, #(schema, nullable)))
                  },
                )
              let #(additional, acc) = case additional_properties {
                None | Some(json_schema.Inline(json_schema.AlwaysFails)) -> #(
                  None,
                  acc,
                )
                Some(values) -> {
                  let #(top, _, acc) = do_lift(values, acc)
                  let #(schema, acc) = not_top(top, acc)
                  #(Some(schema), acc)
                }
              }
              #(
                Compound(Fields(properties, additional, required)),
                nullable,
                acc,
              )
            }
          }
        }
        json_schema.AllOf(NonEmptyList(schema, [])) -> do_lift(schema, acc)
        json_schema.AllOf(items) -> {
          let #(acc, items) =
            list.map_fold(items |> non_empty_list.to_list, acc, fn(acc, item) {
              let #(top, _, acc) = do_lift(item, acc)
              let #(schema, acc) = not_top(top, acc)
              #(acc, schema)
            })
          #(Tuple(items), False, acc)
        }
        json_schema.AnyOf(NonEmptyList(schema, [])) -> do_lift(schema, acc)
        json_schema.OneOf(NonEmptyList(schema, [])) -> do_lift(schema, acc)
        json_schema.AnyOf(..) | json_schema.OneOf(..) -> #(
          Unsupported,
          False,
          acc,
        )
        // OAS generator doesn't support any validation beyond types
        json_schema.Enum(items) -> {
          let NonEmptyList(first, rest) = items
          case first {
            utils.Boolean(_value) ->
              case all_map(rest, boolean_value) {
                Ok(_rest) -> #(Primitive(Boolean), False, acc)
                Error(Nil) -> #(Unsupported, False, acc)
              }
            utils.Integer(_value) ->
              case all_map(rest, integer_value) {
                Ok(_rest) -> #(Primitive(Integer), False, acc)
                Error(Nil) -> #(Unsupported, False, acc)
              }
            utils.Number(_value) ->
              case all_map(rest, number_value) {
                Ok(_rest) -> #(Primitive(Number), False, acc)
                Error(Nil) -> #(Unsupported, False, acc)
              }
            utils.String(_value) ->
              case all_map(rest, string_value) {
                Ok(_rest) -> #(Primitive(String), False, acc)
                Error(Nil) -> #(Unsupported, False, acc)
              }
            utils.Null ->
              case rest {
                [] -> #(Primitive(Null), False, acc)
                _ -> #(Unsupported, False, acc)
              }
            _ -> #(Unsupported, False, acc)
          }
        }
        json_schema.AlwaysPasses(..) -> #(Primitive(Always), False, acc)
        json_schema.AlwaysFails(..) -> #(Primitive(Never), False, acc)
      }
  }
}

fn all_map(items, func) {
  do_all_map(items, func, [])
}

fn do_all_map(items, func, acc) {
  case items {
    [] -> Ok(list.reverse(acc))
    [item, ..rest] ->
      case func(item) {
        Ok(mapped) -> do_all_map(rest, func, [mapped, ..acc])
        Error(reason) -> Error(reason)
      }
  }
}

fn boolean_value(any) {
  case any {
    utils.Boolean(value) -> Ok(value)
    _ -> Error(Nil)
  }
}

fn integer_value(any) {
  case any {
    utils.Integer(value) -> Ok(value)
    _ -> Error(Nil)
  }
}

fn number_value(any) {
  case any {
    utils.Number(value) -> Ok(value)
    _ -> Error(Nil)
  }
}

fn string_value(any) {
  case any {
    utils.String(value) -> Ok(value)
    _ -> Error(Nil)
  }
}
