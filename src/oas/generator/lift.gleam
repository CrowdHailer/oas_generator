import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import non_empty_list.{NonEmptyList}
import oas

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
    oas.Ref(ref:, ..) -> #(Named(ref), False, acc)
    oas.Inline(schema) ->
      case schema {
        oas.Boolean(nullable:, ..) -> #(Primitive(Boolean), nullable, acc)
        oas.Integer(nullable:, ..) -> #(Primitive(Integer), nullable, acc)
        oas.Number(nullable:, ..) -> #(Primitive(Number), nullable, acc)
        oas.String(nullable:, ..) -> #(Primitive(String), nullable, acc)
        oas.Null(..) -> #(Primitive(Null), False, acc)
        oas.Array(nullable:, items:, ..) -> {
          let #(top, _, acc) = do_lift(items, acc)
          let #(schema, acc) = not_top(top, acc)
          #(Array(schema), nullable, acc)
        }
        oas.Object(
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
                None | Some(oas.Inline(oas.AlwaysFails)) -> #(None, acc)
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
        oas.AllOf(NonEmptyList(schema, [])) -> do_lift(schema, acc)
        oas.AllOf(items) -> {
          let #(acc, items) =
            list.map_fold(items |> non_empty_list.to_list, acc, fn(acc, item) {
              let #(top, _, acc) = do_lift(item, acc)
              let #(schema, acc) = not_top(top, acc)
              #(acc, schema)
            })
          #(Tuple(items), False, acc)
        }
        oas.AnyOf(NonEmptyList(schema, [])) -> do_lift(schema, acc)
        oas.OneOf(NonEmptyList(schema, [])) -> do_lift(schema, acc)
        oas.AnyOf(..) | oas.OneOf(..) -> #(Unsupported, False, acc)

        oas.AlwaysPasses(..) -> #(Primitive(Always), False, acc)
        oas.AlwaysFails(..) -> #(Primitive(Never), False, acc)
      }
  }
}
