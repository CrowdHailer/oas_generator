import gleam/list
import gleam/option.{type Option}

pub type Lookup(t) {
  Lookup(ref: String, next: fn(Option(String), String) -> Lookup(t))
  Done(t)
}

pub fn then(lookup: Lookup(t), next: fn(t) -> Lookup(u)) -> Lookup(u) {
  case lookup {
    Lookup(ref, inner) ->
      Lookup(ref, fn(mod, name) { then(inner(mod, name), next) })
    Done(value) -> next(value)
  }
}

pub fn seq(lookups: List(Lookup(t))) -> Lookup(List(t)) {
  do_seq(lookups, [])
}

fn do_seq(lookups, acc) {
  case lookups {
    [] -> Done(list.reverse(acc))
    [Done(value), ..rest] -> do_seq(rest, [value, ..acc])
    [Lookup(ref, inner), ..rest] ->
      Lookup(ref, fn(mod, name) { do_seq([inner(mod, name), ..rest], acc) })
  }
}

pub fn fold(
  items: List(t),
  initial: a,
  func: fn(a, t) -> Lookup(a),
) -> Lookup(a) {
  case items {
    [] -> Done(initial)
    [next, ..rest] -> {
      use acc <- then(func(initial, next))
      fold(rest, acc, func)
    }
  }
}

pub fn map_fold(
  items: List(t),
  initial: a,
  func: fn(a, t) -> Lookup(#(a, b)),
) -> Lookup(#(a, List(b))) {
  use #(inner, buffer) <- then(
    fold(items, #(initial, []), fn(acc, item) {
      let #(inner, buffer) = acc
      use #(inner, next) <- then(func(inner, item))
      Done(#(inner, [next, ..buffer]))
    }),
  )
  Done(#(inner, list.reverse(buffer)))
}
