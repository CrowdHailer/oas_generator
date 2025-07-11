import glance
import gleam/list
import gleam/option.{None}
import gleam/string
import justin

pub fn access(object_or_mod, field) {
  glance.FieldAccess(glance.Variable(object_or_mod), field)
}

pub fn call(f, arg) {
  case f {
    glance.FnCapture(None, f, before, after) -> {
      let args = list.flatten([before, [glance.UnlabelledField(arg)], after])
      glance.Call(f, args)
    }
    _ -> glance.Call(f, [glance.UnlabelledField(arg)])
  }
}

pub fn call0(m, f) {
  glance.Call(access(m, f), [])
}

pub fn call1(m, f, a) {
  glance.Call(access(m, f), [glance.UnlabelledField(a)])
}

pub fn call2(m, f, a, b) {
  glance.Call(access(m, f), [
    glance.UnlabelledField(a),
    glance.UnlabelledField(b),
  ])
}

pub fn pipe(a, b) {
  glance.BinaryOperator(glance.Pipe, a, b)
}

// ------------------------------
fn replace_gleam_keywords(key) {
  case key {
    "as" -> "as_"
    "assert" -> "assert_"
    "auto" -> "auto_"
    "case" -> "case_"
    "const" -> "const_"
    "delegate" -> "delegate_"
    "derive" -> "derive_"
    "echo" -> "echo_"
    "else" -> "else_"
    "fn" -> "fn_"
    "if" -> "if_"
    "implement" -> "implement_"
    "import" -> "import_"
    "let" -> "let_"
    "macro" -> "macro_"
    "opaque" -> "opaque_"
    "panic" -> "panic_"
    "pub" -> "pub_"
    "test" -> "test_"
    "todo" -> "todo_"
    "type" -> "type_"
    "use" -> "use_"
    // used as names within generated fns
    "base" -> "base_"
    "path" -> "path_"
    "method" -> "method_"
    "token" -> "token_"
    _ -> key
  }
}

fn replace_disallowed_charachters(in) {
  in
  |> string.replace("/", "_")
  |> string.replace("+", "_")
  // this is part of kebab casing
  // |> string.replace("-", "Minus")
  |> string.replace("^", "")
  |> string.replace("$", "")
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
  |> replace_disallowed_charachters
  |> justin.pascal_case()
  |> prefix_numbers
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
