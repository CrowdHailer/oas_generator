import gleeunit/should
import oas/generator

pub fn negative_field_test() {
  "-1"
  |> generator.name_for_gleam_field_or_var
  |> should.equal("negative_1")
}
