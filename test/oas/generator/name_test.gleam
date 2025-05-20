import gleeunit/should
import oas/generator

pub fn negative_field_test() {
  "-1"
  |> generator.name_for_gleam_field_or_var
  |> should.equal("negative_1")
}

pub fn rail_op_test() {
  "20220120_get_arrival_board_response"
  |> generator.name_for_gleam_field_or_var
  |> should.equal("n20220120_get_arrival_board_response")
}

pub fn rail_op_underscore_test() {
  "_20220120_get_arrival_board_response"
  |> generator.name_for_gleam_field_or_var
  |> should.equal("n20220120_get_arrival_board_response")
}
