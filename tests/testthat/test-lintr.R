context("Code is high quality and lint free")
test_that("Code Lint", {
  skip_on_cran()
  skip_if_not_installed("lintr")
  my_linters <- lintr::linters_with_defaults(
    # absolute_path_linter = lintr::absolute_path_linter,
    # assignment_linter = lintr::assignment_linter,
    # commas_linter = lintr::commas_linter,
    # # commented_code_linter = lintr::commented_code_linter,
    # # infix_spaces_linter = lintr::infix_spaces_linter,
    # # line_length_linter = lintr::line_length_linter,
    # object_length_linter = lintr::Linter(lintr::object_length_linter),
    # # object_usage_linter = lintr::object_usage_linter,
    # spaces_inside_linter = lintr::spaces_inside_linter,
    # # spaces_left_parentheses_linter = lintr::spaces_left_parentheses_linter,
    # trailing_blank_lines_linter = lintr::trailing_blank_lines_linter,
    # trailing_whitespace_linter = lintr::trailing_whitespace_linter,
    defaults = list() # not include default linters
  )
  lintr::expect_lint_free(linters = my_linters)
})
