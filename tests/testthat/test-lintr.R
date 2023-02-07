context("Code is high quality and lint free")
test_that("Code Lint", {
  skip_on_cran()
  skip_if_not_installed("lintr")
  my_linters <- list(
    lintr::assignment_linter(),
    lintr::brace_linter(),
    lintr::commas_linter(),
    #lintr::commented_code_linter(),
    #lintr::infix_spaces_linter(), #(73)
    #lintr::line_length_linter(), #(181)
    lintr::no_tab_linter(),
    #lintr::object_usage_linter(),
    #lintr::snake_case_linter(),
    lintr::object_length_linter(),
    lintr::single_quotes_linter(),
    lintr::spaces_inside_linter(),
    #lintr::spaces_left_parentheses_linter(),
    lintr::trailing_blank_lines_linter(),
    lintr::trailing_whitespace_linter(),
    lintr::line_length_linter(120)
  )
  lintr::lint_package(linters=my_linters)
})
