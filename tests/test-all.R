library(testthat)
library(caretEnsemble)
test_check("caretEnsemble")


test_file("tests/testthat/test-caretList.R", reporter = "summary")
test_file("tests/testthat/test-ensemble.R", reporter = "summary")
test_file("tests/testthat/test-helper_functions.R", reporter = "minimal")
test_file("tests/testthat/test-caretStack.R", reporter = "minimal")
