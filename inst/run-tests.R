library(testthat)
source("R/caretEnsemble.R")
source("R/caretStack.R")
source("R/helper_functions.R")
source("R/optAUC.R")
source("R/OptRMSE.R")

library(caret)
require("pbapply")

load("data/models_reg.RData")
load("data/models_class.RData")

test_file("inst/test/test_ensemble.R")
test_file("inst/test/test_helper_functions.R")
test_file("inst/test/test_stack.R")


