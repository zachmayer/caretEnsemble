library(testthat)
source("R/caretEnsemble.R")
source("R/caretStack.R")
source("R/helper_functions.R")
source("R/optAUC.R")
source("R/OptRMSE.R")

library(caret)
library(randomForest)
require("pbapply")

load("data/models_reg.RData")
load("data/models_class.RData")

test_file("tests/testthat/test_ensemble.R")
test_file("tests/testthat/test_helper_functions.R")
test_file("tests/testthat/test_stack.R")


