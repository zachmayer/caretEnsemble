testthat::test_that("LDLcalc package model", {
    
    # The LDLcalc package has a saved model that's an old caretStack
    # with old train models in it. Some of them make predictions
    # that are vectors, not lists. This is a test to make sure that
    # these old models still work

})
