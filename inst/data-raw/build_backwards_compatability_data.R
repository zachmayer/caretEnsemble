# This script is a little big sorry.
# We're using a 3rd party dataset from a package
# It depends on caretEnsemble, and I broke it with the 4.0.0 pre-release
# This script isolates the bad, saved model in that package
# and then removes all the parts of that model that aren't needed to make predictions
# this gives us a minimal test case for the backwards compatability issue
# This script shouldn't ever need to get run again, just use the old saved
# caretlist_with_bad_earth_model.rds file in data in testthat in the tests folder.
# This script is for posterity.

# Note this is not in our depends or suggests. Also note new version may not have the bug.
devtools::install_version("LDLcalc", version = "2.1", repos = "http://cran.us.r-project.org")
devtools::load_all()

# Load the data and fit the model
data(SampleData, package = "LDLcalc")
ldl_model <- LDLcalc:::LDL_ML_train_StackingAlgorithm(SampleData) # nolint undesirable_operator_linter
testthat::expect_s3_class(ldl_model$stackModel, "caretStack")
testthat::expect_s3_class(ldl_model$stackModel$models, "caretList")

# Make a caretList with just the bad model
caretlist_with_old_earth_model <- ldl_model$stackModel$models["earth"]

# Function to test the error and warnings after removing a specific part
test_error <- function(obj, path, SampleData) {
  modified_obj <- obj
  eval(parse(text = paste0("modified_obj", path, " <- NULL")))

  wrns <- NULL

  # Capture both errors and warnings
  result <- tryCatch(
    {
      withCallingHandlers(
        {
          predict(modified_obj, SampleData)
        },
        warning = function(w) {
          wrns <<- c(wrns, conditionMessage(w)) # nolint undesirable_operator_linter
          invokeRestart("muffleWarning")
        }
      )
      list(error = NULL, wrns = wrns)
    },
    error = function(e) {
      list(error = e$message, wrns = wrns)
    }
  )

  result
}

# Function to iteratively prune the object
prune_list_iterative <- function(obj, SampleData) { # nolint cyclocomp_linter
  the_stack <- list(list(obj = obj, path = ""))
  pruned_obj <- obj

  while (length(the_stack) > 0L) {
    # Pop the last element from the the_stack
    current <- the_stack[[length(the_stack)]]
    the_stack <- the_stack[-length(the_stack)]

    if (is.list(current$obj)) {
      keys <- names(current$obj)
      for (key in keys) {
        current_path <- paste0(current$path, "$", key)

        # Test by removing the current element
        result <- test_error(pruned_obj, current_path, SampleData)

        # Determine if we should keep or remove the element
        if ((!is.null(result$error) && result$error != "is.vector(pred) is not TRUE") || !is.null(result$wrns)) {
          # If error changes, goes away, or a warning appears, keep the element
          the_stack <- c(the_stack, list(list(obj = current$obj[[key]], path = current_path)))
        } else {
          # If error remains the same and no wrns, remove the element
          eval(parse(text = paste0("pruned_obj", current_path, " <- NULL")))
        }
      }
    }
  }
  pruned_obj
}

# Start the pruning process
pruned_caretlist <- prune_list_iterative(caretlist_with_old_earth_model, SampleData)

# Prune attributes
attr(pruned_caretlist$earth$terms, ".Environment") <- NULL
attr(pruned_caretlist$earth$terms, "dimnames") <- NULL
attr(pruned_caretlist$earth$terms, "term.labels") <- NULL
attr(pruned_caretlist$earth$terms, "order") <- NULL
attr(pruned_caretlist$earth$terms, "intercept") <- NULL
attr(pruned_caretlist$earth$terms, "response") <- NULL
attr(pruned_caretlist$earth$terms, "predvars") <- NULL
attr(pruned_caretlist$earth$terms, "dataClasses") <- NULL

# Test the final pruned object
# Note that once the bug is fixed, this will no longer fail
# this requires version 4.0.0 of caretEnsemble, prior to the PR fixing the prediciton issue
# https://github.com/zachmayer/caretEnsemble/issues/324
testthat::expect_error(
  predict(pruned_caretlist, SampleData),
  "is.vector(pred) is not TRUE",
  fixed = TRUE
)

# Save
saveRDS(
  pruned_caretlist,
  file.path("tests", "testthat", "data", "caretlist_with_bad_earth_model.rds"),
  ascii = FALSE,
  version = 3L,
  compress = "xz"
)
