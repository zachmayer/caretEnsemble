#' @title caretTrain
#' @description
#'  This function is a wrapper around the `train` function from the `caret` package.
#'  It allows for the passing of local and global arguments to the `train` function.
#'  It also allows for the option to continue on fail, and to trim the output model.
#'  Trimming the model removes components that are not needed for stacking, to save
#'  memory and speed up the stacking process. It also converts preds to a data.table.
#' @param local_args A list of arguments to pass to the `train` function.
#' @param global_args A list of arguments to pass to the `train` function.
#' @param continue_on_fail A logical indicating whether to continue if the `train` function fails.
#'  If `TRUE`, the function will return `NULL` if the `train` function fails.
#' @param trim A logical indicating whether to trim the output model.
#' If `TRUE`, the function will remove some elements that are not needed from the output model.
#' @return The output of the `train` function.
#' @keywords internal
caretTrain <- function(local_args, global_args, continue_on_fail = FALSE, trim = TRUE) {
  # Combine args
  # TODO a test for handling arg collisions (e.g. dupe in global vs local)
  # I think my handling here is correct (update globals with locals, which allows locals be partial)
  # but it would be nice to have some tests
  model_args <- utils::modifyList(global_args, local_args)

  # Fit
  if (continue_on_fail) {
    model <- tryCatch(do.call(train, model_args), error = function(e) {
      warning(conditionMessage(e))
      NULL
    })
  } else {
    model <- do.call(train, model_args)
  }

  # Use data.table for stacked predictions
  if ("pred" %in% names(model)) {
    model[["pred"]] <- data.table::data.table(model[["pred"]])
  }

  if (trim) {
    # Remove some elements that are not needed from the train model
    removals <- c("call", "dots", "trainingData", "resampledCM")
    for (i in removals) {
      if (i %in% names(model)) {
        model[[i]] <- NULL
      }
    }

    # Remove some elements that are not needed from the model control (within the train model)
    c_removals <- c("index", "indexOut", "indexFinal")
    for (i in c_removals) {
      if (i %in% names(model[["control"]])) {
        model[["control"]][[i]] <- NULL
      }
    }
  }

  # Return
  model
}
