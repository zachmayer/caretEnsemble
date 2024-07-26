
#####################################################
# Extraction functions - train
#####################################################

#' @title Extract the method name associated with a single train object
#' @description Extracts the method name associated with a single train object. Note
#' that for standard models (i.e. those already prespecified by caret), the
#' "method" attribute on the train object is used directly while for custom
#' models the "method" attribute within the model$modelInfo attribute is
#' used instead.
#' @param x a single caret train object
#' @return Name associated with model
extractModelName <- function(x) {
  if (is.list(x$method)) {
    checkCustomModel(x$method)$method
  } else if (x$method == "custom") {
    checkCustomModel(x$modelInfo)$method
  } else {
    x$method
  }
}

#' @title Extract the best predictions from a train object
#' @description Extract predictions for the best tune from a model
#' @param x a train object
#' @importFrom data.table data.table setorderv set
extractBestPreds <- function(x) {
  # Checks
  stopifnot(
    is(x, "train"),
    x$control$savePredictions %in% c("all", "final", TRUE)
  )

  # Extract the best tune and the pred data
  a <- data.table::data.table(x$bestTune, key = names(x$bestTune))
  b <- data.table::data.table(x$pred, key = names(x$bestTune))

  # Subset pred data to the best tune only
  b <- b[a, ]

  # Remove some columns we don't need and order
  for (var in names(x$bestTune)) {
    data.table::set(b, j = var, value = NULL)
  }
  data.table::setorderv(b, c("Resample", "rowIndex"))

  # Returb
  invisible(gc(reset = TRUE))
  b
}
