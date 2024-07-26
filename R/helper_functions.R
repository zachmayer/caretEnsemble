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
