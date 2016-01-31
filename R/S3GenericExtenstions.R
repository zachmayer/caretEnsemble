## Extensions to generic S3 functions ##
# (someone should spell check this)

#' @title S3 definition for concatenating caretList
#'
#' @description take N objects of class caretList and concatenat them into a larger object of
#' class caretList for future Ensamble'ing
#'
#' @param ... the objects of class caretList or train to bind into a caretList
#' @return a \code{\link{caretList}} object
#' @export
#' @examples
#' \dontrun{
#'  model_list1 <- caretList(Class ~ .,
#'   data=Sonar, trControl = ctrl1,
#'   tuneList = list(
#'                  glm=caretModelSpec(method='glm', family='binomial'),
#'                  rpart=caretModelSpec(method='rpart')
#'                  ),
#'    metric='ROC')
#'
#' model_list2 <- caretList(Class ~ .,
#'                          data=Sonar,
#'                          trControl = ctrl1,
#'                          tuneList = list(
#'                            glm=caretModelSpec(method='rpart'),
#'                            rpart=caretModelSpec(method='rf')
#'                          ),
#'                          metric='ROC')
#'
#'  bigList <- c(model_list1, model_list2)
#' }
#'
c.caretList <- function(...) {

  new_model_list <- unlist(lapply(list(...), function(x) {
    if(class(x)[1] != "caretList") {
      if(class(x)[1] != "train") stop("class of modelList1 must be 'caretList' or 'train'")

      ## assuming this is a single train object
      x <- list(x)
      names(x) <- x[[1]]$method
      return(x)
    } else {
      return(x)
    }
  }), recursive = FALSE)

  ## Make sure names are unique
  names(new_model_list) <- paste0(names(new_model_list), 1:length(new_model_list))

  ## reset the class to caretList
  class(new_model_list) <- "caretList"

  return(new_model_list)
}


#' @title S3 definition for concatenating train objects
#'
#' @description take N objects of class train and concatenat into an object of class caretList for future Ensamble'ing
#'
#' @param ... the objects of class train to bind into a caretList
#' @return a \code{\link{caretList}} object
#' @export
#' @examples
#' \dontrun{
#' rpartTrain <- train(Class ~ .,
#'                     data=Sonar,
#'                     trControl = ctrl1,
#'                     method='rpart')
#'
#' rfTrain <- train(Class ~ .,
#'                  data=Sonar,
#'                  trControl = ctrl1,
#'                  method='rf')
#'
#'  bigList <- c(model_list1, model_list2)
#' }
#'
c.train <- function(...) {

  new_model_list <- unlist(lapply(list(...), function(x) {
    if(class(x)[1] != "caretList") {
      if(class(x)[1] != "train") stop("class of modelList1 must be 'caretList' or 'train'")

      ## assuming this is a single train object
      x <- list(x)
      names(x) <- x[[1]]$method
      return(x)
    } else {
      return(x)
    }
  }), recursive = FALSE)

  ## Make sure names are unique
  names(new_model_list) <- paste0(names(new_model_list), 1:length(new_model_list))

  ## reset the class to caretList
  class(new_model_list) <- "caretList"

  return(new_model_list)
}
