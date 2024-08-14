
# Note this is not in our depends or suggests. Also note new version may not have the bug.
devtools::install_version("LDLcalc", version = "2.1", repos = "http://cran.us.r-project.org")
devtools::load_all()

# Helper
object_storage_report <- function(x){
  sizes <- sort(sapply(x, object.size))
  sizes <- sizes / sum(sizes)
  print(sizes)
}

# Load the data and fit the model
data(SampleData, package='LDLcalc')
ldl_model = LDLcalc:::LDL_ML_train_StackingAlgorithm(SampleData)
testthat::expect_s3_class(ldl_model$stackModel, "caretStack")
testthat::expect_s3_class(ldl_model$stackModel$models, "caretList")

# Make a caretList with just the bad model
caretlist_with_old_earth_model <- ldl_model$stackModel$models['earth']

# Trim
# https://github.com/topepo/caret/blob/master/pkg/caret/R/trim.R#L4
object <- caretlist_with_old_earth_model[['earth']]
  removals <- c(
    "results", "pred", "bestTune", "call", "dots",
    "metric", "trainingData", "resample", "resampledCM",
    "perfNames", "maxmimize", "times")
  for(i in removals)
    if(i %in% names(object)) object[i] <- NULL
  c_removals <- c(
    'method', 'number', 'repeats', 'p', 'initialWindow', 
    'horizon', 'fixedWindow', 'verboseIter', 'returnData', 
    'returnResamp', 'savePredictions', 'summaryFunction', 
    'selectionFunction', 'index', 'indexOut', 'indexFinal',
    'timingSamps', 'trim', 'yLimits')
  for(i in c_removals)
    if(i %in% names(object$control)) object$control[i] <- NULL  
  if(!is.null(object$modelInfo$trim))
    object$finalModel <- object$modelInfo$trim(object$finalModel)

object$finalModel$call <- NULL
object$finalModel$x <- NULL
object$finalModel$y <- NULL
object$finalModel$bx <- NULL
object$finalModel$residuals <- NULL
object$finalModel$fitted.values <- NULL
object$finalModel$leverages <- NULL
object$finalModel$leverages <- NULL
object$modelInfo$fit <- NULL
object$modelInfo$prob <- NULL
object$modelInfo$loop <- NULL
object$modelInfo$varimp <- NULL
object$modelInfo$grid <- NULL
object$modelInfo$predictors <- NULL
object$modelInfo$sort <- NULL
object_storage_report(object$modelInfo)

# Look at storage
caretlist_with_old_earth_model[['earth']] <- object
object.size(caretlist_with_old_earth_model)

# Confirm error
predict(caretlist_with_old_earth_model, SampleData)

# This is the error we're testing for
# Note future versions of caretEnsemble will fix this problem
testthat::expect_error(
  predict(caretlist_with_old_earth_model, SampleData), 
  "is.vector(pred) is not TRUE", 
  fixed=T
)

usethis::use_data(
  caretlist_with_old_earth_model,
  overwrite = TRUE,
  compress = "xz",
  version = 3
)
