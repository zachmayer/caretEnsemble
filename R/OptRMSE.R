
#' TODO
#' @param X
#' @param Y
#' @param iter
#' @export
greedOptRMSE <- function(X, Y, iter = 100L){
  
  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- 0 * X
  sum.weights <- 0L
  
  while(sum.weights < iter) {
    
    sum.weights   <- sum.weights + 1L
    pred          <- (pred + X) * (1L / sum.weights)
    errors        <- sqrt(colSums((pred - Y) ^ 2L))
    best          <- which.min(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
  }
  return(weights)
}


#' TODO
#' @param X
#' @param Y
#' @export
lmOpt <- function(X, Y){
  require('caret')
  model <- lm.fit(X, Y)
  weights <- coef(model)
  names(weights) <- NULL
  return(weights)
}


#' TODO
#' @param X
#' @param Y
#' @export
glmnetOpt <- function(X, Y){
  require('caret')
  model <- train(X, Y, method='glmnet', 
                 trControl=trainControl(method='cv', number=16),
                 tuneGrid=expand.grid(.alpha=c(0, .5, 1), .lambda=seq(0, 5, length=25))
  )
  weights <- as.numeric(coef(model$finalModel, model$bestTune$.lambda))[-1]
  names(weights) <- NULL
  return(weights)
}
