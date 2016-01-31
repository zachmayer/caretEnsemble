#' caretEnsemble: Make ensembles of caret models.
#'
#' Functions for creating ensembles of caret models: caretList and caretStack
#' @docType package
#' @name caretEnsemble
#' @importFrom graphics plot
#' @importFrom methods is
#' @importFrom stats coef median model.frame model.response predict qnorm reshape resid residuals weighted.mean weights
NULL

#' @title caretList of classification models
#' @name models.class
#' @description Data for the caretEnsemble package
#' @docType data
#' @rdname data
#' @author Zachary Deane-Mayer \email{zach.mayer@@gmail.com}
#' @keywords data
NULL

#' @title caretList of regression models
#' @name models.reg
#' @docType data
#' @rdname data
#' @author Zachary Deane-Mayer \email{zach.mayer@@gmail.com}
#' @keywords data
NULL

#' @title data for classification
#' @name X.class
#' @docType data
#' @rdname data
#' @author Zachary Deane-Mayer \email{zach.mayer@@gmail.com}
#' @keywords data
NULL

#' @title data for classification
#' @name Y.class
#' @docType data
#' @rdname data
#' @references \url{data_blah.com}
#' @keywords data
NULL

#' @title data for classification
#' @name X.reg
#' @docType data
#' @rdname data
#' @author Zachary Deane-Mayer \email{zach.mayer@@gmail.com}
#' @keywords data
NULL

#' @title data for regression
#' @name Y.reg
#' @docType data
#' @rdname data
#' @author Zachary Deane-Mayer \email{zach.mayer@@gmail.com}
#' @keywords data
NULL

#Hack to make data.table functions work with devtools::load_all
#http://stackoverflow.com/questions/23252231/r-data-table-breaks-in-exported-functions
#http://r.789695.n4.nabble.com/Import-problem-with-data-table-in-packages-td4665958.html
assign(".datatable.aware", TRUE)

#Avoid false positives in R CMD CHECK:
utils::globalVariables(
  c(".fitted", ".resid", "method", "id", "yhat",
    "ymax", "yavg", "ymin", "metric", "metricSD", "n"))
