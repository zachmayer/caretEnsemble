#' caretEnsemble: Make ensembles of caret models.
#'
#' Functions for creating ensembles of caret models: caretList and caretStack
#' @docType package
#' @aliases caretEnsemble-package
#' @name caretEnsemble
#' @importFrom data.table .SD
#' @importFrom rlang .data
"_PACKAGE"

#' @title caretList of classification models
#' @name models.class
#' @description Data for the caretEnsemble package
#' @docType data
#' @author Zachary Deane-Mayer \email{zach.mayer@@gmail.com}
#' @keywords data internal
NULL

#' @title caretList of regression models
#' @name models.reg
#' @docType data
#' @author Zachary Deane-Mayer \email{zach.mayer@@gmail.com}
#' @keywords data internal
NULL

#' @title data for classification
#' @name X.class
#' @docType data
#' @author Zachary Deane-Mayer \email{zach.mayer@@gmail.com}
#' @keywords data internal
NULL

#' @title data for classification
#' @name Y.class
#' @docType data
#' @keywords data internal
NULL

#' @title data for classification
#' @name X.reg
#' @docType data
#' @author Zachary Deane-Mayer \email{zach.mayer@@gmail.com}
#' @keywords data internal
NULL

#' @title data for regression
#' @name Y.reg
#' @docType data
#' @author Zachary Deane-Mayer \email{zach.mayer@@gmail.com}
#' @keywords data internal
NULL

# Hack to make data.table functions work with devtools::load_all
# http://stackoverflow.com/questions/23252231/r-data-table-breaks-in-exported-functions
# http://r.789695.n4.nabble.com/Import-problem-with-data-table-in-packages-td4665958.html
assign(".datatable.aware", TRUE)
