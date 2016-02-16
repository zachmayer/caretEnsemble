context("Does binary class selection work?")
library(caret)
library(caretEnsemble)

# library(devtools); unload(inst("caretEnsemble")); install_local('/Users/eczech/repos/misc/caretEnsemble');

data(models.class)
data(X.class)
data(Y.class)

custom.rf <- getModelInfo('rf', regex=F)[[1]]
custom.rf$method <- 'custom-rf'

caretList(X.class, Y.class, methodList = list(custom.rf, custom.rf, 'rf', 'rf'))

test_that("Ensembled classifiers do not rearrange outcome factor levels", {
  skip_on_cran()


})
