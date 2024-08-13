# Helper functions
utils::data(models.class)
utils::data(models.reg)
utils::data(iris)

create_dataset <- function(n = 200L, p = 5L, classification = TRUE) {
  set.seed(42L)
  X <- data.table::data.table(matrix(stats::rnorm(n * p), ncol = p))
  data.table::setnames(X, paste0("x", seq_len(p)))
  if (classification) {
    y <- factor(ifelse(rowSums(X) > 0L, "A", "B"))
  } else {
    y <- rowSums(X) + stats::rnorm(n)
  }
  list(X = X, y = y)
}

train_model <- function(x, y, method = "rpart", ...) {
  set.seed(1234L)
  caret::train(
    x = x,
    y = y,
    method = method,
    ...,
    trControl = caret::trainControl(method = "none")
  )
}

check_importance_scores <- function(
    imp,
    expected_names = paste0("x", seq_len(5L)),
    expected_length = length(expected_names)) {
  testthat::expect_type(imp, "double")
  testthat::expect_true(all(is.finite(imp)))
  testthat::expect_named(imp, expected_names)
  testthat::expect_length(imp, expected_length)
  testthat::expect_true(all(imp >= 0L & imp <= 1L))
  testthat::expect_equal(sum(imp), 1L, tolerance = 1e-6)
}

######################################################################
testthat::context("isClassifier function")
######################################################################

testthat::test_that("isClassifier works for train models and caretStacks models", {
  testthat::expect_true(isClassifier(models.class[[1L]]))
  testthat::expect_false(isClassifier(models.reg[[1L]]))

  ens_class <- caretEnsemble::caretEnsemble(models.class)
  ens_reg <- caretEnsemble::caretEnsemble(models.reg)

  testthat::expect_true(isClassifier(ens_class))
  testthat::expect_false(isClassifier(ens_reg))
})

######################################################################
testthat::context("permutationImportance function")
######################################################################

testthat::test_that("permutationImportance works for regression and classification", {
  # Regression
  dt_reg <- create_dataset(classification = FALSE)
  model_reg <- train_model(dt_reg[["X"]], dt_reg[["y"]])
  imp_reg <- permutationImportance(model_reg, dt_reg[["X"]])
  check_importance_scores(imp_reg)

  # Classification
  dt_class <- create_dataset(classification = TRUE)
  model_class <- train_model(dt_class[["X"]], dt_class[["y"]])
  imp_class <- permutationImportance(model_class, dt_class[["X"]])
  check_importance_scores(imp_class)
})

testthat::test_that("permutationImportance works for multiclass classification", {
  set.seed(2468L)
  n <- 300L
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n)
  )
  coef_matrix <- matrix(c(
    1.0, -0.5, 0.2,
    -0.5, 1.0, 0.2,
    0.2, 0.2, 1.0
  ), nrow = 3L, byrow = TRUE)

  linear_combinations <- as.matrix(x) %*% t(coef_matrix)
  linear_combinations <- linear_combinations + matrix(stats::rnorm(n * 3L, sd = 0.1), nrow = n)
  probabilities <- exp(linear_combinations) / rowSums(exp(linear_combinations))
  y <- factor(apply(probabilities, 1L, function(prob) sample(c("A", "B", "C"), 1L, prob = prob)))

  model <- train_model(x, y)
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, c("x1", "x2", "x3"))
})

testthat::test_that("permutationImportance works with single feature cases", {
  n <- 100L

  # Unimportant feature
  x_unimp <- data.table::data.table(x1 = stats::rnorm(n))
  y_unimp <- factor(sample(c("A", "B"), n, replace = TRUE))
  model_unimp <- train_model(x_unimp, y_unimp)
  imp_unimp <- permutationImportance(model_unimp, x_unimp)
  check_importance_scores(imp_unimp, "x1")

  # Important feature
  x_imp <- data.table::data.table(x1 = stats::rnorm(n))
  y_imp <- x_imp$x1 + stats::rnorm(n, sd = 0.1)
  model_imp <- train_model(x_imp, y_imp, method = "lm")
  imp_imp <- permutationImportance(model_imp, x_imp)
  check_importance_scores(imp_imp, "x1")
  testthat::expect_gt(imp_imp["x1"], 0.9)
})

testthat::test_that("permutationImportance works with constant features", {
  n <- 100L

  # Constant unimportant feature
  x_const_unimp <- data.table::data.table(x1 = rep(1L, n), x2 = stats::rnorm(n))
  y_const_unimp <- stats::rnorm(n)
  model_const_unimp <- train_model(x_const_unimp, y_const_unimp)
  imp_const_unimp <- permutationImportance(model_const_unimp, x_const_unimp)
  check_importance_scores(imp_const_unimp, c("x1", "x2"))
  testthat::expect_lte(imp_const_unimp["x1"], imp_const_unimp["x2"])

  # Constant important feature (intercept only)
  x_const_imp <- data.table::data.table(x1 = rep(1L, n), x2 = stats::rnorm(n))
  y_const_imp <- x_const_imp$x1 + stats::rnorm(n, sd = 0.1)
  model_const_imp <- train_model(x_const_imp, y_const_imp)
  imp_const_imp <- permutationImportance(model_const_imp, x_const_imp)
  check_importance_scores(imp_const_imp, c("x1", "x2"))
  testthat::expect_lte(imp_const_imp["x2"], imp_const_imp["x1"])
})

testthat::test_that("permutationImportance works with perfect predictor", {
  n <- 100L
  x <- data.table::data.table(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  y <- x$x1
  model <- train_model(x, y, method = "lm")
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, c("x1", "x2"))
  testthat::expect_gt(imp["x1"], 0.9)
  testthat::expect_lt(imp["x2"], 0.1)
})

testthat::test_that("permutationImportance works for multiclass classification with iris dataset", {
  model <- train_model(iris[, -5L], iris$Species, method = "rpart")
  imp <- permutationImportance(model, iris[, -5L])
  check_importance_scores(imp, names(iris[, -5L]))
})

######################################################################
testthat::context("permutationImportance edge cases")
######################################################################

testthat::test_that("permutationImportance handles various edge cases", {
  n <- 100L
  vars <- 25L

  # All zero importances
  x_zero <- data.table::data.table(matrix(0L, nrow = n, ncol = 3L))
  y_zero <- rep(0L, n)
  model_zero <- train_model(x_zero, y_zero, method = "lm")
  imp_zero <- permutationImportance(model_zero, x_zero)
  check_importance_scores(imp_zero, names(x_zero))
  testthat::expect_equivalent(imp_zero, normalize_to_one(rep(0L, length(imp_zero))), tolerance = 1e-6)

  # Perfect predictor among many variables
  x_perfect <- data.table::data.table(matrix(stats::rnorm(n * vars), nrow = n, ncol = vars))
  data.table::setnames(x_perfect, paste0("x", seq_len(vars)))
  y_perfect <- x_perfect$x1
  model_perfect <- train_model(x_perfect, y_perfect, method = "lm")
  imp_perfect <- permutationImportance(model_perfect, x_perfect)
  check_importance_scores(imp_perfect, names(x_perfect))
  testthat::expect_equal(imp_perfect[["x1"]], 1L, tol = 1e-8)
  testthat::expect_equal(sum(imp_perfect[-1L]), 0L, tol = 1e-8)

  # Highly collinear features
  x_collinear <- data.table::data.table(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  x_collinear$x3 <- x_collinear$x1 + stats::rnorm(n, sd = 0.01)
  y_collinear <- x_collinear$x1 + x_collinear$x2
  model_collinear <- train_model(x_collinear, y_collinear, method = "lm")
  imp_collinear <- permutationImportance(model_collinear, x_collinear)
  check_importance_scores(imp_collinear, names(x_collinear))
  testthat::expect_lt(imp_collinear[["x3"]], 0.1)

  # Very small dataset
  x_small <- data.table::data.table(
    x1 = stats::rnorm(5L),
    x2 = stats::rnorm(5L),
    x3 = stats::rnorm(5L)
  )
  y_small <- x_small$x1 + stats::rnorm(5L)
  model_small <- train_model(x_small, y_small, method = "lm")
  imp_small <- permutationImportance(model_small, x_small)
  check_importance_scores(imp_small, names(x_small))

  # Identical features
  x_identical <- data.table::data.table(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  x_identical$x3 <- x_identical$x1
  y_identical <- x_identical$x1 + x_identical$x2 + stats::rnorm(n)
  model_identical <- train_model(x_identical, y_identical, method = "glmnet")
  imp_identical <- permutationImportance(model_identical, x_identical)
  check_importance_scores(imp_identical, names(x_identical))
  testthat::expect_equal(imp_identical[["x1"]], imp_identical[["x3"]], tol = 1e-1)
})
######################################################################
testthat::context("NAN predictions from rpart")
######################################################################

testthat::test_that("permutationImportance handles NAN predictions from rpart", {
  set.seed(42L)
  model_list <- caretEnsemble::caretList(
    x = iris[, 1L:4L],
    y = iris[, 5L],
    methodList = "rpart"
  )
  ens <- caretEnsemble(model_list)
  imp <- caret::varImp(ens)
  testthat::expect_true(all(is.finite(imp)))
})
