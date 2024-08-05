data(models.class)
data(models.reg)
data(iris)

# Helper function to create a simple dataset
create_dataset <- function(n = 200L, p = 5L, classification = TRUE) {
  set.seed(42L)
  X <- data.table::data.table(matrix(rnorm(n * p), ncol = p))
  data.table::setnames(X, paste0("x", seq_len(p)))
  if (classification) {
    y <- factor(ifelse(rowSums(X) > 0L, "A", "B"))
  } else {
    y <- rowSums(X) + rnorm(n)
  }
  list(X = X, y = y)
}

# Helper function to train a model
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

# Helper function to check test results
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

testthat::test_that("isClassifier works for train models", {
  testthat::expect_true(isClassifier(models.class[[1L]]))
  testthat::expect_false(isClassifier(models.reg[[1L]]))
})

testthat::test_that("isClassifier works for caretStacks models", {
  ens_class <- caretEnsemble(models.class)
  ens_reg <- caretEnsemble(models.reg)

  testthat::expect_true(isClassifier(ens_class))
  testthat::expect_false(isClassifier(ens_reg))
})

testthat::test_that("permutationImportance works for regression", {
  dt <- create_dataset(classification = FALSE)
  model <- train_model(dt[["X"]], dt[["y"]])
  imp <- permutationImportance(model, dt[["X"]])
  check_importance_scores(imp)
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
    1.0, -0.5, 0.2, # coefficients for class A
    -0.5, 1.0, 0.2, # coefficients for class B
    0.2, 0.2, 1.0 # coefficients for class C
  ), nrow = 3L, byrow = TRUE)

  linear_combinations <- as.matrix(x) %*% t(coef_matrix)
  linear_combinations <- linear_combinations + matrix(stats::rnorm(n * 3.0, sd = 0.1), nrow = n)
  probabilities <- exp(linear_combinations) / rowSums(exp(linear_combinations))
  y <- factor(apply(probabilities, 1L, function(prob) sample(c("A", "B", "C"), 1L, prob = prob)))

  model <- train_model(x, y)
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, c("x1", "x2", "x3"))
})

testthat::test_that("permutationImportance works with a single feature unimportant feature", {
  n <- 100L
  x <- data.table::data.table(x1 = stats::rnorm(n))
  y <- factor(sample(c("A", "B"), n, replace = TRUE))
  model <- train_model(x, y)
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, "x1")
})

testthat::test_that("permutationImportance works with a single important feature", {
  set.seed(1234L)

  make_var <- function(n) scale(stats::rnorm(n), center = TRUE, scale = TRUE)[, 1L]

  n <- 1000L
  x <- data.table::data.table(
    x1 = make_var(n),
    x2 = make_var(n),
    x3 = make_var(n)
  )

  cf_set <- c(0L, 1L, 5L, 10L)
  all_cfs <- expand.grid(
    c(0L, 1L),
    cf_set,
    cf_set,
    cf_set
  )

  evaluate_model <- function(cf, do_class) {
    cf <- unname(unlist(cf))
    y <- (cbind(1L, as.matrix(x)) %*% cf)[, 1L]
    if (do_class) {
      classes <- c("A", "B")
      y <- factor(ifelse(y > 0L, classes[1L], classes[2L]), levels = classes)
      if (length(unique(y)) == 1L) {
        return(NULL)
      }
    }
    model <- suppressWarnings(train_model(x, y, method = "glm"))
    imp <- permutationImportance(model, x)
    check_importance_scores(imp, c("x1", "x2", "x3"))

    glm_imp <- normalize_to_one(abs(coef(model$finalModel))[-1L])
    cf_norm <- normalize_to_one(cf[-1L])
    testthat::expect_equivalent(glm_imp, cf_norm, tolerance = 0.1)

    if (!do_class || cf[[1L]] == 0.0) {
      testthat::expect_equivalent(imp, cf_norm, tolerance = 0.1)
    }
  }

  for (do_class in c(FALSE, TRUE)) {
    apply(all_cfs, 1L, evaluate_model, do_class)
  }
})

testthat::test_that("permutationImportance works a single, contant, unimportant feature", {
  n <- 100L
  x <- data.table::data.table(
    x1 = rep(1L, n),
    x2 = stats::rnorm(n)
  )
  y <- stats::rnorm(n)
  model <- train_model(x, y)
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, c("x1", "x2"))
  testthat::expect_lte(imp["x1"], imp["x2"])
})

testthat::test_that("permutationImportance works a single, contant, important feature - aka intercept only", {
  n <- 100L
  x <- data.table::data.table(
    x1 = rep(1L, n),
    x2 = stats::rnorm(n)
  )
  y <- x$x1 + stats::rnorm(n) / 10L
  model <- train_model(x, y)
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, c("x1", "x2"))
  testthat::expect_lte(imp["x1"], imp["x2"])
})

testthat::test_that("permutationImportance works with perfect predictor", {
  n <- 100L
  x <- data.table::data.table(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  y <- x$x1
  model <- train_model(x, y, method = "lm")
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, c("x1", "x2"))
  testthat::expect_gt(imp["x1"], imp["x2"])
})

testthat::test_that("permutationImportance works for multiclass classification and various edge cases", {
  model <- train_model(iris[, -5L], iris$Species, method = "rpart")
  imp <- permutationImportance(model, iris[, -5L])
  check_importance_scores(imp, names(iris[, -5L]))
})

testthat::context("permutationImportance edge cases")
testthat::test_that("permutationImportance normalizes to uniform distribution for all zero importances", {
  n <- 100L
  x <- data.table::data.table(x1 = rep(0L, n), x2 = rep(0L, n), x3 = rep(0L, n))
  y <- rep(0L, n)
  model <- train_model(x, y, method = "lm")
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, names(x))
  testthat::expect_equivalent(imp, normalize_to_one(rep(0L, length(imp))), tolerance = 1e-6)
})

testthat::test_that("permutationImportance assigns full importance to perfect predictor", {
  set.seed(1234L)
  n <- 100L
  vars <- 25L
  x <- data.table::data.table(
    matrix(rnorm(n * vars), nrow = n, ncol = vars)
  )
  data.table::setnames(x, paste0("x", seq_len(vars)))
  y <- x$x1
  model <- train_model(x, y, method = "lm")
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, names(x))
  testthat::expect_equal(imp[["x1"]], 1L, tol = 1e-8)
  testthat::expect_equal(sum(imp[-1L]), 0L, tol = 1e-8)
})

testthat::test_that("permutationImportance handles highly collinear features", {
  set.seed(5678L)
  n <- 100L
  x <- data.table::data.table(
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  x$x3 <- x$x1 + rnorm(n, sd = 0.01)
  y <- x$x1 + x$x2
  model <- train_model(x, y, method = "lm")
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, names(x))
  testthat::expect_equal(imp[["x3"]], 0L, tol = 1e-6)
})

testthat::test_that("permutationImportance works with very small dataset", {
  set.seed(9876L)
  n <- 5L
  x <- data.table::data.table(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )
  y <- x$x1 + rnorm(n)
  model <- train_model(x, y, method = "lm")
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, names(x))
})

testthat::test_that("permutationImportance handles identical features", {
  n <- 100L
  x <- data.table::data.table(
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  x$x3 <- x$x1
  y <- x$x1 + x$x2 + rnorm(n)
  model <- train_model(x, y, method = "glmnet")
  imp <- permutationImportance(model, x)
  check_importance_scores(imp, names(x))
  testthat::expect_equal(imp[["x1"]], imp[["x3"]], tol = 1e-1)
})
