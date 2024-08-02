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
    trControl = caret::trainControl(
      method = "cv",
      number = 5L
    )
  )
}

# Helper function to check test results
check_importance_scores <- function(
    imp,
    expected_names = c("intercept", paste0("x", seq_len(5L))),
    expected_length = length(expected_names)) {
  testthat::expect_type(imp, "double")
  testthat::expect_true(all(is.finite(imp)))
  testthat::expect_named(imp, expected_names)
  testthat::expect_length(imp, expected_length)
  testthat::expect_true(all(imp >= 0L & imp <= 1L))
  testthat::expect_equal(sum(imp), 1L, tolerance = 1e-6)
}

testthat::test_that("permutationImportance works for binary classification", {
  dt <- create_dataset(classification = TRUE)
  model <- train_model(dt[["X"]], dt[["y"]])
  imp <- permutationImportance(model, dt[["X"]], dt[["y"]])
  check_importance_scores(imp)
})

testthat::test_that("permutationImportance works for regression", {
  dt <- create_dataset(classification = FALSE)
  model <- train_model(dt[["X"]], dt[["y"]])
  imp <- permutationImportance(model, dt[["X"]], dt[["y"]])
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
  imp <- permutationImportance(model, x, y)
  check_importance_scores(imp, c("intercept", "x1", "x2", "x3"))
})

testthat::test_that("permutationImportance works with a single feature unimportant feature", {
  n <- 100L
  x <- data.table::data.table(x1 = stats::rnorm(n))
  y <- factor(sample(c("A", "B"), n, replace = TRUE))
  model <- train_model(x, y)
  imp <- permutationImportance(model, x, y)
  check_importance_scores(imp, c("intercept", "x1"))
})


# TODO: parameterize
testthat::test_that("permutationImportance works with a single feature important feature", {
  set.seed(1234L)
  make_var <- function(n) scale(stats::rnorm(n), center = TRUE, scale = TRUE)[, 1L]

  n <- 1000L
  x <- data.table::data.table(
    x1 = make_var(n),
    x2 = make_var(n),
    x3 = make_var(n)
  )

  # Create a perfectly linear relationship
  intercept <- 1L
  cf <- c(2L, 7L, 0L)
  y <- (intercept + as.matrix(x) %*% cf)[, 1L]
  model <- train_model(x, y, method = "lm")

  cf <- coef(model$finalModel)
  cf_im <- abs(cf) / sum(abs(cf))
  imp <- permutationImportance(model, x, y)
  check_importance_scores(imp, c("intercept", "x1", "x2", "x3"))

  testthat::expect_equivalent(cf_im, imp, tolerance = 0.1)
})

testthat::test_that("permutationImportance works a single, contant, unimportant feature", {
  n <- 100L
  x <- data.table::data.table(
    x1 = rep(1L, n),
    x2 = stats::rnorm(n)
  )
  y <- factor(sample(c("A", "B"), n, replace = TRUE))
  model <- train_model(x, y)
  imp <- permutationImportance(model, x, y)
  check_importance_scores(imp, c("intercept", "x1", "x2"))
  testthat::expect_lt(imp["x1"], imp["x2"])
})

testthat::test_that("permutationImportance works a single, contant, important feature", {
  n <- 100L
  x <- data.table::data.table(
    x1 = rep(1L, n),
    x2 = stats::rnorm(n)
  )
  y <- factor(sample(c("A", "B"), n, replace = TRUE))
  model <- train_model(x, y)
  imp <- permutationImportance(model, x, y)
  check_importance_scores(imp, c("intercept", "x1", "x2"))
  testthat::expect_lt(imp["x1"], imp["x2"])
})

testthat::test_that("permutationImportance works with perfect predictor", {
  n <- 100L
  x <- data.table::data.table(
    x1 = rep(c(0L, 1L), each = n / 2L),
    x2 = stats::rnorm(n)
  )
  y <- factor(rep(c("A", "B"), each = n / 2L))
  model <- train_model(x, y)
  imp <- permutationImportance(model, x, y)
  check_importance_scores(imp, c("intercept", "x1", "x2"))
  testthat::expect_gt(imp["x1"], imp["x2"])
})
