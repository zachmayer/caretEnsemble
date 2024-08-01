# Helper function to create a simple dataset
create_dataset <- function(n = 200L, p = 5L, classification = TRUE) {
  set.seed(42)
  X <- data.table::data.table(matrix(rnorm(n * p), ncol = p))
  data.table::setnames(X, paste0("x", seq_len(p)))
  if (classification) {
    y <- factor(ifelse(rowSums(X) > 0, "A", "B"))
  } else {
    y <- rowSums(X) + rnorm(n)
  }
  list(X=X, y=y)
}

# Helper function to train a model
train_model <- function(x, y, method = "rpart") {
  set.seed(1234)
  caret::train(
    x=x, 
    y=y, 
    method = method,
    trControl = trainControl(
        method = "cv",
        number = 5
        )
    )
}

# Helper function to check test results
check_importance_scores <- function(imp, expected_names=paste0('x', seq_len(5L)), expected_length = length(expected_names)) {
  testthat::expect_type(imp, "double")
  testthat::expect_named(imp, expected_names)
  testthat::expect_length(imp, expected_length)
  testthat::expect_true(all(imp >= 0 & imp <= 1))
  testthat::expect_equal(sum(imp), 1, tolerance = 1e-6)
}

testthat::test_that("permutationImportance works for binary classification", {
  dt <- create_dataset(classification = TRUE)
  model <- train_model(dt[['X']], dt[['y']])
  imp <- permutationImportance(model, dt[['X']], dt[['y']])
  check_importance_scores(imp)
})

testthat::test_that("permutationImportance works for regression", {
  dt <- create_dataset(n = 200, p = 5, classification = FALSE)
  model <- train_model(dt[['X']], dt[['y']])
  imp <- permutationImportance(model, dt[['X']], dt[['y']])
  check_importance_scores(imp)
})

testthat::test_that("permutationImportance works for multiclass classification", {
  set.seed(42)
  n <- 300L
  x <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n)
  )
  coef_matrix <- matrix(c(
    1.0, -0.5, 0.2,  # coefficients for class A
    -0.5, 1.0, 0.2,  # coefficients for class B
    0.2, 0.2, 1.0    # coefficients for class C
  ), nrow = 3L, byrow = TRUE)
  
  linear_combinations <- as.matrix(x) %*% t(coef_matrix)
  linear_combinations <- linear_combinations + matrix(stats::rnorm(n * 3.0, sd = 0.1), nrow = n)
  probabilities <- exp(linear_combinations) / rowSums(exp(linear_combinations))
  y <- factor(apply(probabilities, 1, function(prob) sample(c("A", "B", "C"), 1, prob = prob)))
  
  model <- train_model(x, y)
  imp <- permutationImportance(model, x, y)
  check_importance_scores(imp, c('x1', 'x2', 'x3'))
})

testthat::test_that("permutationImportance handles edge cases", {
  # Test with a single feature
  x = data.table::data.table(x1 = stats::rnorm(100))
  y = factor(sample(c("A", "B"), 100, replace = TRUE))
  model <- train_model(x, y)
  imp <- permutationImportance(model, x, y)  # TODO: FIX ME
  check_importance_scores(imp, 'x1')
  testthat::expect_equal(imp_single, 1, tol=0.001)
  
  # Test with constant features
  n = 100L
  x <- data.table::data.table(
    x1 = rep(1L, n),
    x2 = stats::rnorm(n)
  )
  y = factor(sample(c("A", "B"), 100, replace = TRUE))
  model <- train_model(x, y)
  imp <- permutationImportance(model, x, y)
  check_importance_scores(imp, c('x1', 'x2'))
  testthat::expect_lt(imp["x1"], imp["x2"])
  
  # Test with perfect predictor
  n = 100L
  x <- data.table::data.table(
    x1 = rep(c(0L, 1L), each = n / 2L),
    x2 = stats::rnorm(n)
  )
  y = factor(rep(c("A", "B"), each = n / 2L))
  model <- train_model(x, y)
  imp <- permutationImportance(model, x, y)
  check_importance_scores(imp, c('x1', 'x2'))
  testthat::expect_gt(imp["x1"], imp["x2"])  # TODO: FIX ME
})
