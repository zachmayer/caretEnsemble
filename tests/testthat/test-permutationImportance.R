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
  check_importance_scores(imp, c("x1", "x2", "x3"))
})

testthat::test_that("permutationImportance works with a single feature unimportant feature", {
  n <- 100L
  x <- data.table::data.table(x1 = stats::rnorm(n))
  y <- factor(sample(c("A", "B"), n, replace = TRUE))
  model <- train_model(x, y)
  imp <- permutationImportance(model, x, y)
  check_importance_scores(imp, "x1")
})


testthat::test_that("permutationImportance works with a single feature important feature", {
  set.seed(1234L)
  devtools::load_all()
  make_var <- function(n) scale(stats::rnorm(n), center = TRUE, scale = TRUE)[, 1L]

  n <- 1000L
  x <- data.table::data.table(
    x1 = make_var(n),
    x2 = make_var(n),
    x3 = make_var(n)
  )

  cf_set <- c(0L, 1L, 5L, 10L)
  all_cfs <- expand.grid(
    c(0L, 1L, 3L, 5L),
    cf_set,
    cf_set,
    cf_set
  )

  for (do_class in c(FALSE, TRUE)) {
    for (i in seq_len(nrow(all_cfs))) {
      # Create a perfectly linear relationship
      cf <- unname(unlist(all_cfs[i, ]))
      y <- (cbind(1L, as.matrix(x)) %*% cf)[, 1L]
      classes <- c("A", "B")
      if (do_class) {
        y <- factor(ifelse(y > 0L, classes[1L], classes[2L]), levels = classes)
      }

      # Fit the model and compute importance
      model <- suppressWarnings(train_model(x, y, method = "glm"))
      imp <- permutationImportance(model, x, y)
      check_importance_scores(imp, c("x1", "x2", "x3"))

      # Altrernative importance based on coefficients
      # Note that the input data all has a mean of 0 and an sd of 1
      glm_imp <- normalize_to_one(abs(coef(model$finalModel)))
      testthat::expect_equivalent(glm_imp, cf_norm, tolerance = 0.1)

      # Compare the importances to the real coefficients
      # For regression, the importances should be close to the coefficients
      # For classification without an intercept, the same
      # For classificaiton with an intercept its a lot more complicated
      if (!do_class || cf[[1L]] == 0.0) {
        cf_norm <- normalize_to_one(cf[-1L])
      }
      testthat::expect_equivalent(imp, cf_norm, tolerance = 0.1)
    }
  }
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
  testthat::expect_lte(imp["x1"], imp["x2"])
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
  testthat::expect_lte(imp["x1"], imp["x2"])
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
