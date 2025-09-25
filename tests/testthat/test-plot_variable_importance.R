######################################################################
testthat::context("plot_variable_importance function")
######################################################################

# Load example data
utils::data(models.reg)
utils::data(X.reg)
utils::data(Y.reg)
utils::data(models.class)
utils::data(X.class)
utils::data(Y.class)
utils::data(iris)

# --------------------------------------------------------------------
# Unit tests for individual helper functions
# --------------------------------------------------------------------
testthat::test_that("prepare_importance returns sorted data.table", {
  stack_model <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class
  )

  imp_dt <- prepare_importance(stack_model, newdata = X.class)

  testthat::expect_s3_class(imp_dt, "data.table")
  testthat::expect_true(all(c("method", "weight") %in% names(imp_dt)))
  # Check that the table is sorted by decreasing weight
  testthat::expect_true(all(diff(imp_dt$weight) <= 0L))
})

testthat::test_that("add_cross_group_stats splits correctly and adds stats", {
  stack_model <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = colnames(X.class)
  )

  imp_dt <- prepare_importance(stack_model, newdata = X.class)

  # Case without stat_type
  imp_list <- add_cross_group_stats(imp_dt, colnames(X.class), stat_type = NULL)
  testthat::expect_true(all(c("imp_original", "imp_new") %in% names(imp_list)))
  testthat::expect_s3_class(imp_list$imp_original, "data.table")

  # Case with stat_type = "mean"
  imp_list_mean <- add_cross_group_stats(imp_dt, colnames(X.class), stat_type = "mean")
  testthat::expect_true(any(imp_list_mean$imp_original$is_stat))
  testthat::expect_true(any(imp_list_mean$imp_new$is_stat))
})

testthat::test_that("plot_group returns ggplot object with correct limits", {
  dt <- data.table::data.table(
    method = c("x1", "x2"),
    weight = c(0.2, 0.8),
    is_stat = c(FALSE, FALSE)
  )

  p <- plot_group(dt, "Test Plot", c("FALSE" = "blue", "TRUE" = "gray"), y_max = 1L)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_identical(p$labels$title, "Test Plot")
})

# --------------------------------------------------------------------
# Unit tests for plot_variable_importance
# --------------------------------------------------------------------
testthat::test_that("plot_variable_importance returns ggplot or patchwork objects", {
  stack_model <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = colnames(X.class)
  )

  # Case without stat_type
  p <- plot_variable_importance(stack_model, newdata = X.class)
  testthat::expect_s3_class(p, "patchwork")

  # Case with valid stat_type
  p_mean <- plot_variable_importance(stack_model, newdata = X.class, stat_type = "mean")
  testthat::expect_s3_class(p_mean, "patchwork")

  # Case with invalid stat_type: should throw an error
  expect_error(
    plot_variable_importance(stack_model, newdata = X.class, stat_type = "invalid"),
    "stat_type must be 'mean', 'sum', or 'max'"
  )

  # Case without original_features
  stack_model_no_orig <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = NULL
  )
  p_new <- plot_variable_importance(stack_model_no_orig, newdata = X.class)
  testthat::expect_s3_class(p_new, "ggplot")
})

testthat::test_that("plot_variable_importance plots correct titles and colors", {
  stack_model <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = colnames(X.class)
  )

  p <- plot_variable_importance(stack_model, newdata = X.class, stat_type = "max")

  # Extract individual plots if patchwork object
  if (inherits(p, "patchwork")) {
    plots <- list(p[[1L]], p[[2L]])
  } else {
    plots <- list(p)
  }

  # Check plot titles
  titles <- vapply(plots, function(g) g$labels$title, FUN.VALUE = character(1L))
  testthat::expect_true(any(grepl("Original Features", titles, fixed = TRUE)))
  testthat::expect_true(any(grepl("New Features", titles, fixed = TRUE)))

  # Check bar fill colors
  fills <- unique(unlist(lapply(plots, function(g) {
    ggplot2::ggplot_build(g)$data[[1L]]$fill
  })))
  testthat::expect_true(any(grepl("blue", fills, fixed = TRUE)))
  testthat::expect_true(any(grepl("red", fills, fixed = TRUE)))
})

# --------------------------------------------------------------------
# Unit tests for each base model (lm, glm, rpart, rf)
# --------------------------------------------------------------------
testthat::test_that("plot_variable_importance works for individual base models", {
  # Methods to test for classification
  stack_methods_class <- c("rf", "rpart")

  for (method in stack_methods_class) {
    stack_model <- caretStack(
      models.class,
      method = method,
      new_X = X.class,
      new_y = Y.class,
      original_features = colnames(X.class)
    )

    p <- plot_variable_importance(stack_model, newdata = X.class)
    testthat::expect_s3_class(p, "patchwork")
  }

  # Methods to test for regression
  stack_methods_reg <- c("glm", "lm")

  for (method in stack_methods_reg) {
    stack_model <- caretStack(
      models.reg,
      method = method,
      new_X = X.reg,
      new_y = Y.reg
    )

    p <- plot_variable_importance(stack_model, newdata = X.reg)
    testthat::expect_s3_class(p, "ggplot")
  }
})
