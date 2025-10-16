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

testthat::test_that("plot_group returns ggplot object with correct title", {
  dt <- data.table::data.table(
    method = c("x1", "x2"),
    weight = c(0.2, 0.8),
    is_stat = c(FALSE, FALSE)
  )

  # Add fill_group
  dt[, fill_group := "New Features"]

  fill_palette <- c("New Features" = "red", Stat = "gray")

  p <- plot_group(dt, "Test Plot", fill_colors = fill_palette, y_max = 1L)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_identical(p$labels$title, "Test Plot")
})


# --------------------------------------------------------------------
# Unit tests for plot_variable_importance
# --------------------------------------------------------------------
testthat::test_that("plot_variable_importance returns ggplot objects", {
  stack_model <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = colnames(X.class)
  )

  # Case without stat_type
  p <- plot_variable_importance(stack_model, newdata = X.class)
  testthat::expect_s3_class(p, "ggplot")
  facet_vars <- ggplot2::ggplot_build(p)$layout$panel_params
  testthat::expect_length(facet_vars, 2L)

  # Case with valid stat_type
  p_mean <- plot_variable_importance(stack_model, newdata = X.class, stat_type = "mean")
  testthat::expect_s3_class(p_mean, "ggplot")
  facet_vars <- ggplot2::ggplot_build(p_mean)$layout$panel_params
  testthat::expect_length(facet_vars, 2L)

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
  facet_vars <- ggplot2::ggplot_build(p_new)$layout$panel_params
  testthat::expect_length(facet_vars, 1L)
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

  # Check general plot title
  testthat::expect_identical(p$labels$title, "Variable Importance")

  # Check that facet variable exists and includes both types
  facet_var <- unique(p$data$type)
  testthat::expect_true(all(c("Original", "New") %in% facet_var))

  # Check bar fill colors
  fills <- unique(ggplot2::ggplot_build(p)$data[[1L]]$fill)
  testthat::expect_true(any(grepl("blue", fills, fixed = TRUE)))
  testthat::expect_true(any(grepl("red", fills, fixed = TRUE)))
  testthat::expect_true(any(grepl("gray", fills, fixed = TRUE)))
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
    testthat::expect_s3_class(p, "ggplot")
    facet_vars <- ggplot2::ggplot_build(p)$layout$panel_params
    testthat::expect_length(facet_vars, 2L)
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
    facet_vars <- ggplot2::ggplot_build(p)$layout$panel_params
    testthat::expect_length(facet_vars, 1L)
  }
})
