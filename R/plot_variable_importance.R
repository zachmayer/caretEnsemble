#' Prepare variable importance data.table from a caretStack
#'
#' @description
#' Extracts variable importance from a `caretStack` model and returns a
#' `data.table` with columns `method` and `weight` sorted by importance.
#'
#' @param stack_model A trained `caretStack` model.
#' @param newdata A data frame used for calculating variable importance.
#' @return `data.table` with columns `method` (variable names) and `weight` (importance).
prepare_importance <- function(stack_model, newdata) {
  # Extract variable importance using caret::varImp
  imp <- caret::varImp(stack_model, newdata = newdata)

  # Convert to data.table and rename columns
  imp_dt <- data.table::as.data.table(imp)
  data.table::set(imp_dt, j = "method", value = names(imp))
  data.table::setnames(imp_dt, c("weight", "method"))

  # Sort by decreasing importance
  imp_dt <- imp_dt[order(-weight)]

  imp_dt
}

#' Add cross-group statistics to the importance table
#'
#' @description
#' Classifies variables as "Original" or "New" and optionally adds a gray bar
#' representing a summary statistic (mean, sum, max) from the opposite group.
#'
#' @param imp_dt data.table from `prepare_importance`.
#' @param original_features Character vector of original features.
#' @param stat_type Character string: "mean", "sum", "max", or NULL.
#' @return List with `imp_original` and `imp_new` data.tables.
add_cross_group_stats <- function(imp_dt, original_features, stat_type = NULL) {
  # Classify variables
  imp_dt[, type := ifelse(method %in% original_features, "Original", "New")]
  imp_dt[, is_stat := FALSE]

  imp_original <- imp_dt[type == "Original"]
  imp_new <- imp_dt[type == "New"]

  # Add cross-group statistics if applicable
  if (!is.null(stat_type)) {
    stat_label <- switch(stat_type,
      mean = "Mean",
      sum = "Sum",
      max = "Maximum"
    )

    stat_value_for_original <- switch(stat_type,
      mean = mean(imp_new$weight, na.rm = TRUE),
      sum = sum(imp_new$weight, na.rm = TRUE),
      max = max(imp_new$weight, na.rm = TRUE)
    )

    stat_value_for_new <- switch(stat_type,
      mean = mean(imp_original$weight, na.rm = TRUE),
      sum = sum(imp_original$weight, na.rm = TRUE),
      max = max(imp_original$weight, na.rm = TRUE)
    )

    # Labels
    stat_label_original <- paste0(stat_label, " (New)")
    stat_label_new <- paste0(stat_label, " (Original)")

    # Add statistical rows
    if (nrow(imp_original) > 0L) {
      imp_original <- data.table::rbindlist(list(
        imp_original,
        data.table::data.table(
          method = stat_label_original,
          weight = stat_value_for_original,
          type = "Original",
          is_stat = TRUE
        )
      ), use.names = TRUE)
    }
    if (nrow(imp_new) > 0L) {
      imp_new <- data.table::rbindlist(list(
        imp_new,
        data.table::data.table(
          method = stat_label_new,
          weight = stat_value_for_new,
          type = "New",
          is_stat = TRUE
        )
      ), use.names = TRUE)
    }
  }

  list(imp_original = imp_original, imp_new = imp_new)
}

#' Plot a group of variable importances
#'
#' @description
#' Generates a ggplot bar chart for a given set of variable importances.
#'
#' @param imp_dt data.table with columns `method`, `weight`, `is_stat`.
#' @param title Title for the plot.
#' @param fill_colors Named vector: `FALSE` = normal bar color, `TRUE` = gray for statistic bar.
#' @param y_max Numeric maximum for the y-axis.
#' @return ggplot object.
plot_group <- function(imp_dt, title, fill_colors, y_max) {
  ggplot2::ggplot(imp_dt, ggplot2::aes(x = stats::reorder(method, weight), y = weight, fill = is_stat)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title, x = "Variable", y = "Importance") +
    ggplot2::scale_y_continuous(limits = c(0L, y_max))
}

#' @title Plot Variable Importance from a caretStack Model
#'
#' @description
#' This function plots the variable importance from a stacked ensemble model (`caretStack`),
#' separating original features from new (engineered) features. It optionally includes
#' cross-group summary statistics (mean, sum, or max) from one feature group into the other
#' for visual reference. It returns a \code{ggplot} object if all variables are treated as new,
#' or a \code{patchwork} object containing two plots (original and new features).
#' This is useful for diagnosing which group of features contributes more to the stacked model.
#'
#' @param stack_model A \code{caretStack} model trained using \code{caretEnsemble}.
#' It should have an attribute \code{original_features} listing the original input variables.
#' If this attribute is missing, all variables are treated as "new".
#'
#' @param newdata A data frame containing the data used for calculating variable importance.
#' Typically this should be the validation or test set.
#'
#' @param stat_type Optional character string indicating which summary statistic
#' of the opposite group to include as a gray bar for reference.
#' Must be one of \code{"mean"}, \code{"sum"}, or \code{"max"}. If \code{NULL}, no statistic is shown.
#' If invalid, an error is thrown.
#'
#' @return A \code{ggplot} object if no original features attribute is found,
#' or a \code{patchwork} object with two bar plots:
#' one for original features (in blue) and one for new features (in red).
#' If \code{stat_type} is provided, a gray bar appears in each plot representing
#' the selected summary statistic from the opposite group (e.g., mean of new features
#' shown in the original features plot).
#'
#' @details
#' - Variable importance is computed using \code{caret::varImp}.
#' - If the model lacks the \code{original_features} attribute, all variables are considered new.
#' - Requires the packages: \code{data.table}, \code{ggplot2}, \code{caret}, and \code{patchwork}.
#'
#' @importFrom data.table :=
#' @export
plot_variable_importance <- function(stack_model, newdata, stat_type = NULL) {
  # Prepare importance table
  imp_dt <- prepare_importance(stack_model, newdata)
  original_features <- stack_model[["original_features"]]

  # Validate stat_type
  valid_stats <- c("mean", "sum", "max")
  if (!is.null(stat_type) && !(stat_type %in% valid_stats)) {
    stop("stat_type must be 'mean', 'sum', or 'max'.", call. = FALSE)
  }

  # Case 1: No original features → all variables treated as "New"
  if (is.null(original_features)) {
    imp_dt[, type := "New"]
    imp_dt[, is_stat := FALSE]
    plot_group(
      imp_dt, "Variable Importance - New Features", c("FALSE" = "red", "TRUE" = "gray"),
      max(imp_dt$weight, na.rm = TRUE)
    )
  } else {
    # Case 2: Separate original and new, optionally add cross-group statistics
    imp_list <- add_cross_group_stats(imp_dt, original_features, stat_type)
    max_weight <- max(c(imp_list$imp_original$weight, imp_list$imp_new$weight), na.rm = TRUE)

    p1 <- plot_group(imp_list$imp_original, "Original Features", c("FALSE" = "blue", "TRUE" = "gray"), max_weight)
    p2 <- plot_group(imp_list$imp_new, "New Features", c("FALSE" = "red", "TRUE" = "gray"), max_weight)

    # Combine plots side by side
    p1 + p2 + patchwork::plot_layout(ncol = 2L)
  }
}
