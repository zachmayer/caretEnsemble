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
  weight <- NULL # due to NSE notes in R CMD check
  # Extract variable importance using caret::varImp
  imp <- caret::varImp(stack_model, newdata = newdata, normalize = TRUE)

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
#' Classifies each variable as either "Original" or "New" and optionally appends
#' a summary statistic (mean, sum, or max) of the opposite group. The added statistic
#' is flagged with `is_stat = TRUE` and can be used for plotting a reference bar.
#'
#' @param imp_dt data.table from `prepare_importance`.
#' @param original_features Character vector of original features.
#' @param stat_type Character string: "mean", "sum", "max", or NULL.
#' @return List with `imp_original` and `imp_new` data.tables.
add_cross_group_stats <- function(imp_dt, original_features, stat_type = NULL) {
  # Classify variables
  type <- method <- is_stat <- NULL # due to NSE notes in R CMD check
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
#' @param fill_colors Named vector specifying colors for each fill group.
#' @param y_max Numeric maximum for the y-axis.
#' @return ggplot object.
plot_group <- function(imp_dt, title, fill_colors, y_max) {
  method <- weight <- fill_group <- NULL # due to NSE notes in R CMD check
  ggplot2::ggplot(imp_dt, ggplot2::aes(x = stats::reorder(method, weight), y = weight, fill = fill_group)) +
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
#' for visual reference. It returns a \code{ggplot} object; if both original and new features
#' are present, the plot will contain two facets (original and new features) within the same figure.
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
#' @return A \code{ggplot} object. If the model includes both original and new features,
#' the plot will contain two facets ("Original Features" and "New Features").
#' If \code{stat_type} is provided, a gray bar appears in each plot representing
#' the selected summary statistic from the opposite group (e.g., mean of new features
#' shown in the original features plot).
#'
#' @details
#' - Variable importance is computed using \code{caret::varImp}.
#' - If the model lacks the \code{original_features} attribute, all variables are considered new.
#' - Requires the packages: \code{data.table}, \code{ggplot2}, and \code{caret}.
#'
#' @importFrom data.table :=
#' @export
plot_variable_importance <- function(stack_model, newdata, stat_type = NULL) {
  fill_group <- is_stat <- group <- NULL # due to NSE notes in R CMD check
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
    imp_dt[, fill_group := "New Features"]
    plot_group(
      imp_dt, "Variable Importance - New Features",
      fill_colors = c("New Features" = "red"),
      max(imp_dt$weight, na.rm = TRUE)
    )
  } else {
    # Case 2: Separate original and new, optionally add cross-group statistics
    imp_list <- add_cross_group_stats(imp_dt, original_features, stat_type)
    max_weight <- max(c(imp_list$imp_original$weight, imp_list$imp_new$weight), na.rm = TRUE)

    # Add group labels
    imp_list$imp_original[, group := "Original Features"]
    imp_list$imp_new[, group := "New Features"]

    # Combine datasets
    combined_dt <- data.table::rbindlist(list(imp_list$imp_original, imp_list$imp_new))

    # Fill colors
    combined_dt[, fill_group := ifelse(is_stat, "Stat", group)]
    fill_palette <- c("Original Features" = "blue", "New Features" = "red", Stat = "gray")

    # Plot with facets
    plot_group(
      combined_dt,
      "Variable Importance",
      fill_colors = fill_palette,
      max_weight
    ) +
      ggplot2::facet_grid(. ~ group)
  }
}
