# krulRutils: A collection of utility functions for
# data manipulation and visualization

# Load required packages
library(tidyverse)
library(devtools)
library(GGally)
library(broom)

#' Convert codes in a tibble column to a labeled factor using a lookup table
#'
#' @param data_tbl A tibble containing the data with codes to convert.
#' @param code_col Unquoted name of the column in `data_tbl` with the codes.
#' @param lookup_tbl A tibble with code-label pairs for lookup.
#' @param lookup_code_col Unquoted name of the code column in `lookup_tbl`.
#' @param lookup_label_col Unquoted name of the label column in `lookup_tbl`.
#' @param factor_levels Optional character vector specifying the levels order
#' for the output factor. Defaults to all labels in `lookup_tbl`, in order.
#' @param new_factor_col_name Optional name for the new factor column.
#'
#' @return A tibble with a new factor vector with labels corresponding to codes,
#' same length/order as `data_tbl`.
#' Codes not found in the lookup table result in NA and trigger a warning.
#' @export
convert_codes_to_factor <- function(
    data_tbl,
    code_col, # Column in data_tbl to match on
    lookup_tbl, # The lookup table
    lookup_code_col, # Column in lookup_tbl that holds the codes
    lookup_label_col, # Column in lookup_tbl that holds the labels
    factor_levels = NULL, # Optional: explicit order of factor levels
    new_factor_col_name = NULL) {
  # Capture unquoted column names for tidy evaluation
  code_col_sym <- rlang::ensym(code_col)
  lookup_code_col_sym <- rlang::ensym(lookup_code_col)
  lookup_label_col_sym <- rlang::ensym(lookup_label_col)

  new_factor_col_name_input <- rlang::enquo(new_factor_col_name)
  if (rlang::quo_is_null(new_factor_col_name_input)) {
    new_factor_col_name_sym <- rlang::sym(
      paste0(rlang::as_label(code_col_sym), "_factor")
    )
  } else {
    new_factor_col_name_sym <- rlang::sym(
      rlang::as_label(new_factor_col_name_input)
    )
  }
  # Store original column names to ensure they are kept
  original_col_names <- names(data_tbl)

  # Perform the join to get the labels
  join_by_args <- stats::setNames(
    rlang::as_label(lookup_code_col_sym),
    rlang::as_label(code_col_sym)
  )

  joined_tbl <- data_tbl %>%
    left_join(
      lookup_tbl,
      by = join_by_args
    )

  # Check for missing codes and issue a warning
  missing_codes <- joined_tbl %>%
    filter(is.na(!!lookup_label_col_sym)) %>%
    distinct(!!code_col_sym) %>%
    pull(!!code_col_sym)

  if (length(missing_codes) > 0) {
    warning(
      "The following codes were not found in the lookup table ",
      "and will be set to NA in the factor: ",
      paste(unique(missing_codes), collapse = ", ")
    )
  }

  # Determine factor levels if not provided
  if (is.null(factor_levels)) {
    factor_levels <- lookup_tbl %>%
      pull(!!lookup_label_col_sym) %>%
      unique()
  }

  # Add the labeled factor column
  final_tbl <- joined_tbl %>%
    mutate(
      !!new_factor_col_name_sym := factor(
        !!lookup_label_col_sym,
        levels = factor_levels
      )
    )

  # Now, select the desired columns: original columns + the new factor column.
  # We need to explicitly pick the *original* columns that were in data_tbl,
  # and then add the new factor column.
  # The label column from the lookup_tbl that was joined is temporary.
  final_tbl <- final_tbl %>%
    select(
      all_of(original_col_names), # Selects all columns from the original data
      !!new_factor_col_name_sym # Add the newly created factor column
    )

  return(final_tbl)
}





#' Summarise numeric variables in a tidy format
#'
#' Computes min, quartiles, median, mean, and max for all numeric columns,
#' returning a tibble with statistics as rows and variables as columns.
#'
#' @param df A data frame or tibble.
#' @param na.rm Logical, whether to remove NA values. Default TRUE.
#'
#' @return A tibble with rows = statistics and columns = variables.
#' @export
summarise_numeric_tidy <- function(df, na.rm = TRUE) {
  # Check input
  if (!is.data.frame(df)) {
    stop("Input must be a data.frame or tibble.")
  }

  num_cols <- df %>%
    select(where(is.numeric)) %>%
    names()

  if (length(num_cols) == 0) {
    stop("No numeric columns found in input.")
  }

  # Define summary functions with safe wrappers

  # Function for finding the minimal value
  safe_min <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      min(x, na.rm = na.rm)
    }
  }

  # Function for finding the first quartile
  safe_q1 <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      quantile(x, 0.25, na.rm = na.rm)
    }
  }

  # Function for finding the median value
  safe_median <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      median(x, na.rm = na.rm)
    }
  }

  # Function for finding the third quartile
  safe_q3 <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      quantile(x, 0.75, na.rm = na.rm)
    }
  }

  # Function for finding the maximal value
  safe_max <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      max(x, na.rm = na.rm)
    }
  }

  # Function for finding the mean value
  safe_mean <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      mean(x, na.rm = na.rm)
    }
  }

  # Function for finding the mean value
  safe_var <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      var(x, na.rm = na.rm)
    }
  }


  # Compute summaries wide, then reshape long and wide as requested
  df %>%
    summarise(
      across(
        all_of(num_cols),
        list(
          min = safe_min,
          q1 = safe_q1,
          median = safe_median,
          q3 = safe_q3,
          max = safe_max,
          mean = safe_mean,
          var = safe_var
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = c("variable", "statistic"),
      names_sep = "_",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    mutate(
      statistic = factor(
        statistic,
        levels = c("min", "q1", "median", "q3", "max", "mean", "var")
      )
    ) %>%
    arrange(statistic)
}


#' Modifies the color of the grid lines in ggplot2 plots
#' and sets the title size and face.
#'
#' @return A ggplot2 theme with modified grid line colors.
#' @export
theme_krul <- function() {
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray80")
  )
}

#' Applies the 'krul' theme to a GGally matrix
#'
#' @param ggpairs_obj A GGally ggpairs object.
#' @return The modified ggpairs object with the 'krul' theme applied.
#' @export
theme_krul_ggpairs <- function(ggpairs_obj) {
  n <- length(ggpairs_obj$xAxisLabels)

  for (i in 1:n) {
    for (j in 1:i) {
      ggpairs_obj[i, j] <- ggpairs_obj[i, j] +
        theme_krul()
    }
  }

  ggpairs_obj
}





#' Creates a boxplot legend explaining the components of a boxplot
#'
#' @param family Font family for the text in the legend.
#' @return A ggplot object with the boxplot legend.
#' @export
ggplot_box_legend <- function(family = "serif") {
  # Create data to use in the boxplot legend:
  set.seed(100)

  sample_df <- data.frame(
    parameter = "test",
    values = sample(500)
  )

  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350

  # Function to calculate important values:
  ggplot2_boxplot <- function(x) {
    quartiles <- as.numeric(quantile(x,
      probs = c(0.25, 0.5, 0.75)
    ))

    names(quartiles) <- c(
      "25th percentile",
      "50th percentile\n(median)",
      "75th percentile"
    )

    iqr <- diff(quartiles[c(1, 3)])

    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * iqr)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * iqr)])

    upper_dots <- x[x > (quartiles[3] + 1.5 * iqr)]
    lower_dots <- x[x < (quartiles[1] - 1.5 * iqr)]

    list(
      "quartiles" = quartiles,
      "25th percentile" = as.numeric(quartiles[1]),
      "50th percentile\n(median)" = as.numeric(quartiles[2]),
      "75th percentile" = as.numeric(quartiles[3]),
      "IQR" = iqr,
      "upper_whisker" = upper_whisker,
      "lower_whisker" = lower_whisker,
      "upper_dots" = upper_dots,
      "lower_dots" = lower_dots
    )
  }

  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)

  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults(
    "text",
    list(
      size = 3,
      hjust = 0,
      family = family
    )
  )
  # Labels don't inherit text:
  update_geom_defaults(
    "label",
    list(
      size = 3,
      hjust = 0,
      family = family
    )
  )

  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(
      data = sample_df,
      aes(x = parameter, y = values),
      geom = "errorbar", width = 0.3
    ) +
    geom_boxplot(
      data = sample_df,
      aes(x = parameter, y = values),
      width = 0.3, fill = "lightgrey"
    ) +
    geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    geom_text(
      aes(
        x = 1.17, y = 950,
        label = "Number of values"
      ),
      fontface = "bold", vjust = 0.4
    ) +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(
      x = 2.3, xend = 2.3,
      y = ggplot_output[["25th percentile"]],
      yend = ggplot_output[["75th percentile"]]
    )) +
    geom_segment(aes(
      x = 1.2, xend = 2.3,
      y = ggplot_output[["25th percentile"]],
      yend = ggplot_output[["25th percentile"]]
    )) +
    geom_segment(aes(
      x = 1.2, xend = 2.3,
      y = ggplot_output[["75th percentile"]],
      yend = ggplot_output[["75th percentile"]]
    )) +
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),
      label = "Interquartile\nrange", fontface = "bold",
      vjust = 0.4
    ) +
    geom_text(
      aes(
        x = c(1.17, 1.17),
        y = c(
          ggplot_output[["upper_whisker"]],
          ggplot_output[["lower_whisker"]]
        ),
        label = c(
          paste0(
            "Largest value within 1.5 times\n",
            "interquartile range above\n",
            "75th percentile"
          ),
          paste0(
            "Smallest value within 1.5 times\n",
            "interquartile range below\n",
            "25th percentile"
          )
        ),
      ),
      fontface = "bold", vjust = 0.9
    ) +
    geom_text(
      aes(
        x = c(1.17),
        y = ggplot_output[["lower_dots"]],
        label = "Outside value"
      ),
      vjust = 0.5, fontface = "bold"
    ) +
    geom_text(
      aes(
        x = c(1.17),
        y = ggplot_output[["lower_dots"]],
        label = paste0(
          "-Value is >1.5 times and \n",
          "<3 times the interquartile range\n",
          "beyond either end of the box"
        )
      ),
      vjust = 1.3
    ) +
    geom_label(
      aes(
        x = 1.17, y = ggplot_output[["quartiles"]],
        label = names(ggplot_output[["quartiles"]])
      ),
      vjust = c(0.4, 0.85, 0.4),
      fill = "white", label.size = 0
    ) +
    ylab("") +
    xlab("") +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      aspect.ratio = 4 / 3,
      plot.title = element_text(hjust = 0.5, size = 10)
    ) +
    coord_cartesian(xlim = c(1.4, 3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")

  return(explain_plot)
}

#' Color palette from the 'Carnelian' color theme
#'
#' @export
c_palette <- c(
  `C red`          = "#B31B1B",
  `C orange`       = "#67411B",
  `C yellow`       = "#4F4F1B",
  `C chartreuse`   = "#3A591B",
  `C green`        = "#1B681B",
  `C spring green` = "#1B623E",
  `C cyan`         = "#1B5C5C",
  `C azure`        = "#1B548C",
  `C blue`         = "#3B3BBB",
  `C violet`       = "#6D21BB",
  `C magenta`      = "#8A1B8A",
  `C rose`         = "#9C1B5B",
  `C grey`         = "#494949"
)

#' Custom color palette for C colors
#'
#' Returns a named vector of hexadecimal color values.
#' @param ... Optional names of specific colors to return
#' @export
c_pal <- function(...) {
  cols <- c(...)
  if (length(cols) == 0) {
    return(c_palette)
  }
  c_palette[cols]
}

#' Integration with ggplot2 'scale_fill_manual()' function
#'
#' @param ... Optional colors to be chosen from the palette
#' @export
c_scale_fill <- function(...) {
  scale_fill_manual(values = unname(c_pal(...)))
}



#' Integration with ggplot2 'scale_color_manual()' function
#'
#' @param ... Optional colors to be chosen from the palette
#' @export
c_scale_color <- function(...) {
  scale_color_manual(values = unname(c_pal(...)))
}


#' Compute the Statistical Mode
#'
#' Returns the mode (most frequent value) of a numeric or character vector.
#' If there are multiple modes, the first one encountered is returned.
#'
#' @param x A vector of numeric or character values.
#'
#' @return A single value representing the mode of the input vector.
#'
#' @examples
#' kmode(c(1, 2, 2, 3, 3, 3, 4))
#' # Returns: 3
#'
#' kmode(c("apple", "banana", "apple", "orange"))
#' # Returns: "apple"
#'
#' @export
kmode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#' Compute groupwise means and confidence intervals using t-tests
#'
#' @param data A data frame or tibble.
#' @param group_col A bare (unquoted) grouping variable.
#' @param value_col A bare (unquoted) numeric variable.
#' @param conf_level Confidence level for the t-test (default = 0.95).
#'
#' @return A tibble with group means and confidence intervals.
#' @export
compute_ci <- function(
    data,
    ...,
    value_col,
    conf_level = 0.95,
    alternative = "two.sided") {
  data %>%
    group_by(...) %>%
    summarise(
      tidy(t.test(
        {{ value_col }},
        conf.level = conf_level,
        alternative = alternative
      )),
      .groups = "drop"
    ) %>%
    select(
      ...,
      ci_lower = conf.low,
      x_bar = estimate,
      ci_upper = conf.high
    )
}

#' Perform a t-test for group means
#'
#' @param data A data frame or tibble.
#' @param group_col A bare (unquoted) grouping variable.
#' @param value_col A bare (unquoted) numeric variable.
#' @param alpha Significance level for the t-test (default = 0.05).
#'
#' @return A tibble with t-test results
#' @export
t_test <- function(
    data,
    ...,
    value_col,
    mu_0 = 0,
    alpha = 0.05,
    alternative = "two.sided") {
  conf_level <- 1 - alpha

  result_lookup_tbl <- tibble(
    code = c("reject", "fail_reject"),
    label = c("Reject null hypothesis", "Fail to reject null hypothesis")
  )

  data %>%
    group_by(...) %>%
    summarise(
      tidy(t.test(
        {{ value_col }},
        mu = mu_0,
        conf.level = conf_level,
        alternative = alternative
      )),
      .groups = "drop"
    ) %>%
    mutate(
      alpha = alpha,
      test_result = if_else(
        p.value < alpha,
        "reject",
        "fail_reject"
      )
    ) %>%
    convert_codes_to_factor(
      code_col = test_result,
      lookup_tbl = result_lookup_tbl,
      lookup_code_col = code,
      lookup_label_col = label,
      new_factor_col_name = result
    ) %>%
    select(
      ...,
      p_value = p.value,
      alpha,
      result
    )
}

#' Density function for location-scale t-distribution
#'
#' @param x Vector of quantiles.
#' @param df Degrees of freedom.
#' @param mu Location parameter.
#' @param sigma Scale parameter (σ > 0).
#'
#' @return Vector of density values.
#' @export
dlst <- function(mu, df, x_bar = 0, s_n = 1) {
  dt((mu - x_bar) / s_n, df) / s_n
}

#' Cumulative distribution function for location-scale t-distribution
#'
#' @param q Vector of quantiles.
#' @param df Degrees of freedom.
#' @param mu Location parameter.
#' @param sigma Scale parameter (σ > 0).
#'
#' @return Vector of probabilities.
#' @export
plst <- function(q, df, x_bar = 0, s_n = 1) {
  pt((q - x_bar) / s_n, df)
}

#' Quantile function for location-scale t-distribution
#'
#' @param p Vector of probabilities.
#' @param df Degrees of freedom.
#' @param mu Location parameter.
#' @param sigma Scale parameter (σ > 0).
#'
#' @return Vector of quantiles.
#' @export
qlst <- function(p, df, x_bar = 0, s_n = 1) {
  x_bar + s_n * qt(p, df)
}

#' Random generation from location-scale t-distribution
#'
#' @param n Number of observations.
#' @param df Degrees of freedom.
#' @param mu Location parameter.
#' @param sigma Scale parameter (σ > 0).
#'
#' @return Vector of random deviates.
#' @export
rlst <- function(m, df, x_bar = 0, s_n = 1) {
  x_bar + s_n * rt(m, df)
}
