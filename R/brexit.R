# Internal data used by both functions
.brexit_data <- function() {
  data.frame(
    leave          = c(67.60, 53.81, 41.40, 63.21, 58.76, 50.98, 53.20),
    high_education = c(19.17, 26.77, 34.81, 23.38, 24.24, 34.47, 30.03)
  )
}

.brexit_lines <- function(dist_df) {
  ols    <- stats::lm(leave ~ high_education, data = dist_df)
  b0_ols <- stats::coef(ols)[["(Intercept)"]]
  b1_ols <- stats::coef(ols)[["high_education"]]
  list(
    b0 = c(80.00,  b0_ols, 110.00),
    b1 = c(-0.90,  b1_ols, -2.00)
  )
}

#' Render the three-line comparison table for practice exam 2
#'
#' Displays intercept, slope, error sum of squares, and R-squared for
#' three candidate regression lines fit to the Brexit Leave/education data.
#'
#' @return A \code{knitr_kable} object (rendered automatically in R Markdown).
#' @export
brexit_lines_table <- function() {
  dist_df <- .brexit_data()
  lines   <- .brexit_lines(dist_df)

  sst <- sum((dist_df$leave - mean(dist_df$leave))^2)
  sse <- function(a, b)
    round(sum((dist_df$leave - (a + b * dist_df$high_education))^2), 2)

  tbl <- data.frame(
    Line      = c("Line 1", "Line 2", "Line 3"),
    Intercept = round(lines$b0, 2),
    Slope     = round(lines$b1, 2),
    `Error Sum of Squares` = mapply(sse, lines$b0, lines$b1),
    `R-squared` = round(1 - mapply(sse, lines$b0, lines$b1) / sst, 2),
    check.names = FALSE
  )

  knitr::kable(tbl, caption = "Comparison of three regression lines")
}

#' Render the three-line scatterplot for practice exam 2
#'
#' Plots Leave vote share against higher-education share for seven
#' constituencies, overlaid with three candidate regression lines.
#'
#' @return A \code{ggplot} object.
#' @export
brexit_lines_plot <- function() {
  dist_df <- .brexit_data()
  lines   <- .brexit_lines(dist_df)

  x_seq   <- seq(18, 36, length.out = 200)
  line_df <- data.frame(
    high_education = rep(x_seq, 3),
    leave = c(lines$b0[1] + lines$b1[1] * x_seq,
              lines$b0[2] + lines$b1[2] * x_seq,
              lines$b0[3] + lines$b1[3] * x_seq),
    line  = rep(c("Line 1", "Line 2", "Line 3"), each = 200)
  )

  ggplot2::ggplot(dist_df, ggplot2::aes(x = high_education, y = leave)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line(
      data     = line_df,
      ggplot2::aes(x = high_education, y = leave, color = line, linetype = line),
      linewidth = 1
    ) +
    ggplot2::scale_color_manual(
      values = c("Line 1" = "tomato", "Line 2" = "steelblue", "Line 3" = "darkorange")
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Line 1" = "dashed", "Line 2" = "solid", "Line 3" = "dotted")
    ) +
    ggplot2::labs(
      x = "% with Higher Education", y = "% Leave Vote",
      color = NULL, linetype = NULL
    ) +
    ggplot2::theme_minimal()
}
