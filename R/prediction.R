#' Scatterplot with OLS line for the prediction error questions
#'
#' Plots four randomly selected data points with the OLS regression line.
#'
#' @return A \code{ggplot} object.
#' @export
prediction_plot <- function() {
  pts <- data.frame(
    x = c(0, -1, 2, 3),
    y = c(2,  4, -1, 3)
  )
  m  <- stats::lm(y ~ x, data = pts)
  b0 <- stats::coef(m)[["(Intercept)"]]
  b1 <- stats::coef(m)[["x"]]

  ggplot2::ggplot(pts, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_abline(
      intercept = b0, slope = b1,
      color = "steelblue", linewidth = 1
    ) +
    ggplot2::labs(
      x = "X", y = "Y",
      caption = paste0(
        "OLS line: \u0176 = ", round(b0, 2),
        " + (", round(b1, 2), ")X"
      )
    ) +
    ggplot2::theme_minimal()
}
