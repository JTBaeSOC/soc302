#' Check a practice exam answer
#'
#' Compares the student's calculator answer against the correct value
#' recomputed from whatever data is currently defined in the student's
#' environment. Both sides are rounded to 2 decimal places before comparison,
#' matching standard calculator precision.
#'
#' Supports practice exam 1 (q1–q8: ATE, TikTok, WVS cross-tab) and
#' practice exam 2 (q1–q8: regression, ideology interpretation, Brexit OLS).
#' The correct exam is detected automatically from the variables present in
#' the student's environment.
#'
#' @param answer A numeric variable named q1 through q9 holding the student's
#'   answer. Assign your answer first, then pass the variable:
#'   \code{q1 <- 3.50; check(q1)}
#'
#' @return Prints \code{Correct!} or \code{Not quite} to the console.
#'   Returns \code{NULL} invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' treat1   <- c(10, 13, 5, 6, 8, 16)
#' control1 <- c(3, 12, 18, 5, 6, 8, 5)
#' q1 <- 1.52
#' check(q1)
#' }
check <- function(answer) {
  var_name <- deparse(substitute(answer))
  env      <- parent.frame()

  get_var <- function(name) {
    if (exists(name, envir = env, inherits = TRUE))
      get(name, envir = env, inherits = TRUE)
    else
      stop("Could not find '", name, "'. Make sure the data chunk has been run.",
           call. = FALSE)
  }

  has_var <- function(name) exists(name, envir = env, inherits = TRUE)

  valid_qs <- paste0("q", 1:9)
  if (!var_name %in% valid_qs) {
    message("Variable '", var_name, "' not recognised. ",
            "Make sure you name your answer q1 through q9.")
    return(invisible(NULL))
  }

  if (is.na(answer)) {
    cat("\u270f\ufe0f Replace NA with your answer, then re-run check().\n")
    return(invisible(NULL))
  }

  # Context flags — mutually exclusive variable names identify the active exam
  exam1_q1  <- has_var("treat1")
  exam1_q3  <- has_var("tiktok")
  exam1_q6  <- has_var("wvs_tbl")

  correct <- switch(var_name,

    # ── Q1 ──────────────────────────────────────────────────────────────────
    q1 = if (exam1_q1) {
      mean(get_var("treat1")) - mean(get_var("control1"))          # Exam 1: ATE
    } else {
      cov(get_var("x"), get_var("y"))                              # Exam 2: covariance
    },

    # ── Q2 ──────────────────────────────────────────────────────────────────
    q2 = if (exam1_q1) {
      mean(get_var("treat2")) - mean(get_var("control2"))          # Exam 1: ATE binary
    } else {
      x <- get_var("x"); y <- get_var("y")
      cov(x, y) / var(x)                                          # Exam 2: slope
    },

    # ── Q3 ──────────────────────────────────────────────────────────────────
    q3 = if (exam1_q3) {
      mean(get_var("tiktok"))                                      # Exam 1: mean
    } else {
      x <- get_var("x"); y <- get_var("y")
      mean(y) - (cov(x, y) / var(x)) * mean(x)                   # Exam 2: intercept
    },

    # ── Q4 ──────────────────────────────────────────────────────────────────
    q4 = if (exam1_q3) {
      sd(get_var("tiktok"))                                        # Exam 1: SD
    } else {
      x <- get_var("x"); y <- get_var("y")
      cov(x, y)^2 / (var(x) * var(y))                            # Exam 2: R-squared
    },

    # ── Q5 ──────────────────────────────────────────────────────────────────
    q5 = if (exam1_q3) {
      tiktok <- get_var("tiktok")
      (tiktok[1] - mean(tiktok)) / sd(tiktok)                    # Exam 1: z-score
    } else {
      -0.60                                                        # Exam 2: ideology slope
    },

    # ── Q6 ──────────────────────────────────────────────────────────────────
    q6 = if (exam1_q6) {
      tbl <- get_var("wvs_tbl")
      tbl["Democrat", "Agree"] / sum(tbl["Democrat", ]) * 100    # Exam 1: row %
    } else {
      4.50                                                         # Exam 2: predicted at x=0
    },

    # ── Q7 ──────────────────────────────────────────────────────────────────
    q7 = if (exam1_q6) {
      tbl <- get_var("wvs_tbl")
      tbl["Republican", "Disagree"] / sum(tbl[, "Disagree"]) * 100  # Exam 1: col %
    } else {
      3.60                                                         # Exam 2: liberal minus conservative
    },

    # ── Q8 ──────────────────────────────────────────────────────────────────
    q8 = if (exam1_q6) {
      tbl <- get_var("wvs_tbl")
      sum(tbl[, "Hard to say"]) / sum(tbl) * 100                 # Exam 1: marginal %
    } else {
      2                                                            # Exam 2: OLS line number
    },

    # ── Q9 ──────────────────────────────────────────────────────────────────
    q9 = {
      message("Question 9 is a free-response question \u2014 no automatic check available.")
      return(invisible(NULL))
    }
  )

  if (round(as.numeric(answer), 2) == round(correct, 2)) {
    cat("\u2705 Correct! Well done.\n")
  } else {
    cat("\u274c Not quite \u2014 double-check your calculation and try again.\n")
  }

  invisible(NULL)
}
