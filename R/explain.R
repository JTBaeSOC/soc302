# Question metadata used to build prompts
.mc_meta <- list(
  q11 = list(
    correct = 1,
    prompt  = function(ans) paste0(
      "A student in an undergraduate social statistics course answered a ",
      "multiple-choice question incorrectly. ",
      "The question: 'The regression equation is Y-hat = 4.5 - 0.6X. ",
      "When ideology X = 0 (moderate), the predicted value is 4.5. ",
      "Which parameter does this equal? 1=Intercept, 2=Slope, 3=R-squared.' ",
      "Correct answer: 1 (Intercept). Student answered: ", ans, ". ",
      "In 2-3 plain sentences, explain why their choice is wrong and why ",
      "the correct answer is the intercept. No markdown formatting."
    )
  ),
  q13 = list(
    correct = 2,
    prompt  = function(ans) paste0(
      "A student in an undergraduate social statistics course answered a ",
      "multiple-choice question incorrectly. ",
      "The question: 'A table shows three regression lines fit to the same ",
      "data, with their Error Sum of Squares (SSE) and R-squared. ",
      "Line 1 SSE=103.29, Line 2 SSE=56.64, Line 3 SSE=141.20. ",
      "Which line is the OLS estimate? Enter 1, 2, or 3.' ",
      "Correct answer: 2. Student answered: ", ans, ". ",
      "In 2-3 plain sentences, explain why their choice is wrong and how ",
      "to identify the OLS line from the table. No markdown formatting."
    )
  ),
  q14 = list(
    correct = 3,
    prompt  = function(ans) paste0(
      "A student in an undergraduate social statistics course answered a ",
      "multiple-choice question incorrectly. ",
      "The question: 'Which statistic identifies the OLS line? ",
      "1=Intercept, 2=Slope, 3=Error Sum of Squares, 4=R-squared.' ",
      "Correct answer: 3 (Error Sum of Squares), because OLS is defined as ",
      "the estimator that minimises the sum of squared residuals. ",
      "Student answered: ", ans, ". ",
      "In 2-3 plain sentences, explain why their choice is wrong and why ",
      "SSE is the right criterion. No markdown formatting."
    )
  ),
  q15 = list(
    correct = 2,
    prompt  = function(ans) paste0(
      "A student in an undergraduate social statistics course answered a ",
      "multiple-choice question incorrectly. ",
      "The question: 'In two regression models, Model 1 regresses earnings ",
      "on work experience only. Model 2 adds skill rating. Work experience ",
      "causes skill development, which in turn affects earnings — so skill ",
      "rating lies on the causal path from experience to earnings. ",
      "What is the role of skill rating? ",
      "1=Multiple cause, 2=Mediator, 3=Interaction, 4=Confounder.' ",
      "Correct answer: 2 (Mediator). Student answered: ", ans, ". ",
      "In 2-3 plain sentences, explain why their choice is wrong and why ",
      "skill rating is a mediator. No markdown formatting."
    )
  )
)

.call_gemini <- function(prompt, key) {
  url  <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    "gemini-2.0-flash-lite:generateContent?key=", key
  )
  body <- list(
    contents = list(
      list(parts = list(list(text = prompt)))
    )
  )
  resp <- httr2::request(url) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200) {
    stop(
      "Gemini API error (", httr2::resp_status(resp), "): ",
      httr2::resp_body_string(resp),
      call. = FALSE
    )
  }
  result <- httr2::resp_body_json(resp)
  result$candidates[[1]]$content$parts[[1]]$text
}

#' Ask for an explanation of a wrong multiple-choice answer
#'
#' Sends the question context and the student's answer to the Gemini API
#' and prints a short explanation of why the answer is incorrect.
#' Available for multiple-choice questions q11, q13, q14, and q15.
#'
#' Requires a free Gemini API key set in the environment:
#' \code{Sys.setenv(GEMINI_API_KEY = "your-key")}
#' Get a free key at \url{https://aistudio.google.com/apikey}.
#'
#' @param answer The answer variable (e.g. \code{q11}).
#'
#' @return Prints the explanation to the console.
#'   Returns \code{NULL} invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' q15 <- 4
#' explain(q15)
#' }
explain <- function(answer) {
  var_name <- deparse(substitute(answer))

  if (!var_name %in% names(.mc_meta)) {
    message(
      "explain() is available for MC questions only: ",
      paste(names(.mc_meta), collapse = ", ")
    )
    return(invisible(NULL))
  }

  meta <- .mc_meta[[var_name]]

  if (is.na(answer)) {
    cat("\u270f\ufe0f Enter your answer first, then run explain().\n")
    return(invisible(NULL))
  }

  if (round(as.numeric(answer), 0) == meta$correct) {
    cat("\u2705 Your answer is correct \u2014 no explanation needed!\n")
    return(invisible(NULL))
  }

  key <- Sys.getenv("GEMINI_API_KEY")
  if (!nzchar(key)) {
    message(
      "No API key found. Set it with:\n",
      "  Sys.setenv(GEMINI_API_KEY = \"your-key\")\n",
      "Get a free key at: https://aistudio.google.com/apikey"
    )
    return(invisible(NULL))
  }

  cat("Thinking...\n")
  tryCatch(
    {
      text <- .call_gemini(meta$prompt(answer), key)
      cat("\n", text, "\n", sep = "")
    },
    error = function(e) message("Could not reach Gemini API: ", e$message)
  )

  invisible(NULL)
}
