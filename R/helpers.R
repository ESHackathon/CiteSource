# Licensed under the MIT licence
# YEAR: 2020
# COPYRIGHT HOLDER: usethis authors
# see https://github.com/r-lib/usethis/blob/main/LICENSE

ui_yeah <- function (x, yes = c("Yes", "Definitely", "For sure", "Yup", 
                     "Yeah", "I agree", "Absolutely"), no = c("No way", "Not now", 
                                                              "Negative", "No", "Nope", "Absolutely not"), n_yes = 1, n_no = 2, 
          shuffle = TRUE, .envir = parent.frame()) 
{
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = .envir)
  if (!interactive()) {
    stop(c("User input required, but session is not interactive.", 
              glue::glue("Query: {x}")))
  }
  n_yes <- min(n_yes, length(yes))
  n_no <- min(n_no, length(no))
  qs <- c(sample(yes, n_yes), sample(no, n_no))
  if (shuffle) {
    qs <- sample(qs)
  }
  rlang::inform(x)
  out <- utils::menu(qs)
  out != 0L && qs[[out]] %in% yes
}