#' Extension of \code{\link[dplyr]{mutate}} by a predicate function
#'
#' Use a predicate function \code{.p} to mutate only a subset of rows where the
#' predicate is satisfied. Creating new columns in combination with a predicate
#' creates by default \code{NA} values in rows where the predicate is not
#' satisfied.
#'
#' @param .df A \code{\link[tibble]{tibble}}.
#' @param .p A predicate function.
#' @param .ignore_na Should NA values in the evaluation of the predicate
#'   function be ignored? Defaults to \code{FALSE}.
#' @param ... Name-value pairs passed to \code{\link[dplyr]{mutate}}.
#'
#' @return A \code{\link[tibble]{tibble}}.
#' @export
#'
#' @examples
#' mutate_rows(
#'   tibble::tibble(
#'     x = c(-1, 0, -1),
#'     y = c(1, 1, 1)
#'   ),
#'   x < 0,
#'   y = y + 1,
#'   z = y + 1
#' )
mutate_rows <- function(.df, .p, .ignore_na = FALSE, ...) {
  .p_eval <- rlang::eval_tidy(rlang::enquo(.p), .df)

  if (any(is.na(.p_eval))) {
    .msg <- glue::glue(
      "Evaluating the predicate function resulted in NA values at index
       {which(is.na(.p_eval))}."
    )
    if (.ignore_na) {
      # work with index
      .p_eval <- which(.p_eval[!is.na(.p_eval)])
      rlang::warn(.msg)
    } else {
      rlang::abort(.msg)
    }
  }

  .dots <- rlang::enquos(..., .ignore_empty = "all")
  .vars <- rlang::names2(.dots)

  .vars_old <- purrr::keep(.vars, .vars %in% rlang::names2(.df))
  .vars_new <- purrr::discard(.vars, .vars %in% rlang::names2(.df))

  if (rlang::has_length(.vars_old)) {
    .df[.p_eval, ] <- dplyr::mutate(.df[.p_eval, ], !!!.dots[.vars_old])
  }

  if (rlang::has_length(.vars_new)) {
    .missing_expr <- purrr::map(
      .vars_new,
      ~ rlang::quo_get_expr(.dots[[.x]])
    ) |>
      rlang::set_names(.vars_new)

    .df_aug <- tibble::tibble(.rows = nrow(.df))

    for (i in .vars_new) {
      .df_aug[, i] <- rlang::eval_tidy(.missing_expr[[i]], .df)
      .df_aug[!.p_eval, ] <- NA
    }

    .df <- dplyr::bind_cols(.df, .df_aug)
  }

  return(.df)
}
