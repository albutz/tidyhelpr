#' Create a random \code{\link[tibble]{tibble}}
#'
#' @param .col_names Column names for the random \code{\link[tibble]{tibble}}.
#' @param .clean_col_names Should column names be cleaned? Defaults to
#'   \code{TRUE}.
#' @param .col_types A list of column types. If \code{NULL} (the default),
#'   then all columns will be of type \code{character}.
#' @param .n_rows Number of rows for the random \code{\link[tibble]{tibble}}.
#'
#' @return A randomly created \code{\link[tibble]{tibble}} with specified column
#'   types.
#' @export
#'
#' @examples
#' random_tibble(
#'   c("x_1", "X 2"),
#'   .n_rows = 10
#' )
random_tibble <- function(.col_names,
                          .clean_col_names = TRUE,
                          .col_types = NULL,
                          .n_rows = 100) {
  if (.clean_col_names) {
    .col_names <- janitor::make_clean_names(.col_names)
  }

  # No col_types specified
  if (rlang::is_null(.col_types)) {
    .df_random <- purrr::map_dfc(
      .col_names,
      function(.col) {
        tibble::tibble(
          stringi::stri_rand_lipsum(1) |>
            stringr::str_split(pattern = " ") |>
            purrr::flatten_chr() |>
            stringr::str_subset(pattern = "[^A-Za-z]", negate = TRUE) |>
            sample(size = .n_rows, replace = TRUE)
        ) |>
          rlang::set_names(.col)
      }
    )
  }

  return(.df_random)
}
