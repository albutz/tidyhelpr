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
          purrr::map_chr(
            seq_len(n_rows),
            # Sample 10 letters for each column
            ~ paste(sample(union(letters, LETTERS), 10), collapse = "")
          )
        ) %>%
          rlang::set_names(.col)
      }
    )
  }

  return(.df_random)
}
