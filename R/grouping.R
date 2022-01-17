split_groups <- function(.df, .group_vars) {
  .df_grouped <- .df

  for (.group in .group_vars) {
    .df_grouped %<>% dplyr::group_by(!!rlang::sym(.group), .add = TRUE)
  }

  .keys <- dplyr::group_keys(.df_grouped) %>%
    # Combine multiple columns as each group key gets a separate column
    tidyr::unite(col = "key", tidyselect::everything(), sep = " + ") %>%
    dplyr::pull()

  .df_split <- dplyr::group_split(.df_grouped) %>%
    rlang::set_names(.keys)

  return(.df_split)
}
