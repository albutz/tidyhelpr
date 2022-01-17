#' Split by group and name by group key
#'
#' This wrapper function performs a group split via
#' \code{\link[dplyr]{group_split}} and renames the resulting list elements
#' with the group keys.
#'
#' @param .df A \code{\link[tibble]{tibble}}.
#' @param .group_vars The column names to group by.
#'
#' @return A list of \code{\link[tibble]{tibble}}s split by group named by keys.
#' @export
#'
#' @examples
#' split_groups(
#'   datasets::iris,
#'   "Species"
#' )
split_groups <- function(.df, .group_vars) {
  .df_grouped <- .df

  for (.group in .group_vars) {
    .df_grouped <- .df_grouped %>%
      dplyr::group_by(!!rlang::sym(.group), .add = TRUE)
  }

  .keys <- dplyr::group_keys(.df_grouped) %>%
    # Combine multiple columns as each group key gets a separate column
    tidyr::unite(col = "key", tidyselect::everything(), sep = " + ") %>%
    dplyr::pull()

  .df_split <- dplyr::group_split(.df_grouped, .keep = FALSE) %>%
    rlang::set_names(.keys)

  return(.df_split)
}
