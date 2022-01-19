guess_locale <- function(.file) {
  .candidates <- readr::guess_encoding(.file, n_max = -1)
  .buest_guess <- dplyr::slice_max(.candidates, .data$confidence) %>%
    dplyr::pull(.data$encoding)
  .locale <- readr::locale(encoding = .buest_guess)

  return(.locale)
}

locate_pattern <- function(.lines, .pattern) {
  .idx <- purrr::detect_index(
    .lines,
    ~ stringr::str_detect(.x, .pattern)
  )

  if (.idx == 0) {
    rlang::abort(
      glue::glue("Pattern not found.")
    )
  }

  return(.idx)
}

split_lines <- function(.file, .pattern, .locale) {
  .lines <- readr::read_lines(.file, locale = .locale)
  .split_point <- locate_pattern(.lines, .pattern)

  if (.split_point == 1) {
    # case when all lines are relevant
    return(.lines)
  } else if (.split_point == length(.lines)) {
    # case when no lines are relevant
    return(character())
  } else {
    # case when a subset of lines is relevant
    return(.lines[.split_point:length(.lines)])
  }
}

read_sub <- function(.file, .pattern = NULL, .delim = ",", ...) {
  .locale <- guess_locale(.file)

  if (!is.null(.pattern)) {
    .lines <- split_lines(.file, .pattern, .locale)
  } else {
    .lines <- readr::read_lines(.file, locale = .locale)
  }

  .tbl <- readr::read_delim(I(.lines), .delim, ...) %>%
    janitor::clean_names()

  return(.tbl)
}
