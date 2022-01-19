#' Encoding guess
#'
#' Guess the locale for \code{.file} via \code{\link[readr]{guess_encoding}}.
#'
#' @param .file A path to a file or a connection.
#'
#' @return A \code{\link[readr]{locale}} object.
#'
guess_locale <- function(.file) {
  .candidates <- readr::guess_encoding(.file, n_max = -1)
  .buest_guess <- dplyr::slice_max(.candidates, .data$confidence) %>%
    dplyr::pull(.data$encoding)
  .locale <- readr::locale(encoding = .buest_guess)

  return(.locale)
}

#' Locate index of pattern
#'
#' Search each line in \code{.lines} for the regex \code{.pattern} and return
#' the first index of a match. Yields an error if the pattern cannot be located.
#'
#' @param .lines A character vector.
#' @param .pattern Pattern to look for.
#'
#' @return An integer value.
#'
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

#' Filter relevant lines
#'
#' Subset lines to the relevant part, starting (inclusive) with the line in
#' which \code{.pattern} is located.
#'
#' @param .file A path to a file or a connection.
#' @param .pattern Pattern to look for.
#' @param .locale A \code{\link[readr]{locale}} object.
#'
#' @return A character vector.
#'
subset_lines <- function(.file, .pattern, .locale) {
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

#' Subset lines and read into a \code{\link[tibble]{tibble}}
#'
#' Read in the lines in \code{.file}, (optionally) identify the first line to
#' match \code{.pattern} and starting here read into a
#' \code{\link[tibble]{tibble}} with \code{.delim} as separator and clean names
#' via \code{\link[janitor]{clean_names}}. All lines
#' are read in if no \code{.pattern} is passed (the default).
#'
#' @param .file A path to a file or a connection.
#' @param .pattern Pattern to look for.
#' @param .delim Single character used to separate fields within a record.
#' @param ... Further arguments passed to \code{\link[readr]{read_delim}}.
#'
#' @return A \code{\link[tibble]{tibble}}.
#' @export
#'
#' @examples
#' lines <- c(
#'   "key_1,val_1",
#'   "key_2,val_2",
#'   "",
#'   "y,x_1,x_2",
#'   "1,low,0.989",
#'   "0,medium,-1.923"
#'  )
#'  temp_file <- tempfile()
#'  writeLines(lines, temp_file)
#'  read_sub(temp_file, "(?=.*y)(?=.*x_1)(?=.*x_2)")
#'
read_sub <- function(.file, .pattern = NULL, .delim = ",", ...) {
  .locale <- guess_locale(.file)

  if (!is.null(.pattern)) {
    .lines <- subset_lines(.file, .pattern, .locale)
  } else {
    .lines <- readr::read_lines(.file, locale = .locale)
  }

  .tbl <- readr::read_delim(I(.lines), .delim, ...) %>%
    janitor::clean_names()

  return(.tbl)
}
