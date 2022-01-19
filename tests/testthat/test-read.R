test_that("guess_locale with utf-8", {
  lines <- c(
    "Ḽơᶉëᶆ ȋṕšᶙṁ ḍỡḽǭᵳ ʂǐť ӓṁệẗ",
    "ĉṓɲṩḙċťᶒțûɾ ấɖḯƥĭṩčįɳġ ḝłįʈ",
    "șếᶑ ᶁⱺ ẽḭŭŝḿꝋď ṫĕᶆᶈṓɍ ỉñḉīḑȋᵭṵńť ṷŧ ḹẩḇőꝛế éȶ đꝍꞎôꝛȇ ᵯáꞡᶇā ąⱡîɋṹẵ"
  )
  temp_file <- fs::file_temp()
  writeLines(lines, temp_file)

  res_act <- guess_locale(temp_file)
  res_exp <- readr::locale(encoding = "UTF-8")

  expect_equal(res_act, res_exp)
})

test_that("guess_locale with latin1", {
  lines <- c(
    "some random text",
    "to test guess_locale",
    "with latin1 encoding"
  )
  lines <- iconv(lines, to = "latin1")
  temp_file <- fs::file_temp()
  writeLines(lines, temp_file)

  res_act <- guess_locale(temp_file)
  res_exp <- readr::locale(encoding = "ASCII")

  expect_equal(res_act, res_exp)
})

test_that("locate_pattern if pattern is found", {
  lines <- c(
    "some, ||| random, &$text",
    "'to, %§test, 'locate_pattern'",
    "IN, !tidyhelpr!, "
  )

  expect_equal(locate_pattern(lines, "&"), 1)
  expect_equal(locate_pattern(lines, "^\\'"), 2)
  expect_equal(locate_pattern(lines, "tidyhelpr"), 3)
})

test_that("locate_pattern if pattern is not found", {
  lines <- c(
    "some, ||| random, &$text",
    "'to, %§test, 'locate_pattern'",
    "IN, !tidyhelpr!, "
  )

  expect_error(locate_pattern(lines, "^&"))
})

test_that("subset_lines if pattern is found in first line", {
  lines <- c(
    "some, ||| random, &$text",
    "'to, %§test, 'subset_lines'",
    "IN, !tidyhelpr!, "
  )
  temp_file <- fs::file_temp()
  writeLines(lines, temp_file)

  expect_equal(
    subset_lines(temp_file, "&", readr::locale(encoding = "UTF-8")),
    lines
  )
})

test_that("subset_lines if pattern is found inbetween", {
  lines <- c(
    "some, ||| random, &$text",
    "'to, %§test, 'subset_lines'",
    "IN, !tidyhelpr!, "
  )
  temp_file <- fs::file_temp()
  writeLines(lines, temp_file)

  expect_equal(
    subset_lines(temp_file, "^\\'", readr::locale(encoding = "UTF-8")),
    c("'to, %§test, 'subset_lines'", "IN, !tidyhelpr!, ")
  )
})

test_that("subset_lines if pattern is found in last line", {
  lines <- c(
    "some, ||| random, &$text",
    "'to, %§test, 'subset_lines'",
    "IN, !tidyhelpr!, "
  )
  temp_file <- fs::file_temp()
  writeLines(lines, temp_file)

  expect_length(
    subset_lines(temp_file, "!tidy*", readr::locale(encoding = "UTF-8")),
    0
  )
})

test_that("read_sub with no pattern", {
  lines <- c(
    "y,x_1, x_2",
    "1,low,0.989",
    "0,medium,-1.923"
  )
  temp_file <- fs::file_temp()
  writeLines(lines, temp_file)

  res_act <- read_sub(temp_file, show_col_types = FALSE)
  res_exp <- tibble::tibble(
    y = c(1, 0),
    x_1 = c("low", "medium"),
    x_2 = c(0.989, -1.923)
  )

  expect_equal(res_act, res_exp)
})

test_that("read_sub with clean header and body", {
  lines <- c(
    "key_1,val_1",
    "key_2,val_2",
    "",
    "y,x_1,x_2",
    "1,low,0.989",
    "0,medium,-1.923"
  )
  temp_file <- fs::file_temp()
  writeLines(lines, temp_file)

  res_act <- read_sub(
    temp_file, "(?=.*y)(?=.*x_1)(?=.*x_2)",
    show_col_types = FALSE
  )
  res_exp <- tibble::tibble(
    y = c(1, 0),
    x_1 = c("low", "medium"),
    x_2 = c(0.989, -1.923)
  )

  expect_equal(res_act, res_exp)
})
