test_that("mutate_rows with an existing column", {
  data <- tibble::tibble(
    x = c("some", NA, "random", "feature"),
    y = c(0, 1, 0, 1)
  )

  res_act <- mutate_rows(data, is.na(x), y = -1)
  res_exp <- tibble::tibble(
    x = c("some", NA, "random", "feature"),
    y = c(0, -1, 0, 1)
  )

  expect_equal(res_act, res_exp)
})

test_that("case when evaluation of predicate function yields NA values", {
  data <- tibble::tibble(
    x = c("some", NA, "random", "feature"),
    y = c(0, 1, 0, 1)
  )

  expect_error(mutate_rows(data, nchar(x) < 5, x = "missing"))

  expect_warning({
    res_act <- mutate_rows(
      data, nchar(x) < 5,
      x = "missing", .ignore_na = TRUE
    )
  })
  res_exp <- tibble::tibble(
    x = c("missing", NA, "random", "feature"),
    y = c(0, 1, 0, 1)
  )

  expect_equal(res_act, res_exp)
})

test_that("mutate_rows with multiple existing columns", {
  data <- tibble::tibble(
    x = c(1, 2, 3, 4),
    y = c(0, 1, 0, 1),
    z = c("some", "really", "useful", "feature")
  )

  res_act <- mutate_rows(data, x >= 3, y = 999, z = "")
  res_exp <- tibble::tibble(
    x = c(1, 2, 3, 4),
    y = c(0, 1, 999, 999),
    z = c("some", "really", "", "")
  )

  expect_equal(res_act, res_exp)
})

test_that("mutate_rows with new column", {
  data <- tibble::tibble(
    x = c("some", NA, "random", "feature"),
    y = c(0, 1, 0, 1)
  )

  res_act <- mutate_rows(data, is.na(x), z = 999)
  res_exp <- data <- tibble::tibble(
    x = c("some", NA, "random", "feature"),
    y = c(0, 1, 0, 1),
    z = c(NA, 999, NA, NA)
  )

  expect_equal(res_act, res_exp)
})
