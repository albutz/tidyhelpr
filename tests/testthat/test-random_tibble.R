test_that("random_tibble with no column types", {
  res_act <- withr::with_seed(
    seed = 42,
    random_tibble(c("x_1", "X 2"), .n_rows = 10)
  )

  res_exp <- tibble::tribble(
    ~x_1, ~x_2,
    "condimentum", "Vitae",
    "ante", "Vitae",
    "phasellus", "dolor",
    "luctus", "est",
    "sed", "ac",
    "ut", "sociosqu",
    "sed", "magna",
    "vitae", "in",
    "turpis", "litora",
    "venenatis", "praesent"
  )

  expect_equal(res_act, res_exp)
})
