test_that("split_groups with a single grouping variable", {
  data <- datasets::iris |>
    janitor::clean_names() |>
    dplyr::slice(c(1, 25, 51, 75, 101, 120))

  res_act <- split_groups(data, species)

  res_exp <- list(
    setosa = tibble::tibble(
      sepal_length = c(5.1, 4.8),
      sepal_width = c(3.5, 3.4),
      petal_length = c(1.4, 1.9),
      petal_width = c(0.2, 0.2)
    ),
    versicolor = tibble::tibble(
      sepal_length = c(7.0, 6.4),
      sepal_width = c(3.2, 2.9),
      petal_length = c(4.7, 4.3),
      petal_width = c(1.4, 1.3)
    ),
    virginica = tibble::tibble(
      sepal_length = c(6.3, 6.0),
      sepal_width = c(3.3, 2.2),
      petal_length = c(6.0, 5.0),
      petal_width = c(2.5, 1.5)
    )
  )

  expect_equal(res_act, res_exp, ignore_attr = TRUE)
})

test_that("split_groups with multiple grouping variables", {
  data <- datasets::mtcars |>
    janitor::clean_names() |>
    tibble::rownames_to_column("model") |>
    dplyr::mutate(
      manufacturer = dplyr::if_else(
        stringr::str_starts(.data$model, "([Mm]erc)|(Porsche)"),
        "german",
        "not german"
      ),
      transmission_type = dplyr::if_else(.data$am == 1, "manual", "automatic")
    ) |>
    dplyr::slice(c(7:9, 14:15, 26:28))

  res_act <- split_groups(data, manufacturer, transmission_type)

  res_exp <- list(
    `german + automatic` = tibble::tribble(
      ~model, ~mpg, ~cyl, ~disp, ~hp, ~drat, ~wt, ~qsec, ~vs, ~am, ~gear, ~carb,
      "Merc 240D", 24.4, 4, 146.7, 62, 3.69, 3.19, 20, 1, 0, 4, 2,
      "Merc 230", 22.8, 4, 140.8, 95, 3.92, 3.15, 22.9, 1, 0, 4, 2,
      "Merc 450SLC", 15.2, 8, 275.8, 180, 3.07, 3.78, 18, 0, 0, 3, 3
    ),
    `german + manual` = tibble::tribble(
      ~model, ~mpg, ~cyl, ~disp, ~hp, ~drat, ~wt, ~qsec, ~vs, ~am, ~gear, ~carb,
      "Porsche 914-2", 26, 4, 120.3, 91, 4.43, 2.14, 16.7, 0, 1, 5, 2
    ),
    `not german + automatic` = tibble::tribble(
      ~model, ~mpg, ~cyl, ~disp, ~hp, ~drat, ~wt, ~qsec, ~vs, ~am, ~gear, ~carb,
      "Duster 360", 14.3, 8, 360, 245, 3.21, 3.57, 15.84, 0, 0, 3, 4,
      "Cadillac Fleetwood", 10.4, 8, 472, 205, 2.93, 5.25, 17.98, 0, 0, 3, 4
    ),
    `not german + manual` = tibble::tribble(
      ~model, ~mpg, ~cyl, ~disp, ~hp, ~drat, ~wt, ~qsec, ~vs, ~am, ~gear, ~carb,
      "Fiat X1-9", 27.3, 4, 79, 66, 4.08, 1.935, 18.9, 1, 1, 4, 1,
      "Lotus Europa", 30.4, 4, 95.1, 113, 3.77, 1.513, 16.9, 1, 1, 5, 2
    )
  )

  expect_equal(res_act, res_exp, ignore_attr = TRUE)
})
