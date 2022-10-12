test_that("test multiserver works", {
  expect_equal(Multiserver(1,1,1), dplyr::tibble(Arrivals = 1, ServiceBegins =1, ChosenServer = 1, ServiceEnds = 2))
  expect_error(Multiserver())
})
