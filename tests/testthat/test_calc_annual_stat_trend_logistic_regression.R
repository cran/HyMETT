test_that("calculate annual stat trend and logistic regression", {

  # run calc_annual_stat_trend on HyMETT::example_annual dataset
  r <- calc_annual_stat_trend(data = example_annual,
                              year = "WY",
                              value = "high_q1")

  # expect returned dataframe with data
  expect_s3_class(r, class = "data.frame")
  expect_true(length(r) > 0)

  # run calc_logistic_regression on HyMETT::example_annual dataset
  r <- calc_logistic_regression(data = example_annual,
                                year = "WY",
                                value = "high_q1")

  # expect returned dataframe with data
  expect_s3_class(r, class = "data.frame")
  expect_true(length(r) > 0)
})
