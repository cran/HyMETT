test_that("calculate annual flow stats", {

  # run calc_annual_flow_stats on HyMETT::example_preproc dataset
  r <- calc_annual_flow_stats(data = example_preproc,
                              Date = "Date",
                              year_group = "WY",
                              Q = "value",
                              Q3 = "Q3",
                              Q7 = "Q7",
                              Q30 = "Q30",
                              jd = "jd",
                              calc_high = T,
                              calc_low = T,
                              calc_percentiles = T,
                              calc_monthly = T,
                              calc_WSCVD = T,
                              longitude = -68,
                              calc_ICVD = T,
                              zero_threshold = 33,
                              quantile_type = 8,
                              na.action = "na.omit")

  # expect returned dataframe with >0 rows and columns
  expect_s3_class(r, class = "data.frame")
  expect_true(length(r) > 0)
  expect_true(nrow(r) > 0)

  # expect returned dataframe to equal HyMETT::example_annual dataset
  expect_equal(nrow(r), nrow(example_annual))
})
