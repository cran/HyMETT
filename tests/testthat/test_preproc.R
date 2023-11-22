test_that("preproc functions", {
  
  # test preprocessing functions on HyMETT::example_obs and HyMETT::example_preproc datasets
  # expect returned data.frames or lists of data.frames
  r <- preproc_audit_data(data = example_preproc, Date = "Date", value = "value", year_group = "WY")
  expect_s3_class(r, class = "data.frame")
  expect_true(length(r) == 3)
  expect_true(nrow(r) == 27)
  
  r <- preproc_fill_daily(Date = c(seq.Date(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "1 day"),
                                   seq.Date(as.Date("2020-01-20"), as.Date("2020-01-31"), by = "1 day")),
                          value = c(seq.int(1, 22, 1)))
  expect_true(nrow(r) == 31)
  expect_true(NA %in% r$value)
  
  r <- preproc_main(data = example_obs, Date = "Date", value = "streamflow_cfs", longitude = -68)
  expect_type(r, "list")
  expect_length(r, 3)
  expect_s3_class(r[[1]], class = "data.frame")
  expect_s3_class(r[[2]], class = "data.frame")
  expect_s3_class(r[[3]], class = "data.frame")
  
  r <- preproc_precondition_data(data = example_obs, Date = "Date", value = "streamflow_cfs")
  expect_s3_class(r, class = "data.frame")
  expect_length(r, 12)
  expect_true(nrow(r) == 9496)
  
  Date = c(seq.Date(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "1 day"),
           seq.Date(as.Date("2020-01-20"), as.Date("2020-01-31"), by = "1 day"))
  value = c(seq.int(1, 22, 1))
  expect_error(preproc_validate_daily(Date = Date, value = value))
})