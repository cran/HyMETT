test_that("POR functions", {

  # test POR functions on HyMETT::example_annual and HyMETT::example_preproc daily datasets
  # expect data.frames or numeric doubles returned
  r <- POR_apply_annual_hiflow_stats(example_annual[ , c("high_q1", "high_q3")])
  expect_s3_class(r, class = "data.frame")
  expect_true(length(r) == 3)
  expect_true(nrow(r) == 4)
  
  r <- POR_apply_annual_lowflow_stats(example_annual[ , c("low_q1", "low_q3")])
  expect_s3_class(r, class = "data.frame")
  expect_true(length(r) == 3)
  expect_true(nrow(r) == 4)
  
  r <- POR_calc_amp_and_phase(data = example_preproc, Date = "Date", value = "value")
  expect_s3_class(r, class = "data.frame")
  expect_true(length(r) == 2)
  expect_true(nrow(r) == 2)
  
  r <- POR_calc_AR1(data = example_preproc, Date = "Date", value = "value")
  expect_type(r, "double")
  expect_vector(r, size = 1)
  
  r <- POR_calc_lp3_quantile(example_annual[["low_q1"]], p = 0.1)
  expect_type(r, "double")
  expect_vector(r, size = 1)
  
  r <- POR_deseasonalize(data = example_preproc, Date = "Date", value = "value")
  expect_type(r, "double")
  expect_vector(r, size = nrow(example_preproc))
  
  r <- POR_distribution_metrics(example_preproc$value)
  expect_s3_class(r, class = "data.frame")
  expect_true(length(r) == 2)
  expect_true(nrow(r) == 17)
})