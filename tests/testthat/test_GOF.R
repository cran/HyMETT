test_that("GOF functions", {

  # test correlation function, expect warning, returned data.frame with data
  expect_warning(
    r <- GOF_correlation_tests(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
  )
  expect_s3_class(r, class = "data.frame")
  expect_true(length(r) == 3)
  expect_true(nrow(r) == 3)

  # test all other GOF functions, expect numeric double returned
  r <- GOF_kling_gupta_efficiency(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
  expect_type(r, "double")
  expect_vector(r, size = 1)
  
  r <- GOF_mean_absolute_error(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
  expect_type(r, "double")
  expect_vector(r, size = 1)
  
  r <- GOF_mean_error(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
  expect_type(r, "double")
  expect_vector(r, size = 1)
  
  r <- GOF_nash_sutcliffe_efficiency(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
  expect_type(r, "double")
  expect_vector(r, size = 1)
  
  r <- GOF_percent_bias(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
  expect_type(r, "double")
  expect_vector(r, size = 1)
  
  r <- GOF_rmse(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
  expect_type(r, "double")
  expect_vector(r, size = 1)
  
  r <- GOF_rmse(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs, normalize = "stdev")
  expect_type(r, "double")
  expect_vector(r, size = 1)
  
  r <- GOF_volumetric_efficiency(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
  expect_type(r, "double")
  expect_vector(r, size = 1)

  # test GOF_summary, expect warning and data.frame with data returned
  expect_warning(
    r <- GOF_summary(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
  )
  expect_s3_class(r, class = "data.frame")
  expect_true(length(r) == 3)
  expect_true(nrow(r) == 11)
})
