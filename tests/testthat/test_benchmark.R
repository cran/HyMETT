test_that("Benchmark functions",{

  r <- benchmark_KGE_DOY(example_preproc)
  expect_true(checkmate::test_data_frame(r))
  expect_named(r, expected = c("KGE_DOY_mean", "KGE_DOY_median"))

  r <- round(r, digits = 2)
  expect_equal(r$KGE_DOY_mean,   0.73)
  expect_equal(r$KGE_DOY_median, 0.67)
})