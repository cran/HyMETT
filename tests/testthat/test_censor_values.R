test_that("censor values", {

  # run censor_values and test that NAs returned
  r <- censor_values(value = seq.int(1, 10, 1), censor_threshold = 5)
  expect_false(all(is.na(r)))
  expect_true(is.na(r[1]))
})
