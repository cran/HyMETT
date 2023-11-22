test_that("calculate qpearsonIII", {

  # test calc_qpearsonIII and calc_qlpearsonIII, expect returned numeric value
  r <- calc_qpearsonIII(p = 0.1)
  expect_true(is.numeric(r))
  
  r <- calc_qlpearsonIII(p = 0.1)
  expect_true(is.numeric(r))
})
