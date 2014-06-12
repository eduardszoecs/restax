# tests for RPKC-S
context("RPKC-S")

data(samp)
samp_w <- wide_class(samp, 'itis', verbose = FALSE)
out <- rpkc_s(samp_w, value.var = 'A')
comm <- out$comm

test_that("RPKC-S returns the correct value", {
  expect_that(comm$A[comm$taxon == 'Insecta'], equals(0))
  expect_that(comm$A[comm$taxon == 'Zygoptera'], equals(0))
  expect_that(comm$A[comm$taxon == 'Argia'], equals(8))
  expect_that(comm$A[comm$taxon == 'Baetis intercalaris'], equals(99))
})

test_that("RPKC-S returns dim of original data", {
  expect_that(nrow(comm), equals(ncol(samp)))
})

test_that("correct action", {
  expect_that(all(comm$A[out$action == 'removed'] == 0), is_true())
})
