# tests for RPKC-S
context("RPMC-S")

data(samp)
df <- data.frame(t(samp), stringsAsFactors = FALSE)
df[ , 'taxon'] <- rownames(df)
taxa.var = 'taxon'
value.var = 'A'
df_w <- get_hier(df, taxa.var = taxa.var, db = 'itis')
out <- rpmc_s(df_w, value.var = value.var)
comm <- out$comm

test_that("RPMC-S returns the correct value", {
  expect_that(comm[comm$taxon == 'Acentrella', value.var], equals(0))
  expect_that(comm[comm$taxon == 'Baetidae', value.var], equals(0))
  expect_that(comm[comm$taxon == 'Zygoptera', value.var], equals(118))
  expect_that(comm[comm$taxon == 'Argia', value.var], equals(0))
})

test_that("RPMC-S returns correct object", {
  expect_that(nrow(comm), equals(nrow(df)))
  expect_that(ncol(comm), equals(2))
  expect_that(all(comm[ ,taxa.var] == df[ ,taxa.var]), is_true())
})

test_that("correct action", {
  expect_that(all(comm$A[out$action == 'removed'] == 0), is_true())
})
