# tests for DPAC-S
context("DPAC-S")

data(samp)
df <- data.frame(t(samp), stringsAsFactors = FALSE)
df[ , 'taxon'] <- rownames(df)
taxa.var = 'taxon'
value.var = 'A'
df_w <- get_hier(df, taxa.var = taxa.var, db = 'itis')
out <- dpac_s(df_w, value.var = value.var)
comm <- out$comm

test_that("DPAC-S returns the correct value", {
  expect_that(comm[comm$taxon == 'Acentrella', value.var], equals(0))
  expect_that(comm[comm$taxon == 'Baetidae', value.var], equals(0))
  expect_that(comm[comm$taxon == 'Zygoptera', value.var], equals(0))
  expect_that(round(comm[comm$taxon == 'Argia', value.var], 2), equals(118.94))
  expect_that(round(comm[comm$taxon == 'Baetis intercalaris', value.var], 2), equals(221.02))
})

test_that("DPAC-S returns correct object", {
  expect_that(nrow(comm), equals(nrow(df)))
  expect_that(ncol(comm), equals(2))
  expect_that(all(comm[ ,taxa.var] == df[ ,taxa.var]), is_true())
  expect_that(sum(comm[ , value.var]) == sum(df_w$comm[ ,value.var]), is_true())
})

test_that("DPAC-S correct action", {
  expect_that(all(comm$A[out$action == 'removed'] == 0), is_true())
})
