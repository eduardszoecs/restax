# tests for RPMC-G
context("RPMC-G")

data(samp)
df <- data.frame(t(samp), stringsAsFactors = FALSE)
df[ , 'taxon'] <- rownames(df)
taxa.var = 'taxon'
value.var = 'S3'
df_w <- get_hier(df, taxa.var = taxa.var, db = 'itis')
out <- rpmc_g(df_w, value.var = value.var, group = c('S1', 'S2', 'S3', 'S4'))
comm <- out$comm

test_that("RPMC-G returns the correct value", {
  expect_that(comm[comm$taxon == 'Acentrella', value.var], equals(0))
  expect_that(comm[comm$taxon == 'Baetidae', value.var], equals(0))
  expect_that(comm[comm$taxon == 'Zygoptera', value.var], equals(10))
  expect_that(comm[comm$taxon == 'Argia', value.var], equals(0))
})

test_that("RPMC-G returns correct object", {
  expect_that(nrow(comm), equals(nrow(df)))
  expect_that(ncol(comm), equals(2))
  expect_that(all(comm[ ,taxa.var] == df[ ,taxa.var]), is_true())
})

test_that("RPMC-G correct action", {
  expect_that(all(comm$A[out$action == 'removed'] == 0), is_true())
})


out2 <- rpmc_g(df_w, value.var = NULL, group = c('S1', 'S2', 'S3', 'S4'))
comm2 <- out2$comm

test_that("RPMC-G works on whole data.frame", {
  expect_that(comm2[comm2$taxon == 'Acentrella', 'S3'], equals(0))
  expect_that(comm2[comm2$taxon == 'Zygoptera', 'S3'], equals(10))
  expect_that(comm2[comm2$taxon == 'Zygoptera', 'S4'], equals(108))
  expect_that(nrow(comm2), equals(nrow(df)))
  expect_that(ncol(comm2), equals(ncol(df)))
  expect_that(all(rowSums(comm2[out2$action == 'removed', 2:6]) == 0), is_true())
  expect_that(all(comm2[out2$action == 'keep', 2:6] == df_w$comm[out2$action == 'keep', 1:5]), is_true())
})

test_that("RPMC-S returns correct object", {
  expect_that(nrow(comm), equals(nrow(df)))
  expect_that(ncol(comm), equals(2))
  expect_that(all(comm[ ,taxa.var] == df[ ,taxa.var]), is_true())
})
