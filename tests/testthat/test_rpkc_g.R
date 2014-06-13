# tests for RPKC-S
context("RPKC-G")

data(samp)
df <- data.frame(t(samp), stringsAsFactors = FALSE)
df[ , 'taxon'] <- rownames(df)
taxa.var = 'taxon'
value.var = 'S2'
group = c('S1', 'S2', 'S3', 'S4')

df_w <- get_hier(df, taxa.var = taxa.var, db = 'itis')

out_c <- rpkc_g(df_w, value.var = value.var, group = group, option = 'C')
comm_c <- out_c$comm

test_that("RPKC-S returns the correct value", {
  expect_that(comm_c[comm_c$taxon == 'Acentrella', value.var], equals(0))
  expect_that(comm_c[comm_c$taxon == 'Baetidae', value.var], equals(0))
  expect_that(comm_c[comm_c$taxon == 'Acentrella parvula', value.var], equals(30))
  expect_that(comm_c[comm_c$taxon == 'Acentrella turbida', value.var], equals(0))
})

test_that("RPKC-S returns correct object", {
  expect_that(nrow(comm_c), equals(nrow(df)))
  expect_that(ncol(comm_c), equals(2))
  expect_that(all(comm_c[ ,taxa.var] == df[ ,taxa.var]), is_true())
})

test_that("correct action", {
  expect_that(all(comm_c$A[out$action == 'removed'] == 0), is_true())
})


out_l <- rpkc_g(df_w, value.var = value.var, group = group, option = 'L')
comm_l <- out_l$comm

test_that("RPKC-S returns the correct value", {
  expect_that(comm_l[comm_l$taxon == 'Acentrella', value.var], equals(0))
  expect_that(comm_l[comm_l$taxon == 'Baetidae', value.var], equals(0))
  expect_that(round(comm_l[comm_l$taxon == 'Acentrella parvula', value.var], 2), equals(25.66))
  expect_that(round(comm_l[comm_l$taxon == 'Acentrella turbida', value.var], 2), equals(4.34))
})

test_that("RPKC-S returns correct object", {
  expect_that(nrow(comm_l), equals(nrow(df)))
  expect_that(ncol(comm_l), equals(2))
  expect_that(all(comm_l[ ,taxa.var] == df[ ,taxa.var]), is_true())
})

test_that("correct action", {
  expect_that(all(comm_l$A[out$action == 'removed'] == 0), is_true())
})
