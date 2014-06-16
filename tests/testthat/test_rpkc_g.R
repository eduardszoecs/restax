# tests for RPKC-G
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

test_that("RPKC-G returns the correct value", {
  expect_that(comm_c[comm_c$taxon == 'Acentrella', value.var], equals(0))
  expect_that(comm_c[comm_c$taxon == 'Baetidae', value.var], equals(0))
  expect_that(comm_c[comm_c$taxon == 'Acentrella parvula', value.var], equals(30))
  expect_that(comm_c[comm_c$taxon == 'Acentrella turbida', value.var], equals(0))
})

test_that("RPKC-G returns correct object", {
  expect_that(nrow(comm_c), equals(nrow(df)))
  expect_that(ncol(comm_c), equals(2))
  expect_that(all(comm_c[ ,taxa.var] == df[ ,taxa.var]), is_true())
})

test_that("correct action", {
  expect_that(all(comm_c$A[out_c$action == 'removed'] == 0), is_true())
})


out_l <- rpkc_g(df_w, value.var = value.var, group = group, option = 'L')
comm_l <- out_l$comm

test_that("RPKC-G returns the correct value", {
  expect_that(comm_l[comm_l$taxon == 'Acentrella', value.var], equals(0))
  expect_that(comm_l[comm_l$taxon == 'Baetidae', value.var], equals(0))
  expect_that(round(comm_l[comm_l$taxon == 'Acentrella parvula', value.var], 2), equals(25.66))
  expect_that(round(comm_l[comm_l$taxon == 'Acentrella turbida', value.var], 2), equals(4.34))
})

test_that("RPKC-G returns correct object", {
  expect_that(nrow(comm_l), equals(nrow(df)))
  expect_that(ncol(comm_l), equals(2))
  expect_that(all(comm_l[ ,taxa.var] == df[ ,taxa.var]), is_true())
})

test_that("correct action", {
  expect_that(all(comm_l$A[out_l$action == 'removed'] == 0), is_true())
})


out_l2 <- rpkc_g(df_w, group = group, option = 'L')
comm_l2 <- out_l2$comm
out_c2 <- rpkc_g(df_w, group = group, option = 'C')
comm_c2 <- out_c2$comm

test_that("RPKC-G works on whole data.frame", {
  expect_that(comm_l2[comm_l2$taxon == 'Acentrella', 'S2'], equals(0))
  expect_that(comm_l2[comm_l2$taxon == 'Baetidae', 'S2'], equals(0))
  expect_that(round(comm_l2[comm_l2$taxon == 'Acentrella parvula', 'S2'], 2), equals(25.66))
  expect_that(round(comm_l2[comm_l2$taxon == 'Acentrella turbida', 'S2'], 2), equals(4.34))
  expect_that(comm_l2[comm_l2$taxon == 'Acentrella', 'S3'], equals(0))
  expect_that(comm_l2[comm_l2$taxon == 'Baetidae', 'S3'], equals(0))
  expect_that(comm_l2[comm_l2$taxon == 'Acentrella parvula', 'S3'], equals(26))
  expect_that(comm_l2[comm_l2$taxon == 'Acentrella turbida', 'S3'], equals(0))
  #
  expect_that(nrow(comm_l2), equals(nrow(df)))
  expect_that(nrow(comm_c2), equals(nrow(df)))
  expect_that(ncol(comm_l2), equals(ncol(df)))
  expect_that(ncol(comm_c2), equals(ncol(df)))
  
  expect_that(all(rowSums(comm_l2[out_l2$action == 'removed', 2:6]) == 0), is_true())
  expect_that(all(comm_l2[ ,taxa.var] == df[ ,taxa.var]), is_true())
  
  expect_that(all(rowSums(comm_c2[out_l2$action == 'removed', 2:6]) == 0), is_true())
  expect_that(all(comm_c2[ ,taxa.var] == df[ ,taxa.var]), is_true())
  
})
