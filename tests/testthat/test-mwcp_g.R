# tests for RPKC-S
context("MCWP-G")

data(samp)
df <- data.frame(t(samp), stringsAsFactors = FALSE)
df[ , 'taxon'] <- rownames(df)
taxa.var = 'taxon'
value.var = 'S3'
df_w <- get_hier(df, taxa.var = taxa.var, db = 'itis')

out_f <- mcwp_g(df_w, value.var = 'S3', group = c('S1', 'S2', 'S3', 'S4'), level = 'Family')
comm_f <- out_f$comm

out_o <- mcwp_g(df_w, value.var = 'S3', group = c('S1', 'S2', 'S3', 'S4'), level = 'Order')
comm_o <- out_o$comm

out_c <-  mcwp_g(df_w, value.var = 'S3', group = c('S1', 'S2', 'S3', 'S4'), level = 'Class')
comm_c <- out_c$comm

test_that("MCWP-G returns the correct value", {
  expect_that(comm_f[comm_f$taxon == 'Acentrella', value.var], equals(0))
  expect_that(comm_f[comm_f$taxon == 'Baetidae', value.var], equals(66))
  expect_that(comm_f[comm_f$taxon == 'Ephemeroptera', value.var], equals(0))
  expect_that(comm_f[comm_f$taxon == 'Argia', value.var], equals(0))
  #
  expect_that(comm_o[comm_o$taxon == 'Acentrella', value.var], equals(0))
  expect_that(comm_o[comm_o$taxon == 'Baetidae', value.var], equals(0))
  expect_that(comm_o[comm_o$taxon == 'Ephemeroptera', value.var], equals(166))
  expect_that(comm_o[comm_o$taxon == 'Argia', value.var], equals(0))
  expect_that(comm_o[comm_o$taxon == 'Zygoptera', value.var], equals(10))
  #
  expect_that(comm_c[comm_c$taxon == 'Acentrella', value.var], equals(0))
  expect_that(comm_c[comm_c$taxon == 'Baetidae', value.var], equals(0))
  expect_that(comm_c[comm_c$taxon == 'Ephemeroptera', value.var], equals(0))
  expect_that(comm_c[comm_c$taxon == 'Insecta', value.var], equals(176))
})

test_that("MCWP-G returns correct object", {
  expect_that(nrow(comm_c), equals(nrow(df)))
  expect_that(ncol(comm_c), equals(2))
  expect_that(all(comm_c[ ,taxa.var] == df[ ,taxa.var]), is_true())
  expect_that(nrow(comm_o), equals(nrow(df)))
  expect_that(ncol(comm_o), equals(2))
  expect_that(all(comm_o[ ,taxa.var] == df[ ,taxa.var]), is_true())
  expect_that(nrow(comm_f), equals(nrow(df)))
  expect_that(ncol(comm_f), equals(2))
  expect_that(all(comm_f[ ,taxa.var] == df[ ,taxa.var]), is_true())
})

test_that("MCWP-F returns correct action", {
  expect_that(all(comm_c$A[out_c$action == 'removed'] == 0), is_true())
  expect_that(all(comm_o$A[out_o$action == 'removed'] == 0), is_true())
  expect_that(all(comm_f$A[out_f$action == 'removed'] == 0), is_true())
})


out_f2 <- mcwp_g(df_w, group = c('S1', 'S2', 'S3', 'S4'), level = 'Family')
comm_f2 <- out_f2$comm
out_o2 <- mcwp_g(df_w, group = c('S1', 'S2', 'S3', 'S4'), level = 'Order')
comm_o2 <- out_o2$comm
out_c2 <-  mcwp_g(df_w, group = c('S1', 'S2', 'S3', 'S4'), level = 'Class')
comm_c2 <- out_c2$comm

test_that("MCWP-G works on whole data.frame", {
  expect_that(comm_f2[comm_f2$taxon == 'Acentrella', "S3"], equals(0))
  expect_that(comm_f2[comm_f2$taxon == 'Baetidae', "S3"], equals(66))
  expect_that(comm_f2[comm_f2$taxon == 'Baetidae', "S1"], equals(206))
  expect_that(comm_f2[comm_f2$taxon == 'Argia', "S4"], equals(8))
  #
  expect_that(comm_o2[comm_o2$taxon == 'Acentrella', "S3"], equals(166))
  expect_that(comm_o2[comm_o2$taxon == 'Baetidae', "S3"], equals(0))
  expect_that(comm_o2[comm_o2$taxon == 'Argia', "S3"], equals(0))
  expect_that(comm_o2[comm_o2$taxon == 'Zygoptera', "S3"], equals(10))
  expect_that(comm_o2[comm_o2$taxon == 'Zygoptera', "S4"], equals(108))
  #
  expect_that(comm_c2[comm_c2$taxon == 'Acentrella', "S4", equals(0))
  expect_that(comm_c2[comm_c2$taxon == 'Baetidae', "S3"], equals(0))
  expect_that(comm_c2[comm_c2$taxon == 'Ephemeroptera', "S4"], equals(0))
  expect_that(comm_c2[comm_c2$taxon == 'Insecta', "S3"], equals(176))
  #
  expect_that(nrow(comm_c2), equals(nrow(df)))
  expect_that(nrow(comm_o2), equals(nrow(df)))
  expect_that(ncol(comm_c2), equals(ncol(df)))
  expect_that(ncol(comm_o2), equals(ncol(df)))
  
  expect_that(all(rowSums(comm_f2[out_f2$action == 'removed', 1:5]) == 0), is_true())
  expect_that(all(comm_f2[ ,taxa.var] == df[ ,taxa.var]), is_true())
})
