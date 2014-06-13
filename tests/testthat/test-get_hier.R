# tests for get_hier
context("get_hier")

data(samp)
df <- data.frame(t(samp), stringsAsFactors = FALSE)
df[ , 'taxon'] <- rownames(df)
df <- df[1:6, ]
taxa.var = 'taxon'
out <- get_hier(df, taxa.var = taxa.var, db = 'itis')
outcomm <- out$comm
outhier <- out$hier



test_that("wide_class returns the correct value", {
  expect_that(outhier$Genus[outcomm$taxon == 'Acentrella turbida'], equals("Acentrella"))
  expect_that(is.na(outhier$Genus[outcomm$taxon == 'Baetidae']), is_true())
  expect_that(outhier$Class[outcomm$taxon == 'Ephemeroptera'], equals("Insecta"))
  expect_that(outcomm$A[outcomm$taxon == 'Ephemeroptera'], equals(df$A[df$taxon== 'Ephemeroptera']))
})

test_that("wide_class returns dim of original data", {
  expect_that(dim(outcomm), equals(dim(df)))
  expect_that(all(outcomm[ , taxa.var] == df[ , taxa.var]), is_true())
  expect_that(all(outhier[ , taxa.var] == df[ , taxa.var]), is_true())
})

test_that("wide_class returns correct class", {
  expect_that(out, is_a('wide_class'))
  expect_that(all(names(out) %in% c("comm", "hier", "taxa.var")), is_true())
})

test_that("other information is correct", {
  expect_that(out$taxa.var, equals(taxa.var))
})
