# tests for RPKC-S
context("wide_class")

data(samp)
df <- data.frame(t(samp), stringsAsFactors = FALSE)
df[ , 'taxon'] <- rownames(df)
df <- df[1:6, ]
taxa.var = 'taxon'
out <- wide_class(df, taxa.var = taxa.var, db = 'itis')
outcomm <- out$comm



test_that("wide_class returns the correct value", {
  expect_that(outcomm$Genus[outcomm$taxon == 'Acentrella turbida'], equals("Acentrella"))
  expect_that(is.na(outcomm$Genus[outcomm$taxon == 'Baetidae']), is_true())
  expect_that(outcomm$Class[outcomm$taxon == 'Ephemeroptera'], equals("Insecta"))
  expect_that(outcomm$A[outcomm$taxon == 'Ephemeroptera'], equals(df$A[df$taxon== 'Ephemeroptera']))
})

test_that("wide_class returns dim of original data", {
  expect_that(nrow(outcomm), equals(nrow(df)))
})

test_that("wide_class returns correct class", {
  expect_that(out, is_a('wide_class'))
  expect_that(all(names(out) %in% c("comm", "hnames", "taxa.var")), is_true())
})

test_that("other information is correct", {
  expect_that(out$taxa.var, equals(taxa.var))
  expect_that(length(names(outcomm)) - length(out$hnames), equals(ncol(df)))
})
