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

test_that("RPMC-S  correct action", {
  expect_that(all(comm$A[out$action == 'removed'] == 0), is_true())
})


df_w2 <- structure(list(
  comm = structure(list(taxon = c("Chironomidae",  "Chironominae", "Chironomini", 
                                  "Chernoushii", "Chironomus", "Cladopelma", "Tanytarsini", 
                                  "Cladotanytarsus", "Microspectra", "Neozaurelia", 
                                  "Diamesinae", "Diamesa", "Pagastia", "Potthastia"), 
                        abu = c(100L, 225L, 100L, 10L, 20L, 5L, 5L, 15L, 20L, 30L, 
                                10L, 15L, 35L, 50L)), 
                   .Names = c("taxon", "abu"), 
                   class = "data.frame", 
                   row.names = c(NA, -14L)), 
  hier = structure(list(Family = c("Chironomidae", "Chironomidae","Chironomidae", 
                                   "Chironomidae", "Chironomidae", "Chironomidae", 
                                   "Chironomidae", "Chironomidae", "Chironomidae",
                                   "Chironomidae",  "Chironomidae", "Chironomidae", 
                                   "Chironomidae", "Chironomidae"), 
                        Subfamily = c(NA, "Chironominae", "Chironominae", "Chironominae", 
                                      "Chironominae", "Chironominae", "Chironominae", 
                                      "Chironominae", "Chironominae", "Chironominae", 
                                      "Diamesinae", "Diamesinae", "Diamesinae", "Diamesinae"), 
                        Tribe = c(NA, NA, "Chironomini", "Chironomini", "Chironomini", 
                                  "Chironomini", "Tanytarsini", "Tanytarsini", "Tanytarsini", 
                                  "Tanytarsini", NA, NA, NA, NA), 
                        Genus = c(NA, NA, NA, "Chernoushii", "Chironomus", "Cladopelma", NA, 
                                  "Cladotanytarsus", "Microspectra", "Neozaurelia", NA, 
                                  "Diamesa", "Pagastia", "Potthastia"), 
                        taxon = c("Chironomidae",  "Chironominae", "Chironomini", 
                                  "Chernoushii", "Chironomus", "Cladopelma", "Tanytarsini", 
                                  "Cladotanytarsus", "Microspectra", "Neozaurelia", 
                                  "Diamesinae", "Diamesa", "Pagastia", "Potthastia")), 
                   .Names = c("Family", "Subfamily", "Tribe", "Genus", "taxon"), 
                   class = "data.frame", row.names = c(NA,-14L)), 
  taxa.var = "taxon"), 
  .Names = c("comm", "hier", "taxa.var" ), 
  class = "wide_class")
out2 <- rpmc_s(df_w2, value.var = 'abu')


test_that("RPMC-S second example", {
  expect_that(out2$comm[, "abu"], equal(c(0, 430, 0,0,0,0,0,0,0,0,0,15,35,50)))
})

