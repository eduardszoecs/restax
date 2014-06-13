# tests for RPKC-S
context("format_class")

testdf <- structure(list(name = c("Animalia", "Bilateria", "Protostomia", 
                                  "Ecdysozoa", "Arthropoda", "Hexapoda", 
                                  "Insecta"), 
                         rank = c("Kingdom", "Subkingdom", "Infrakingdom", 
                                  "Superphylum", "Phylum", "Subphylum", "Class")),
                    .Names = c("name", "rank"), 
                    row.names = c(NA, 7L), class = "data.frame")

out <- format_class(testdf)


test_that("format_class returns the correct value", {
  expect_that(out[ , "Kingdom"] , equals("Animalia"))
  expect_that(out[ , "Class"], equals("Insecta"))
})

test_that("format_class returns the correct class", {
  expect_that(out , is_a("data.frame"))
  expect_that(nrow(out), equals(1))
})
