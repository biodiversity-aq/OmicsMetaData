#==============================================================
# Author Maxime Sweetlove
# license CC-0
# Part of the POLA3R website (successor or mARS.biodiversity.aq)
# version 1.0 (2021-09-23)
# file encding UTF-8
#
#==============================================================
library(OmicsMetaData)
library(testthat)

test_that("all data is loaded", {
  expect_s3_class(TaxIDLib, "data.frame")
  expect_s3_class(TermsLib, "data.frame")
  expect_type(TermsSyn, "list")
  expect_s3_class(ENA_checklistAccession, "data.frame")
  expect_type(ENA_geoloc, "character")
  expect_type(ENA_instrument, "character")
  expect_type(ENA_select, "character")
  expect_type(ENA_strat, "character")
})

sampleNames <- c("sample_1", "sample_2")
test_MIxS <- new("MIxS.metadata",
                 data = data.frame(var1=c(1,2), var2=c(3,4), eventID=sampleNames, 
                                   row.names=sampleNames),
                 section = c(var1="section1", var2="section1", eventID="miscellaneous"),
                 units      = c(var1="unit1", var2="unit2", eventID="alphanumeric"),
                 env_package = "water",
                 type = "versatile",
                 QC = TRUE)

test_that("MIxS.metadata show works", {
  expect_s4_class(test_MIxS, "MIxS.metadata")
  expect_message(show(test_MIxS))
})

test_that("check.valid.MIxS.metadata() works", {
  expect_equal(check.valid.MIxS.metadata(data.frame()), FALSE)
  expect_equal(check.valid.MIxS.metadata(test_MIxS), TRUE)
})

test_that("check.valid.metadata.DwC() works", {
  expect_equal(check.valid.metadata.DwC(data.frame()), FALSE)
})

test_that("write.MIxS() works", {
  expect_error(write.MIxS(x=NULL, file="test.csv"))
})

