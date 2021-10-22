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


test_that("MIxS.metadata show works", {
  sampleNames <- c("sample_1", "sample_2")
  test_MIxS <- new("MIxS.metadata",
                   data = data.frame(var1=c(1,2), var2=c(3,4), eventID=sampleNames, 
                                     row.names=sampleNames),
                   section = c(var1="section1", var2="section1", eventID="miscellaneous"),
                   units      = c(var1="unit1", var2="unit2", eventID="alphanumeric"),
                   env_package = "water",
                   type = "versatile",
                   QC = TRUE)
  test_MIxS2 <- new("MIxS.metadata",
                   data = data.frame(var1=c(1,2), var2=c(3,4), eventID=sampleNames, 
                                     row.names=sampleNames),
                   section = c(var1="section1", var2="section1", eventID="miscellaneous"),
                   units      = c(var1="unit1", var2="unit2", eventID="alphanumeric"),
                   env_package = "water",
                   type = "strict.MIxS",
                   QC = FALSE)
  
  expect_s4_class(test_MIxS, "MIxS.metadata")
  expect_message(show(test_MIxS))
  expect_message(show(test_MIxS2))
})

test_that("check.valid.MIxS.metadata() works", {
  sampleNames <- c("sample_1", "sample_2")
  test_MIxS <- new("MIxS.metadata",
                   data = data.frame(var1=c(1,2), var2=c(3,4), eventID=sampleNames, 
                                     row.names=sampleNames),
                   section = c(var1="section1", var2="section1", eventID="miscellaneous"),
                   units      = c(var1="unit1", var2="unit2", eventID="alphanumeric"),
                   env_package = "water",
                   type = "versatile",
                   QC = TRUE)
  
  expect_equal(check.valid.MIxS.metadata(data.frame()), FALSE)
  expect_equal(check.valid.MIxS.metadata(test_MIxS), TRUE)
})

test_that("DwC.event show works", {
  test_event <- data.frame(eventID=c("sample1", "sample2"),
                           eventDate=c("2021-09-27", "2021-09-28"),
                           decimalLatitude=c("54.7", "33"),
                           decimalLongitude=c("88.9", "-48.4"),
                           row.names=c("sample1", "sample2"))
  test_event_DwC <- new("DwC.event",
                        core = test_event,
                        occurrence = data.frame(),
                        emof = data.frame(),
                        EML.url=as.character(NA),
                        QC=TRUE)
  test_event_DwC2 <- new("DwC.event",
                        core = test_event,
                        occurrence = data.frame(),
                        emof = data.frame(),
                        EML.url=as.character(NA),
                        QC=FALSE)
  
  expect_s4_class(test_event_DwC, "DwC.event")
  expect_message(show(test_event_DwC))
  expect_message(show(test_event_DwC2))
})

test_that("DwC.occurrence show works", {
  test_occ <- data.frame(occurrenceID=c("sample1", "sample2"),
                         basisOfRecord=c("humanObservation", "humanObservation"),
                         eventDate=c("2021-09-27", "2021-09-28"),
                         row.names=c("sample1", "sample2"))
  test_occ_DwC <- new("DwC.occurrence",
                      core = test_occ,
                      emof = data.frame(),
                      EML.url=as.character(NA),
                      QC=TRUE)
  test_occ_DwC2 <- new("DwC.occurrence",
                      core = test_occ,
                      emof = data.frame(),
                      EML.url=as.character(NA),
                      QC=FALSE)
  
  expect_s4_class(test_occ_DwC, "DwC.occurrence")
  expect_message(show(test_occ_DwC))
  expect_message(show(test_occ_DwC2))
})

test_that("check.valid.metadata.DwC() works", {
  test_event <- data.frame(eventID=c("sample1", "sample2"),
                           eventDate=c("2021-09-27", "2021-09-28"),
                           decimalLatitude=c("54.7", "33"),
                           decimalLongitude=c("88.9", "-48.4"),
                           row.names=c("sample1", "sample2"))
  test_event_DwC <- new("DwC.event",
                        core = test_event,
                        occurrence = data.frame(),
                        emof = data.frame(),
                        EML.url=as.character(NA),
                        QC=TRUE)
  test_occ <- data.frame(occurrenceID=c("sample1", "sample2"),
                         basisOfRecord=c("humanObservation", "humanObservation"),
                         eventDate=c("2021-09-27", "2021-09-28"),
                         row.names=c("sample1", "sample2"))
  test_occ_DwC <- new("DwC.occurrence",
                        core = test_occ,
                        emof = data.frame(),
                        EML.url=as.character(NA),
                        QC=TRUE)
  
  expect_equal(check.valid.metadata.DwC(data.frame()), FALSE)
  expect_equal(check.valid.metadata.DwC(test_event_DwC), TRUE)
  expect_equal(check.valid.metadata.DwC(test_occ_DwC), TRUE)
})

test_that("write.MIxS() works", {
  sampleNames <- c("sample_1", "sample_2")
  test_MIxS <- new("MIxS.metadata",
                   data = data.frame(var1=c(1,2), var2=c(3,4), eventID=sampleNames, 
                                     row.names=sampleNames),
                   section = c(var1="section1", var2="section1", eventID="miscellaneous"),
                   units      = c(var1="unit1", var2="unit2", eventID="alphanumeric"),
                   env_package = "water",
                   type = "versatile",
                   QC = TRUE)
  temp_file <-  tempfile()
  
  expect_error(write.MIxS(x=NULL, file="test.csv"))
  expect_equal(write.MIxS(x=test_MIxS, file=temp_file), NULL)
  
  file.remove(temp_file)
  
})

