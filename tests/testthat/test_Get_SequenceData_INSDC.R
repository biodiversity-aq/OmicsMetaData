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

test_that("get.BioProject.metadata.INSDC works", {
  expect_s3_class(get.BioProject.metadata.INSDC(BioPrjct="PRJNA303951", just.names=FALSE),
                  "data.frame")
  expect_equal(class(get.BioProject.metadata.INSDC(BioPrjct="PRJNA303951", just.names=TRUE)),
               "character")
  #expect_error(get.BioProject.metadata.INSDC(BioPrjct="nonsens_DezeNaamBestaatNiet_mooiweervandaag", just.names=FALSE))
})


# get.sample.attributes.INSDC can't be fully tested due to needing a personal API key
test_that("get.BioProject.metadata.INSDC works", {
  expect_error(get.sample.attributes.INSDC(sampleID="SRR3087847", apiKey=NA))
  expect_error(get.sample.attributes.INSDC(BioPrjct="PRJNA303951", apiKey=NA))
  
  expect_s3_class(get.sample.attributes.INSDC(sampleID="SRR3087847", apiKey="test"),
                  "data.frame")
  closeAllConnections()
  expect_s3_class(get.sample.attributes.INSDC(BioPrjct="PRJNA303951", apiKey="test"),
                  "data.frame")
  closeAllConnections()
})


test_that("download.sequences.INSDC works 1", {
  expect_message(download.sequences.INSDC(BioPrj = "PRJNA303951", destination.path = NA, 
                                          apiKey=NA, unzip = FALSE, keep.metadata = FALSE, 
                                          download.sequences = FALSE))
  expect_error(download.sequences.INSDC(BioPrj = "nonsens_DezeNaamBestaatNiet_mooiweervandaag", destination.path = NA, 
                                          apiKey=NA, unzip = FALSE, keep.metadata = TRUE, 
                                          download.sequences = FALSE))
  expect_error(download.sequences.INSDC(BioPrj = "nonsens_DezeNaamBestaatNiet_mooiweervandaag", destination.path = NA, 
                                        apiKey=NA, unzip = TRUE, keep.metadata = TRUE, 
                                        download.sequences = FALSE))
  closeAllConnections()
})

test_that("download.sequences.INSDC works 2", {
  expect_s3_class(download.sequences.INSDC(BioPrj = "PRJNA303951", destination.path = NA, 
                                          apiKey="test", unzip = FALSE, keep.metadata = TRUE, 
                                          download.sequences = FALSE), 
                  "data.frame")
  closeAllConnections()
  
})


# clean up
closeAllConnections()



