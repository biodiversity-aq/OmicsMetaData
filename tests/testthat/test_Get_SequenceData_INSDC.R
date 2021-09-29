#==============================================================
# Author Maxime Sweetlove
# lisence CC-0
# Part of the POLA3R website (successor or mARS.biodiversity.aq)
# version 1.0 (2021-09-23)
# file encdong UTF-8
#
#==============================================================
library(OmicsMetaData)
library(testthat)

test_that("get.BioProject.metadata.INSDC works", {
  expect_s3_class(get.BioProject.metadata.INSDC(BioPrjct="PRJNA303951", just.names=FALSE), "data.frame")
  expect_error(get.BioProject.metadata.INSDC(BioPrjct="nonsens_DezeNaamBestaatNiet_mooiweervandaag", just.names=FALSE))
})


# get.sample.attributes.INSDC can't be fully tested due to needing a personal API key
test_that("download.sequences.INSDC works", {
  expect_message(download.sequences.INSDC(BioPrj = "PRJNA303951", destination.path = NA, apiKey=NA,
                                          unzip = FALSE, keep.metadata = FALSE, download.sequences = FALSE),
                 c("Getting the metadata ...", "Processing BioProject PRJNA303951...", 
                   "50 samples (Runs)...", "Finished processing", "No files were downloaded"))
})


