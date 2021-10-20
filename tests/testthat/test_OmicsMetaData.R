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

test_that("OmicsMetaData.help() works", {
  expect_message(OmicsMetaData.help())
  expect_error(OmicsMetaData.help("p"))
})

test_that("OmicsMetaData.help() works", {
  expect_message(term.definition("term"))
  expect_message(term.definition("conduc"))
  expect_error(term.definition("term", "term2"))
})




