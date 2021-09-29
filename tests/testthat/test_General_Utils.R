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

test_that("find.dataset works", {
  expect_equal(find.dataset(data.frame(var1=1:3, var2=1:3), "var3"),
               "")
  expect_equal(find.dataset(data.frame(var1=1:3, var2=1:3), "var2"),
               1:3)
})

test_that("coordinate.to.decimal works", {
  expect_equal(coordinate.to.decimal("33N"),
               33)
  expect_equal(coordinate.to.decimal("33S"),
               -33)
  expect_equal(coordinate.to.decimal("33°"),
               33)
})

test_that("coordinate.to.decimal works", {
  expect_equal(coordinate.to.decimal("33N"),
               33)
  expect_equal(coordinate.to.decimal("33S"),
               -33)
  expect_equal(coordinate.to.decimal("33°"),
               33)
})


test_that("coordinate.to.decimal works", {
  expect_equal(get.boundingBox(c(44, 55), c(66, 77)),
               NULL)
  expect_message(get.boundingBox(c(44, 55), c(66, 77)), 
                 paste("South =  44", "\n", "North =  55", "\n\n", "West =  66", "\n", "East =  77", "\n", sep = "")
  )
})





