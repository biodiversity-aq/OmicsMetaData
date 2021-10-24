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

test_that("dataQC.MIxS works without user input", {
  test_metadata <- data.frame(sample_name=c("sample1", "sample2"),
                              collection_date=c("2021-09-27", "2021-09-28"),
                              lat_lon=c("54.7 88.9", "33 -48.4"),
                              row.names=c("sample1", "sample2"))
  
  test_metadata_data <- data.frame(original_name=c("sample1", "sample2"),
                                   lat_lon=c("54.7 88.9", "33 -48.4"),
                                   decimalLatitude=c("54.7", "33"),
                                   decimalLongitude=c("88.9", "-48.4"),
                                   collection_date=c("2021-09-27", "2021-09-28"),
                                   sample_name=c("sample1", "sample2"),
                                   row.names=c("sample1", "sample2"))
  test_metadata_section <- c(original_name="miscellaneous", lat_lon="environment",
                             decimalLatitude = "miscellaneous", decimalLongitude= "miscellaneous",
                             collection_date ="environment", sample_name="miscellaneous")
  test_metadata_units <- c(original_name="alphanumeric", lat_lon="decimalCoordinates",
                           decimalLatitude = "decimalCoordinates", decimalLongitude= "decimalCoordinates",
                           collection_date ="UTC", sample_name="alphanumeric")
  
  test_metadataMIxS <- new("MIxS.metadata",
                           data   = test_metadata_data,
                           section = test_metadata_section,
                           units      = test_metadata_units,
                           env_package = "not_specified",
                           type = "versatile",
                           QC = TRUE)
  
  expect_s4_class(dataQC.MIxS(test_metadata, ask.input=FALSE), "MIxS.metadata")
  expect_equal(dataQC.MIxS(test_metadata, ask.input=FALSE), test_metadataMIxS)
  expect_error(dataQC.MIxS(test_metadata, ask.input=FALSE, sample.names = "eventID"))
})

test_that("dataQC.MIxS works with user input", {
  test_metadata <- data.frame(original_name=c("sample1", "sample2"),
                              collection_date=c("2021-09-27", "2021-09-28"),
                              lat_lon=c("54.7 88.9", "33 -48.4"),
                              row.names=c("sample1", "sample2"))
  test_metadata_2 <- data.frame(original_name=c("unit","sample1", "sample2"),
                              collection_date=c("UTC","2021-09-27", "2021-09-28"),
                              lat_lon=c("decimal degrees","54.7 88.9", "33 -48.4"),
                              temp=c("degrees f", NA, NA),
                              ph=c("pH", 4, 7),
                              depth=c("cm", 4000, 6000),
                              librarylayout=c("alphanumeric", "PAIRED", "PAIRED"),
                              row.names=c("unit","sample1", "sample2"))
  test_metadata_3 <- data.frame(original_name=c("sample1", "sample2"),
                              collection_date=c("2021-09-27", "2021-09-28"),
                              decimalLatitude=c("54.7", "33"),
                              decimalLongitude=c("88.9", "-48.4"),
                              env_package=c("water", "soil"),
                              target_gene=c("16S", "ITS-1"),
                              extravar1=c("tt", "pp"),
                              depth=c(3, 200),
                              row.names=c("sample1", "sample2"))
  
  #turn of user connection
  f<-file()
  options(mypkg.connection = f)
  ans <- c("3", #test 1
           "y", "2", "soil", "n", #test 2
           "y", "1", "mimarks-survey", #test 3
           "n", "1", "n", "n") #test 3
  write(ans, f)
  
  expect_error(dataQC.MIxS(dataset=test_metadata, ask.input=TRUE))
  expect_warning(dataQC.MIxS(dataset=test_metadata, ask.input=TRUE))
  expect_warning(dataQC.MIxS(dataset=test_metadata_2, ask.input=TRUE))
  expect_warning(dataQC.MIxS(dataset=test_metadata_3, ask.input=TRUE))
  
  #reset connection
  options(mypkg.connection = stdin())
  close(f)
  closeAllConnections()
})



