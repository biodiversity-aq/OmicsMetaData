#==============================================================
# Author Maxime Sweetlove
# Part of the POLA3R website (successor or mARS.biodiversity.aq)
# version 1.0 (2023-09-23)
# file encding UTF-8
#
#==============================================================
library(OmicsMetaData)
library(testthat)

test_that("combine.data.frame gives correct output", {
  # generate test data
  df01 <- data.frame(var1=1:4, var2=2:5, row.names=paste("r", 1:4, sep=""))
  df02 <- data.frame(var1=5:8, var3=1:4, row.names=paste("t", 1:4, sep=""))
  df03 <- data.frame(var1=5:8, var3=1:4, row.names=c(paste("r", 1:3, sep=""), "q1"))
  df12 <- data.frame(var1=1:8, 
                     var2=c(2:5, rep(NA, 4)), 
                     var3=c(rep(NA, 4), 1:4),
                     row.names=c(paste("r", 1:4, sep=""), paste("t", 1:4, sep="")))
  
  
  expect_equal(combine.data.frame(df01, df02, fill=NA, merge.cols=TRUE, 
                                  original_rowName.col=TRUE, merge.rows="df1"), df12)
  expect_s3_class(combine.data.frame(df01, df02, fill="-", merge.cols=FALSE, 
                                  original_rowName.col=TRUE, merge.rows="df1"), "data.frame")
  expect_s3_class(combine.data.frame(df01, df03, fill="-", merge.cols=TRUE, 
                                     original_rowName.col=TRUE, merge.rows=NA), "data.frame")
  
  })

# test the eMoF.to.wideTable function
test_that("eMoF.to.wideTable gives correct output", {
  sampleNames <- c("sample_1", "sample_2")
  test_emof_1 <- data.frame(eventID = c(rep(c("sample_1", "sample_2"), 2)),
                          measurementType = factor(c(rep("var1", 2), rep("var2", 2))), 
                          measurementValue = c(1:4),
                          measurementUnit = c(rep("unit1", 2), rep("unit2", 2)))
  test_emof_2 <- data.frame(occurrenceID = c(rep(c("sp1", "sp2"), 2)),
                            measurementType = factor(c(rep("var1", 2), rep("var2", 2))), 
                            measurementValue = c(1:4),
                            measurementMethod =c(rep("visual observation"), 4),
                            measurementUnit = c(rep("unit1", 2), rep("unit2", 2)))
  
  # a mock wide formated table
  test_wideTab_1 <- list(data = data.frame(var1=c(1,2), var2=c(3,4), row.names=sampleNames),
                       units = c(var1="unit1", var2="unit2"),
                       method=NA)
  
  
  expect_equal(eMoF.to.wideTable(test_emof_1), test_wideTab_1)
  expect_type(eMoF.to.wideTable(test_emof_2), "list")
  expect_error(eMoF.to.wideTable(test_emof_1[2:4]))
  expect_error(eMoF.to.wideTable(test_emof_2[1:3]))
})


#test the wideTable.to.eMoF function
test_that("wideTable.to.eMoF gives correct output", {
  sampleNames <- c("sample_1", "sample_2")
  test_emof_1 <- data.frame(eventID = c(rep(c("sample_1", "sample_2"), 2)),
                            measurementType = factor(c(rep("var1", 2), rep("var2", 2))), 
                            measurementValue = c(1:4),
                            measurementUnit = c(rep("unit1", 2), rep("unit2", 2)))
  test_wideTab_1 <- list(data = data.frame(var1=c(1,2), var2=c(3,4), row.names=sampleNames),
                         units = c(var1="unit1", var2="unit2"),
                         method=NA)
  test_MIxS_1 <- new("MIxS.metadata",
                     data   = data.frame(var1=c(1,2), var2=c(3,4), eventID=sampleNames, row.names=sampleNames),
                     section = c(var1="section1", var2="section1", eventID="miscellaneous"),
                     units      = c(var1="unit1", var2="unit2", eventID="alphanumeric"),
                     env_package = "water",
                     type = "versatile",
                     QC = TRUE)
  test_MIxS_2 <- new("MIxS.metadata",
                     data   = data.frame(var1=c(1,2), var2=c(3,4), occurrenceID=sampleNames, row.names=sampleNames),
                     section = c(var1="section1", var2="section1", eventID="miscellaneous"),
                     units      = c(var1="unit1", var2="unit2", eventID="alphanumeric"),
                     env_package = "water",
                     type = "versatile",
                     QC = TRUE)
  
  expect_equal(wideTable.to.eMoF(metadata.object=test_MIxS_1, variables=c("var1", "var2")), 
               test_emof_1)
  expect_s3_class(wideTable.to.eMoF(metadata.object=test_MIxS_2, variables=c("var1", "var2")), 
               "data.frame")
  
  expect_error(wideTable.to.eMoF(metadata.object=test_wideTab_1$data, variables=NA))
  expect_error(wideTable.to.eMoF(metadata.object=test_MIxS_1, variables="unkown"))
})


test_that("combine.data for MIxS.metadata objects gives correct output", {
  sampleNames <- c("sample_1", "sample_2")
  sampleNames2 <- c("sample_3", "sample_4")
  test_MIxS1 <- new("MIxS.metadata",
                     data   = data.frame(var1=c(1,2), var2=c(3,4), eventID=sampleNames, row.names=sampleNames),
                     section = c(var1="section1", var2="section1", eventID="miscellaneous"),
                     units      = c(var1="unit1", var2="unit2", eventID="alphanumeric"),
                     env_package = "water",
                     type = "versatile",
                     QC = TRUE)
  test_MIxS2 <- new("MIxS.metadata",
                    data   = data.frame(var1=c(1,2), var3=c(3,4), eventID=sampleNames2, row.names=sampleNames2),
                    section = c(var1="section1", var3="section1", eventID="miscellaneous"),
                    units      = c(var1="unit1", var3="unit1", eventID="alphanumeric"),
                    env_package = "water",
                    type = "versatile",
                    QC = TRUE)
  test_MIxS3 <- new("MIxS.metadata",
                    data   = data.frame(var1=c(1,2,1,2), 
                                        var2=c(3,4,NA,NA),
                                        eventID=c(sampleNames, sampleNames2),
                                        var3=c(NA,NA,3,4),
                                        row.names=c(sampleNames, sampleNames2)),
                    section = c(var1="section1", 
                                var2="section1",
                                eventID="miscellaneous",
                                var3="section1"),
                    units      = c(var1="unit1", 
                                   var2="unit2", 
                                   eventID="alphanumeric",
                                   var3="unit1"),
                    env_package = "water",
                    type = "versatile",
                    QC = TRUE)
  test_MIxS4 <- new("MIxS.metadata",
                    data   = data.frame(var1=c(1,2), var3=c(3,4), eventID=sampleNames2, row.names=sampleNames2),
                    section = c(var1="section1", var3="section1", eventID="miscellaneous"),
                    units      = c(var1="unit2", var3="unit1", eventID="alphanumeric"),
                    env_package = "water",
                    type = "versatile",
                    QC = TRUE)
  
  expect_equal(combine.data(d1=test_MIxS1, d2=test_MIxS2, fill=NA, variables.as.cols=TRUE),
               test_MIxS3)
  expect_s4_class(combine.data(d1=test_MIxS1, d2=test_MIxS2, fill="-", 
                               variables.as.cols=FALSE),
               "MIxS.metadata")
  expect_s3_class(combine.data(d1=test_MIxS1@data, d2=test_MIxS2@data, fill="-", 
                               variables.as.cols=FALSE),
                  "data.frame")
  expect_s3_class(combine.data(d1=test_MIxS1@data, d2=test_MIxS2@data, fill=NA, 
                               variables.as.cols=TRUE),
                  "data.frame")
  expect_error(combine.data(d1=test_MIxS1, d2=test_MIxS4, fill=NA, variables.as.cols=TRUE))
  expect_error(combine.data(d1=test_MIxS1, d2=test_wideTab, fill=NA, variables.as.cols=TRUE))
})


