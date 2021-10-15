#==============================================================
# Author Maxime Sweetlove
# lisence CC-0
# Part of the POLA3R website (successor or mARS.biodiversity.aq)
# version 1.0 (2023-09-23)
# file encdong UTF-8
#
#==============================================================
library(OmicsMetaData)
library(testthat)

#outputs are the correct format (including dimensions and components)
#sample input produces the correct sample output

# generate test data
sampleNames <- c("sample_1", "sample_2")

# a mock extended measurement or fact (eMoF) table
test_emof <- data.frame(eventID = c(rep(c("sample_1", "sample_2"), 2)),
                        measurementType = factor(c(rep("var1", 2), rep("var2", 2))), 
                        measurementValue = c(1:4),
                        measurementUnit = c(rep("unit1", 2), rep("unit2", 2)))
# a mock wide formated table
test_wideTab <- list(data = data.frame(var1=c(1,2), var2=c(3,4), row.names=sampleNames),
                     units = c(var1="unit1", var2="unit2"),
                     method=NA)

# a mock MIxS.metadata object
test_MIxS <- new("MIxS.metadata",
                 data   = data.frame(var1=c(1,2), var2=c(3,4), eventID=sampleNames, row.names=sampleNames),
                 section = c(var1="section1", var2="section1", eventID="miscellaneous"),
                 units      = c(var1="unit1", var2="unit2", eventID="alphanumeric"),
                 env_package = "water",
                 type = "versatile",
                 QC = TRUE)

# test the eMoF.to.wideTable function
test_that("eMoF.to.wideTable gives correct output", {
  expect_equal(eMoF.to.wideTable(test_emof), test_wideTab)
  expect_error(eMoF.to.wideTable(test_emof[2:4]))
})

#test the wideTable.to.eMoF function
test_that("wideTable.to.eMoF gives correct output", {
  expect_equal(wideTable.to.eMoF(metadata.object=test_MIxS, variables=c("var1", "var2")), test_emof)
  expect_error(wideTable.to.eMoF(metadata.object=test_wideTab$data, variables=NA))
})

# generate test data
df01 <- data.frame(var1=1:4, var2=2:5, row.names=paste("r", 1:4, sep=""))
df02 <- data.frame(var1=5:8, var3=1:4, row.names=paste("t", 1:4, sep=""))

df12 <- data.frame(var1=1:8, 
                   var2=c(2:5, rep(NA, 4)), 
                   var3=c(rep(NA, 4), 1:4),
                   row.names=c(paste("r", 1:4, sep=""), paste("t", 1:4, sep="")))

test_that("combine.data.frame gives correct output", {
  expect_equal(combine.data.frame(df01, df02, fill=NA, merge.cols=TRUE, 
                                  original_rowName.col=TRUE, merge.rows="df1"), df12)
})

# generate test data
sampleNames2 <- c("sample_3", "sample_4")

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


test_that("combine.data for MIxS.metadata objects gives correct output", {
  expect_equal(combine.data(d1=test_MIxS, d2=test_MIxS2, fill=NA, variables.as.cols=TRUE),
               test_MIxS3)
  expect_error(combine.data(d1=test_MIxS, d2=test_MIxS4, fill=NA, variables.as.cols=TRUE))
  expect_error(combine.data(d1=test_MIxS, d2=test_wideTab, fill=NA, variables.as.cols=TRUE))
})



test_event <- data.frame(var1=1:4, 
                         var2=6:9,
                         eventID=c(sampleNames, sampleNames2),
                         parentEventID=c("p1", "p1", "p2", "p2"),
                         row.names=c(sampleNames, sampleNames2))

