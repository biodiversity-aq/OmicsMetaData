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

test_event <- data.frame(eventID=c("sample1", "sample2"),
                            eventDate=c("2021-09-27", "2021-09-28"),
                            decimalLatitude=c("54.7", "33"),
                            decimalLongitude=c("88.9", "-48.4"),
                            row.names=c("sample1", "sample2"))


test_that("dataQC.DwC_general works for event data", {
  expect_s3_class(dataQC.DwC_general(dataset=test_event, DwC.type = "event", ask.input = FALSE,
                                     complete.data=TRUE), "data.frame")
  expect_s3_class(dataQC.DwC_general(dataset=test_event, DwC.type = "event", ask.input = FALSE,
                                     complete.data=FALSE), "data.frame")
  expect_s3_class(dataQC.DwC_general(dataset=test_event, DwC.type = "event", ask.input = TRUE,
                                     complete.data=FALSE), "data.frame")
  expect_s3_class(dataQC.DwC_general(dataset=test_event, DwC.type = "occurrence", ask.input = FALSE,
                                     complete.data=TRUE), "data.frame")
  expect_equal(dataQC.DwC_general(dataset=test_event, DwC.type = "event", ask.input = FALSE,
                                  complete.data=TRUE), test_event)
})

test_that("dataQC.DwC_general throws error", {
  expect_error(dataQC.DwC_general(dataset=NULL, DwC.type = "event", ask.input = FALSE,
                                  complete.data=FALSE))
  expect_error(dataQC.DwC_general(dataset=NULL, DwC.type = "event", ask.input = TRUE,
                                  complete.data=FALSE))
  expect_error(dataQC.DwC_general(dataset=NULL, DwC.type = "event", ask.input = FALSE,
                                  complete.data=TRUE))
  expect_error(dataQC.DwC_general(dataset=NULL, DwC.type = "event", ask.input = TRUE,
                                  complete.data=TRUE))
  expect_error(dataQC.DwC_general(dataset=NULL, DwC.type = "occurrence", ask.input = FALSE,
                                  complete.data=FALSE))
  expect_error(dataQC.DwC_general(dataset=NA, DwC.type = "event", ask.input = TRUE,
                                  complete.data=TRUE))
  expect_error(dataQC.DwC_general(dataset=data.frame, DwC.type = "event", ask.input = TRUE,
                                  complete.data=TRUE))
  expect_error(dataQC.DwC_general(dataset=test_event, DwC.type = NA, ask.input = FALSE,
                                  complete.data=TRUE))
  expect_error(dataQC.DwC_general(dataset=test_event, DwC.type = "random", ask.input = FALSE,
                                  complete.data=TRUE))
})

test_occur1 <- data.frame(occurrenceID=c("sp1", "sp2"),
                          eventDate=c("2021-09-27", NA),
                          decimalLatitude=c("54.7", "33"),
                          decimalLongitude=c("88.9", "-48.4"),
                          genus=c("Aulacoseira", "Rosa sp."),
                          row.names=c("sp1", "sp2"))

test_that("dataQC.DwC_general works for occurrence data", {
  expect_s3_class(dataQC.DwC_general(dataset=test_occur1, DwC.type = "occurrence", ask.input = FALSE,
                                     complete.data=TRUE), "data.frame")
  expect_s3_class(dataQC.DwC_general(dataset=test_occur1, DwC.type = "occurrence", ask.input = FALSE,
                                     complete.data=FALSE), "data.frame")
  expect_equal(dataQC.DwC_general(dataset=test_occur1, DwC.type = "occurrence", ask.input = FALSE,
                                  complete.data=TRUE), 
               data.frame(occurrenceID=c("sp1", "sp2"),
                          eventDate=c("2021-09-27", NA),
                          decimalLatitude=c("54.7", "33"),
                          decimalLongitude=c("88.9", "-48.4"),
                          genus=c("Aulacoseira", "Rosa"),
                          basisOfRecord=c(NA, NA),
                          occurrenceStatus=c(NA, NA),
                          scientificName=c("Aulacoseira", "Rosa"),
                          scientificNameID=c("urn:lsid:marinespecies.org:taxname:148959", "urn:lsid:marinespecies.org:taxname:425714"),
                          kingdom=c("Chromista", "Plantae"),
                          phylum=c("Ochrophyta", "Tracheophyta"),
                          class=c("Bacillariophyceae", "Magnoliopsida"),
                          order=c("Aulacoseirales", "Rosales"),
                          family=c("Aulacoseiraceae", "Rosaceae"),
                          specificEpithet=as.character(c(NA, NA)),
                          scientificNameAuthorship=c("G.H.K. Thwaites", "Linnaeus"),
                          namePublishedInYear=c("1848", "1753"),
                          row.names=c("sp1", "sp2")))
  expect_equal(dataQC.DwC_general(dataset=test_occur1, DwC.type = "occurrence", ask.input = FALSE,
                                  complete.data=FALSE), 
               data.frame(occurrenceID=c("sp1", "sp2"),
                          eventDate=c("2021-09-27", NA),
                          decimalLatitude=c("54.7", "33"),
                          decimalLongitude=c("88.9", "-48.4"),
                          genus=c("Aulacoseira", "Rosa sp."),
                          basisOfRecord=c(NA, NA),
                          occurrenceStatus=c(NA, NA),
                          row.names=c("sp1", "sp2")))
})


test_emof1 <- data.frame(eventID=c("sample1", "sample2"),
                         eventDate=c("2021-09-27", "2021-09-06"),
                         measurementValueID=c(4, 5),
                         measurementUnitID=c("m", "m"),
                         measurementTypeID=c("MIxS:depth", "MIxS:depth"),
                         row.names=c("sample1", "sample2"))

test_that("dataQC.DwC_general works for emof data", {
  expect_s3_class(dataQC.DwC_general(dataset=test_emof1, DwC.type = "emof", ask.input = FALSE,
                                  complete.data=FALSE), "data.frame")
  expect_s3_class(dataQC.DwC_general(dataset=test_emof1, DwC.type = "emof", ask.input = FALSE,
                                     complete.data=TRUE), "data.frame")
  expect_equal(dataQC.DwC_general(dataset=test_emof1, DwC.type = "emof", ask.input = FALSE,
                                  complete.data=FALSE), test_emof1)
  expect_equal(dataQC.DwC_general(dataset=test_emof1, DwC.type = "EMOF", ask.input = FALSE,
                                  complete.data=FALSE), test_emof1)
  expect_equal(dataQC.DwC_general(dataset=test_emof1, DwC.type = "emof", ask.input = FALSE,
                                     complete.data=NA), test_emof1)
  expect_error(dataQC.DwC_general(dataset=data.frame(), DwC.type = "emof", ask.input = FALSE,
                                  complete.data=NA))
})


test_emof2 <- data.frame(eventID=c("sample1", "sample2"),
                         date=c("2021-09-27", "2021-09-06"),
                         row.names=c("sample1", "sample2"))

test_that("dataQC.DwC_general works for emof data", {
  expect_error(dataQC.DwC_general(dataset=test_emof2, DwC.type = "emof", ask.input = FALSE,
                                  complete.data=NA))
  expect_error(dataQC.DwC_general(dataset=test_emof2, DwC.type = "event", ask.input = FALSE,
                                  complete.data=NA))
  expect_error(dataQC.DwC_general(dataset=test_emof2, DwC.type = "occurrence", ask.input = FALSE,
                                  complete.data=NA))
  expect_error(dataQC.DwC_general(dataset=test_emof2, DwC.type = "unkown", ask.input = FALSE,
                                  complete.data=NA))
})

dataQC.DwC_general(dataset=test_emof2, DwC.type = "emof", ask.input = TRUE,
                   complete.data=NA)


dataQC.DwC_general(dataset=test_emof2, DwC.type = "EMOF", ask.input = FALSE,
                   complete.data=FALSE)

test_that("dataQC.DwC_general works for emof data", {
  expect_equal(dataQC.DwC_general(dataset=test_emof2, DwC.type = "emof", ask.input = FALSE,
                                  complete.data=FALSE), test_emof1)

})

test_that("dataQC.DwC works for event data", {
  expect_s4_class(dataQC.DwC(Event=test_event, ask.input = FALSE, out.type="event"), 
                  "DwC.event")
  expect_equal(dataQC.DwC(Event=test_event, ask.input = FALSE, out.type="event"),
               new("DwC.event",
                   core = test_event,
                   occurrence = data.frame(),
                   emof = data.frame(),
                   EML.url=as.character(NA),
                   QC=TRUE))
})

test_that("dataQC.DwC works for occurrence data", {
  expect_s4_class(dataQC.DwC(Occurrence=test_occur1, ask.input = FALSE, out.type="occurrence"), 
                  "DwC.occurrence")
  expect_error(dataQC.DwC(Occurrence=test_occur1, ask.input = FALSE, out.type="event"))
  expect_equal(dataQC.DwC(Occurrence=test_occur1, ask.input = FALSE, out.type="occurrence"),
               new("DwC.occurrence",
                   core = data.frame(occurrenceID=c("sp1", "sp2"),
                                     eventDate=c("2021-09-27", NA),
                                     decimalLatitude=c("54.7", "33"),
                                     decimalLongitude=c("88.9", "-48.4"),
                                     genus=c("Aulacoseira", "Rosa"),
                                     basisOfRecord=c(NA, NA),
                                     occurrenceStatus=c(NA, NA),
                                     scientificName=c("Aulacoseira", "Rosa"),
                                     scientificNameID=c("urn:lsid:marinespecies.org:taxname:148959", "urn:lsid:marinespecies.org:taxname:425714"),
                                     kingdom=c("Chromista", "Plantae"),
                                     phylum=c("Ochrophyta", "Tracheophyta"),
                                     class=c("Bacillariophyceae", "Magnoliopsida"),
                                     order=c("Aulacoseirales", "Rosales"),
                                     family=c("Aulacoseiraceae", "Rosaceae"),
                                     specificEpithet=as.character(c(NA, NA)),
                                     scientificNameAuthorship=c("G.H.K. Thwaites", "Linnaeus"),
                                     namePublishedInYear=c("1848", "1753"),
                                     row.names=c("sp1", "sp2")),
                   emof = data.frame(),
                   EML.url=as.character(NA),
                   QC=TRUE))
})


test_that("dataQC.DwC works for event data with eMoF", {
  expect_equal(dataQC.DwC(Event=test_event, eMoF=test_emof1, ask.input = FALSE, out.type="event"),
               new("DwC.event",
                   core = test_event,
                   occurrence = data.frame(),
                   emof = test_emof1,
                   EML.url=as.character(NA),
                   QC=TRUE))
})

