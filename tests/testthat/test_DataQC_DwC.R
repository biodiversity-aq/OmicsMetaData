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

test_that("dataQC.DwC_general works for event data, no user input", {
  test_event <- data.frame(eventID=c("sample1", "sample2"),
                           eventDate=c("2021-09-27", "2021-09-28"),
                           decimalLatitude=c("54.7", "33"),
                           decimalLongitude=c("88.9", "-48.4"),
                           row.names=c("sample1", "sample2"))
  
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

test_that("dataQC.DwC_general works for event data, with user input", {
  test_event_1 <- data.frame(eventid=c("sample1", "sample2"),
                           eventDate=c("2021-09-27", "2021-09-28"),
                           decimalLatitude=c("54.7", "33"),
                           decimalLongitude=c("88.9", "-48.4"),
                           row.names=c("sample1", "sample2"))
  test_event_2 <- data.frame(staalnaam=c("sample1", "sample2"),
                           datum=c("2021-09-27", "2021-09-28"),
                           latitude=c("54.7", "33"),
                           longitude=c("88.9", "-48.4"),
                           row.names=c("sample1", "sample2"))
  test_event_3 <- data.frame(eventid=c("sample1", "sample2"),
                             eventids=c("sample1", "sample2"),
                             eventdate=c("2021-09-27", "2021-09-28"),
                             extravar1=c("88.9", "-48.4"),
                             extravar2=c("sample1", "sample2"))
  test_event_1OUT <- data.frame(eventID=c("sample1", "sample2"),
                              eventDate=c("2021-09-27", "2021-09-28"),
                              decimalLatitude=c("54.7", "33"),
                              decimalLongitude=c("88.9", "-48.4"),
                              row.names=c("sample1", "sample2"))
  test_event_2OUT <- data.frame(staalnaam=c("sample1", "sample2"),
                                datum=c("2021-09-27", "2021-09-28"),
                                decimalLatitude=c("54.7", "33"),
                                decimalLongitude=c("88.9", "-48.4"),
                                dynamicProperties=c("{\"staalnaam\":sample1}", "{\"staalnaam\":sample2}"),
                                eventRemark = c("datum:2021-09-27", "datum:2021-09-28"),
                                eventID = c("perfix_1", "perfix_2"),
                                row.names=c("sample1", "sample2"))
  test_event_3OUT <- data.frame(eventID=c("sample1", "sample2"),
                                eventids=c("sample1", "sample2"),
                                eventDate=c("2021-09-27", "2021-09-28"),
                                extravar2=c("sample1", "sample2"),
                                fieldNotes=c("extravar2:sample1", "extravar2:sample2"))
  #turn of user connection
  f<-file()
  options(mypkg.connection = f)
  ans <- c("y", #test1
          "y", "y", "2", "3", "perfix",#test2
          "y", "n", "y", "1", "4") #test3
  write(ans, f)
  

  expect_equal(dataQC.DwC_general(dataset=test_event_1, DwC.type = "event", ask.input = TRUE,
                                  complete.data=TRUE), test_event_1OUT)
  expect_equal(dataQC.DwC_general(dataset=test_event_2, DwC.type = "event", ask.input = TRUE,
                                  complete.data=TRUE), test_event_2OUT)
  expect_equal(dataQC.DwC_general(dataset=test_event_3, DwC.type = "event", ask.input = TRUE,
                                  complete.data=TRUE), test_event_3OUT)
  #reset connection
  options(mypkg.connection = stdin())
  close(f)
})

test_that("dataQC.DwC_general throws error", {
  test_event <- data.frame(eventID=c("sample1", "sample2"),
                           eventDate=c("2021-09-27", "2021-09-28"),
                           decimalLatitude=c("54.7", "33"),
                           decimalLongitude=c("88.9", "-48.4"),
                           row.names=c("sample1", "sample2"))
  
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

test_that("dataQC.DwC_general throws an error on bad input", {
  test_event_1 <- data.frame(eventid=c("sample1", "sample2"),
                             eventDate=c("2021-09-27", "2021-09-28"),
                             extravar1=c("54.7", "33"),
                             etravar2=c("88.9", "-48.4"),
                             row.names=c("sample1", "sample2"))
  #turn of user connection
  f<-file()
  options(mypkg.connection = f)
  ans <- c("r", #test1
           "y", "y") #test2
  write(ans, f)
  
  
  expect_error(dataQC.DwC_general(dataset=test_event_1, DwC.type = "event", ask.input = TRUE,
                                  complete.data=FALSE))
  expect_error(dataQC.DwC_general(dataset=test_event_1, DwC.type = "event", ask.input = TRUE,
                                  complete.data=FALSE))
  #reset connection
  options(mypkg.connection = stdin())
  close(f)
})

test_that("dataQC.DwC_general works for occurrence data, no user input", {
  test_occur <- data.frame(occurrenceID=c("sp1", "sp2"),
                            eventDate=c("2021-09-27", NA),
                            decimalLatitude=c("54.7", "33"),
                            decimalLongitude=c("88.9", "-48.4"),
                            genus=c("Aulacoseira", "Rosa sp."),
                            row.names=c("sp1", "sp2"))
  test_occur_OUT1 <- data.frame(occurrenceID=c("sp1", "sp2"), 
                                 eventDate=c("2021-09-27", NA),
                                 decimalLatitude=c("54.7", "33"),
                                 decimalLongitude=c("88.9", "-48.4"),
                                 genus=c("Aulacoseira", "Rosa"),
                                 basisOfRecord=c(NA, NA), occurrenceStatus=c(NA, NA),
                                 scientificName=c("Aulacoseira", "Rosa"),
                                 scientificNameID=c("urn:lsid:marinespecies.org:taxname:148959", "urn:lsid:marinespecies.org:taxname:425714"),
                                 kingdom=c("Chromista", "Plantae"), phylum=c("Ochrophyta", "Tracheophyta"),
                                 class=c("Bacillariophyceae", "Magnoliopsida"), order=c("Aulacoseirales", "Rosales"),
                                 family=c("Aulacoseiraceae", "Rosaceae"),
                                 specificEpithet=as.character(c(NA, NA)),
                                 scientificNameAuthorship=c("G.H.K. Thwaites", "Linnaeus"),
                                 namePublishedInYear=c("1848", "1753"),
                                 row.names=c("sp1", "sp2"))
  test_occur_OUT2 <- data.frame(occurrenceID=c("sp1", "sp2"),
                                 eventDate=c("2021-09-27", NA),
                                 decimalLatitude=c("54.7", "33"),
                                 decimalLongitude=c("88.9", "-48.4"),
                                 genus=c("Aulacoseira", "Rosa sp."),
                                 basisOfRecord=c(NA, NA), occurrenceStatus=c(NA, NA),
                                 row.names=c("sp1", "sp2"))
  
  
  expect_s3_class(dataQC.DwC_general(dataset=test_occur, DwC.type = "occurrence", ask.input = FALSE,
                                     complete.data=TRUE), "data.frame")
  expect_s3_class(dataQC.DwC_general(dataset=test_occur, DwC.type = "occurrence", ask.input = FALSE,
                                     complete.data=FALSE), "data.frame")
  expect_equal(dataQC.DwC_general(dataset=test_occur, DwC.type = "occurrence", ask.input = FALSE,
                                  complete.data=TRUE), test_occur_OUT1)
  expect_equal(dataQC.DwC_general(dataset=test_occur, DwC.type = "occurrence", ask.input = FALSE,
                                  complete.data=FALSE), test_occur_OUT2)
})

test_that("dataQC.DwC_general works for occurrence data, with user input", {
  test_occur_1 <- data.frame(occurrenceID=c("sp1", "sp2"),
                           eventDate=c("2021-09-27", NA),
                           decimalLatitude=c("54.7", "33"),
                           decimalLongitude=c("88.9", "-48.4"),
                           genus=c("Aulacoseira", "Rosa sp."),
                           row.names=c("sp1", "sp2"))
  test_occur_2 <- data.frame(occurrenceID=c("sp1", "sp2"),
                             extravar1=c("a", "b"),
                             extravar2=c("1", "2"),
                             extravar3=c("aa", "bb"),
                             extravar4=c("cc", "dd"),
                             extravar5=c("ccf", "ddf"),
                             row.names=c("sp1", "sp2"))
  test_occur_3 <- data.frame(occurrenceid=c("sp1", "sp2"),
                             year=c("1996", "2021"),
                             month=c("12", "04"),
                             day=c("1", "3"),
                             genus=c("Aulacoseira", "Rosa"),
                             row.names=c("sp1", "sp2"))
  test_occur_1OUT <- data.frame(occurrenceID=c("sp1", "sp2"), 
                                eventDate=c("2021-09-27", NA),
                                decimalLatitude=c("54.7", "33"),
                                decimalLongitude=c("88.9", "-48.4"),
                                genus=c("Aulacoseira", "Rosa"),
                                basisOfRecord=c("HumanObservation", "HumanObservation"), 
                                occurrenceStatus=c(NA, NA),
                                scientificName=c("Aulacoseira", "Rosa"),
                                scientificNameID=c("urn:lsid:marinespecies.org:taxname:148959", "urn:lsid:marinespecies.org:taxname:425714"),
                                kingdom=c("Chromista", "Plantae"), phylum=c("Ochrophyta", "Tracheophyta"),
                                class=c("Bacillariophyceae", "Magnoliopsida"), order=c("Aulacoseirales", "Rosales"),
                                family=c("Aulacoseiraceae", "Rosaceae"),
                                specificEpithet=as.character(c(NA, NA)),
                                scientificNameAuthorship=c("G.H.K. Thwaites", "Linnaeus"),
                                namePublishedInYear=c("1848", "1753"),
                                row.names=c("sp1", "sp2"))
  test_occur_OUT2 <- data.frame(occurrenceID=c("sp1", "sp2"),
                             extravar1=c("a", "b"),
                             extravar2=c("1", "2"),
                             extravar3=c("aa", "bb"),
                             extravar4=c("cc", "dd"),
                             extravar5=c("ccf", "ddf"),
                             identificationRemarks=c("extravar1:a", "extravar1:b"),
                             taxonRemarks=c("extravar2:1", "extravar2:2"),
                             measurementRemarks=c("extravar3:aa", "extravar3:bb"),
                             occurrenceRemarks=c("extravar4:cc, extravar5:ccf",
                                                 "extravar4:dd, extravar5:ddf"),
                             basisOfRecord=c("MachineObservation", "MachineObservation"),
                             occurrenceStatus =c(NA, NA),
                             row.names=c("sp1", "sp2"))
  test_occur_3OUT <- data.frame(occurrenceid=c("sp1", "sp2"),
                                year=c("1996", "2021"),
                                month=c("12", "04"),
                                day=c("1", "3"),
                                genus=c("Aulacoseira", "Rosa"),
                                occurrenceID=c("occid_1", "occid_2"),
                                basisOfRecord=c("LivingSpecimen", "LivingSpecimen"),
                                occurrenceStatus=c(NA, NA),
                                eventDate=c("1996-12-01", "2021-04-03"),
                                row.names=c("sp1", "sp2"))
  #turn of user connection
  f<-file()
  options(mypkg.connection = f)
  ans <- c("1", #test1
           "5", "6", "7", "8", "8", "2", #test2
           "n", "3", "occid") #test3
  write(ans, f)

  expect_equal(dataQC.DwC_general(dataset=test_occur_1, DwC.type = "occurrence", 
                                  ask.input = TRUE, complete.data=TRUE), 
               test_occur_1OUT)
  expect_equal(dataQC.DwC_general(dataset=test_occur_2, DwC.type = "occurrence", 
                                  ask.input = TRUE, complete.data=FALSE), 
               test_occur_OUT2)
  expect_equal(dataQC.DwC_general(dataset=test_occur_3, DwC.type = "occurrence", 
                                  ask.input = TRUE, complete.data=FALSE), 
               test_occur_3OUT)
  
  
  #reset connection
  options(mypkg.connection = stdin())
  close(f)
})

test_that("dataQC.DwC_general works for emof data", {
  test_emof <- data.frame(eventID=c("sample1", "sample2"),
                           eventDate=c("2021-09-27", "2021-09-06"),
                           measurementValueID=c(4, 5),
                           measurementUnitID=c("m", "m"),
                           measurementTypeID=c("MIxS:depth", "MIxS:depth"),
                           row.names=c("sample1", "sample2"))
  
  expect_s3_class(dataQC.DwC_general(dataset=test_emof, DwC.type = "emof", ask.input = FALSE,
                                  complete.data=FALSE), "data.frame")
  expect_s3_class(dataQC.DwC_general(dataset=test_emof, DwC.type = "emof", ask.input = FALSE,
                                     complete.data=TRUE), "data.frame")
  expect_equal(dataQC.DwC_general(dataset=test_emof, DwC.type = "emof", ask.input = FALSE,
                                  complete.data=FALSE), test_emof)
  expect_equal(dataQC.DwC_general(dataset=test_emof, DwC.type = "EMOF", ask.input = FALSE,
                                  complete.data=FALSE), test_emof)
  expect_equal(dataQC.DwC_general(dataset=test_emof, DwC.type = "emof", ask.input = FALSE,
                                     complete.data=NA), test_emof)
  expect_error(dataQC.DwC_general(dataset=data.frame(), DwC.type = "emof", ask.input = FALSE,
                                  complete.data=NA))
})

test_that("dataQC.DwC_general fails on bad data", {
  test_emof <- data.frame(eventID=c("sample1", "sample2"),
                           date=c("2021-09-27", "2021-09-06"),
                           row.names=c("sample1", "sample2"))
  
  expect_error(dataQC.DwC_general(dataset=test_emof, DwC.type = "emof", ask.input = FALSE,
                                  complete.data=NA))
  expect_error(dataQC.DwC_general(dataset=test_emof, DwC.type = "occurrence", ask.input = FALSE,
                                  complete.data=NA))
  expect_error(dataQC.DwC_general(dataset=test_emof, DwC.type = "unkown", ask.input = FALSE,
                                  complete.data=NA))
})


#dataQC.DwC_general(dataset=test_emof2, DwC.type = "event", ask.input = TRUE,
#                   complete.data=NA)


#dataQC.DwC_general(dataset=test_emof2, DwC.type = "EMOF", ask.input = FALSE,
#                   complete.data=FALSE)

test_that("dataQC.DwC_general works for emof data", {
  test_emof <- data.frame(eventID=c("sample1", "sample2"),
                          eventDate=c("2021-09-27", "2021-09-06"),
                          measurementValueID=c(4, 5),
                          measurementUnitID=c("m", "m"),
                          measurementTypeID=c("MIxS:depth", "MIxS:depth"),
                          row.names=c("sample1", "sample2"))
  
  expect_equal(dataQC.DwC_general(dataset=test_emof, DwC.type = "emof", ask.input = FALSE,
                                  complete.data=FALSE), test_emof)

})

test_that("dataQC.DwC works for event data", {
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
  expect_s4_class(dataQC.DwC(Event=test_event, ask.input = FALSE, out.type="event"), 
                  "DwC.event")
  expect_equal(dataQC.DwC(Event=test_event, ask.input = FALSE, out.type="event"),
               test_event_DwC)
})

test_that("dataQC.DwC works for occurrence data", {
  test_occur <- data.frame(occurrenceID=c("sp1", "sp2"),
                           eventDate=c("2021-09-27", NA),
                           decimalLatitude=c("54.7", "33"),
                           decimalLongitude=c("88.9", "-48.4"),
                           genus=c("Aulacoseira", "Rosa sp."),
                           row.names=c("sp1", "sp2"))
  test_occur_DwC <- new("DwC.occurrence",
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
                        QC=TRUE)
  
  expect_s4_class(dataQC.DwC(Occurrence=test_occur, ask.input = FALSE, out.type="occurrence"), 
                  "DwC.occurrence")
  expect_error(dataQC.DwC(Occurrence=test_occur, ask.input = FALSE, out.type="event"))
  expect_equal(dataQC.DwC(Occurrence=test_occur, ask.input = FALSE, out.type="occurrence"),
               test_occur_DwC)
})


test_that("dataQC.DwC works for event data with eMoF", {
  test_event <- data.frame(eventID=c("sample1", "sample2"),
                           eventDate=c("2021-09-27", "2021-09-28"),
                           decimalLatitude=c("54.7", "33"),
                           decimalLongitude=c("88.9", "-48.4"),
                           row.names=c("sample1", "sample2"))
  test_emof <- data.frame(eventID=c("sample1", "sample2"),
                           eventDate=c("2021-09-27", "2021-09-06"),
                           measurementValueID=c(4, 5),
                           measurementUnitID=c("m", "m"),
                           measurementTypeID=c("MIxS:depth", "MIxS:depth"),
                           row.names=c("sample1", "sample2"))
  test_event_DwC <- new("DwC.event",
                        core = test_event,
                        occurrence = data.frame(),
                        emof = test_emof,
                        EML.url=as.character(NA),
                        QC=TRUE)
  
  expect_equal(dataQC.DwC(Event=test_event, eMoF=test_emof, ask.input = FALSE, out.type="event"),
               test_event_DwC)
})

