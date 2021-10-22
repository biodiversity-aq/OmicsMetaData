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

dates <- c("2020-09-23", "2020", "16 Jan. 2020", "November 1998", "12/01/1999")
latitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
longitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
sampleNames <- paste("sample", 1:5, sep="_")
test_metadata <- data.frame(sample_name=sampleNames,
                           collection_date=dates,
                           latitude=latitudes,
                           longitude=longitudes)

test_that("dataQC.dateCheck delivers on dates it can handle", {
  expect_equal(dataQC.dateCheck(dataset=test_metadata, date.colnames=c("collection_date")),
               list(values=c("2020-09-23","2020","2020-01-16","1998-11","1999-01-12"), 
                    warningmessages=NULL))
})

test_that("dataQC.LatitudeLongitudeCheck delivers on values it can handle", {
  expect_equal(dataQC.LatitudeLongitudeCheck(dataset=test_metadata, 
                                             latlon.colnames=list(c("lat_lon"),c("latitude"), c("longitude"))),
               list(values=c("23 23","45 45","-56.44 -56.44","47.5 47.5","-87.9319 -87.9319"), 
                    warningmessages=NULL))
})

test_that("dataQC.guess.env_package.from.data works fine", {
  expect_equal(dataQC.guess.env_package.from.data(dataset=test_metadata, pckge.colnames=c("env_package", "ScientificName")),
               list(values=NULL, 
                    warningmessages=c("the env_package was not specified. An educated guess was made, but should be checked",
                                      "No env_package could be infered")))
})

test_that("dataQC.TermsCheck gives correct output", {
  expect_equal(dataQC.TermsCheck(observed="ph", exp.standard="MIxS", exp.section=NA, fuzzy.match=TRUE,out.type="full"),
               list(terms_OK="ph", terms_wrongWithSolution=NULL, terms_notFound=NULL))
  expect_equal(dataQC.TermsCheck(observed="cond", exp.standard="MIxS", exp.section=NA, fuzzy.match=TRUE,out.type="full"),
               list(terms_OK=NULL, terms_wrongWithSolution=c(cond="conduc"), terms_notFound=NULL))
  expect_equal(dataQC.TermsCheck(observed="randomness", exp.standard="MIxS", exp.section=NA, fuzzy.match=TRUE,out.type="full"),
               list(terms_OK=NULL, terms_wrongWithSolution=NULL, terms_notFound="randomness"))
})


test_that("dataQC.generate.footprintWKT gives correct output", {
  expect_equal(dataQC.generate.footprintWKT(data.frame(decimalLatitude="23", decimalLongitude=45, eventID="sample_1"), NA.val=NA),
               "POINT(45 23)")
  expect_equal(dataQC.generate.footprintWKT(data.frame(decimalLatitude=c(23, 24), decimalLongitude=c(45, -46), eventID=c("sample_1", "sample_2")), NA.val=NA),
               c("POINT(45 23)", "POINT(-46 24)"))
})

test_metadata2 <- test_metadata
colnames(test_metadata2)[1]<-"eventID"

test_that("dataQC.findNames gives correct output without user input", {
  dates <- c("2020-09-23", "2020", "16 Jan. 2020", "November 1998", "12/01/1999")
  latitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  longitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  sampleNames <- paste("sample", 1:5, sep="_")
  test_metadata <- data.frame(sample_name=sampleNames,
                              collection_date=dates,
                              latitude=latitudes,
                              longitude=longitudes)
  test_metadata2 <- test_metadata
  colnames(test_metadata2)[1]<-"eventID"
  
  test_metadata_OUT1 <- list(Names=data.frame(original_names=sampleNames, eventID=NA, 
                                             parentEventID=NA, 
                                             occurrenceID=NA, INSDC_SampleID=NA, 
                                             row.names=rownames(test_metadata)),
                            Names.column="sample_name", 
                            warningmessages = "")
  test_metadata_OUT2 <- list(Names=data.frame(original_names=sampleNames, eventID=sampleNames, parentEventID=NA, 
                                              occurrenceID=NA, INSDC_SampleID=NA, row.names=rownames(test_metadata2)),
                             Names.column="eventID", 
                             warningmessages = "assumed the \"eventID\" column contained the original sample names")
  
  expect_equal(dataQC.findNames(dataset=test_metadata, ask.input=FALSE, 
                                sample.names="sample_name"),
               test_metadata_OUT1)
  expect_equal(dataQC.findNames(dataset=test_metadata2, ask.input=FALSE, sample.names=NA),
               test_metadata_OUT2)
})

test_that("dataQC.findNames gives correct output WITH user input", {
  dates <- c("2020-09-23", "2020", "16 Jan. 2020", "November 1998", "12/01/1999")
  latitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  longitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  sampleNames <- paste("sample", 1:5, sep="_")
  test_metadata <- data.frame(sample_names=sampleNames,
                              collection_date=dates,
                              latitude=latitudes,
                              longitude=longitudes,
                              row.names=sampleNames)
  #turn of user connection
  f<-file()
  options(mypkg.connection = f)
  ans <- c("y", "n", "sample_names") #test 1
  write(ans, f)

  expect_type(dataQC.findNames(dataset=test_metadata, ask.input=TRUE, 
                                sample.names=NA), "list")
  expect_type(dataQC.findNames(dataset=test_metadata, ask.input=TRUE, 
                               sample.names=NA), "list")
  expect_error(dataQC.findNames(dataset=test_metadata, ask.input=TRUE, 
                                sample.names="randomname"))
  
  #reset connection
  options(mypkg.connection = stdin())
  close(f)
})

tax_sample <- c("Aulacoseira", "Calothrix confervicola", "unknown species", "Micrasterias cf. denticulata", "Calothrix sp.")
phy_sample <- c("Bacilariophyta", "Cyanobacteria", "undetermined", "Chlorophta", "Cyanobacteria")
test_metadata3 <- data.frame(test_metadata, 
                             genus=tax_sample)
test_metadata4 <- data.frame(test_metadata, 
                             species=tax_sample)
test_metadata5 <- data.frame(test_metadata, 
                             phylum=phy_sample,
                             genus=tax_sample)

test_that("dataQC.TaxonListFromData finds correct taxonomic columns", {
  expect_equal(dataQC.TaxonListFromData(test_metadata3),tax_sample)
  expect_equal(dataQC.TaxonListFromData(test_metadata4),as.character(rep(NA, 5)))
  expect_equal(dataQC.TaxonListFromData(test_metadata5),tax_sample)
})


test_that("dataQC.taxaNames performs correctly", {
  expect_equal(dataQC.taxaNames(tax_sample),
               data.frame(speciesLevelName=c("Aulacoseira sp.", "Calothrix confervicola", "unknown species", "Micrasterias cf. denticulata", "Calothrix sp."),
                          scientificName=c("Aulacoseira", "Calothrix confervicola", "unknown species", "Micrasterias", "Calothrix"),
                          identificationQualifier =c("sp.", NA, NA, "cf. denticulata", "sp.")))
})

test_that("dataQC.completeTaxaNamesFromRegistery performs correctly", {
  expect_equal(dataQC.completeTaxaNamesFromRegistery("Aulacoseira"),
               data.frame(scientificName="Aulacoseira",
                          scientificNameID="urn:lsid:marinespecies.org:taxname:148959", aphID=148959,
                          kingdom="Chromista", phylum="Ochrophyta", class="Bacillariophyceae",
                          order="Aulacoseirales", family="Aulacoseiraceae", genus="Aulacoseira",
                          specificEpithet=as.character(NA), scientificNameAuthorship="G.H.K. Thwaites",
                          namePublishedInYear="1848"))
  expect_equal(dataQC.completeTaxaNamesFromRegistery("Micrasterias denticulata"),
               data.frame(scientificName="Micrasterias denticulata",
                          scientificNameID="urn:lsid:marinespecies.org:taxname:609045", aphID=609045,
                          kingdom="Plantae", phylum="Charophyta", class="Conjugatophyceae",
                          order="Desmidiales", family="Desmidiaceae", genus="Micrasterias",
                          specificEpithet="denticulata", scientificNameAuthorship="Brébisson ex Ralfs",
                          namePublishedInYear="1848"))
  expect_equal(dataQC.completeTaxaNamesFromRegistery("unknown"),
               data.frame(scientificName="unknown",
                          scientificNameID=NA, aphID=NA,
                          kingdom=NA, phylum=NA, class=NA,
                          order=NA, family=NA, genus=NA,
                          specificEpithet=NA, scientificNameAuthorship=NA,
                          namePublishedInYear=NA))
})


test_that("dataQC.eventStructure performs as expected", {
  expect_equal(dataQC.eventStructure(dataset=test_metadata, eventID.col = "sample_name", parentEventID.col = NA,
                                     project.col = NA, project = NA, event.prefix = NA,
                                     complete.hierarchy=FALSE),
               data.frame(original_name=rownames(test_metadata),
                          eventID=sampleNames,
                          parentEventID=rep("", 5),
                          type=rep("event", 5),
                          row.names=rownames(test_metadata)))
  expect_equal(dataQC.eventStructure(dataset=test_metadata, eventID.col = "sample_name", parentEventID.col = NA,
                                     project.col = NA, project = "project_1", event.prefix = NA,
                                     complete.hierarchy=FALSE),
               data.frame(original_name=rownames(test_metadata),
                          eventID=sampleNames,
                          parentEventID=rep("", 5),
                          type=rep("event", 5),
                          project=rep("project_1", 5),
                          row.names=rownames(test_metadata)))
  expect_equal(dataQC.eventStructure(dataset=test_metadata, eventID.col = "sample_name", parentEventID.col = NA,
                                     project.col = NA, project = "project_1", event.prefix = NA,
                                     complete.hierarchy=TRUE),
               data.frame(original_name=c(NA, rownames(test_metadata)),
                          eventID=c("project_1", sampleNames),
                          parentEventID=c(NA, rep("project_1", 5)),
                          project=rep("project_1", 6),
                          type=c("project", rep("event", 5)),
                          original=c(FALSE, rep(TRUE, 5)),
                          row.names=c("project_1", "11", "2", "3", "4", "5")))
})


