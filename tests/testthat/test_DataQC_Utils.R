#==============================================================
# Author Maxime Sweetlove
# Part of the POLA3R website (successor or mARS.biodiversity.aq)
# version 1.0 (2021-09-23)
# file encding UTF-8
#
#==============================================================
library(OmicsMetaData)
library(testthat)



test_that("dataQC.dateCheck delivers on dates it can handle", {
  dates <- c("2020-09-23", "2020", "16 Jan. 2020", "November 1998", "12/01/1999")
  latitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  longitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  sampleNames <- paste("sample", 1:5, sep="_")
  test_metadata <- data.frame(sample_name=sampleNames,
                              collection_date=dates,
                              latitude=latitudes,
                              longitude=longitudes)
  
  expect_equal(dataQC.dateCheck(dataset=test_metadata, date.colnames=c("collection_date")),
               list(values=c("2020-09-23","2020","2020-01-16","1998-11","1999-01-12"), 
                    warningmessages=NULL))
})

test_that("dataQC.LatitudeLongitudeCheck delivers on values it can handle", {
  dates <- c("2020-09-23", "2020", "16 Jan. 2020", "November 1998", "12/01/1999")
  latitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  longitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  sampleNames <- paste("sample", 1:5, sep="_")
  test_metadata <- data.frame(sample_name=sampleNames,
                              collection_date=dates,
                              latitude=latitudes,
                              longitude=longitudes)
  
  expect_equal(dataQC.LatitudeLongitudeCheck(dataset=test_metadata, 
                                             latlon.colnames=list(c("lat_lon"),c("latitude"), c("longitude"))),
               list(values=c("23 23","45 45","-56.44 -56.44","47.5 47.5","-87.9319 -87.9319"), 
                    warningmessages=NULL))
})

test_that("dataQC.guess.env_package.from.data works fine", {
  dates <- c("2020-09-23", "2020", "16 Jan. 2020", "November 1998", "12/01/1999")
  latitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  longitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  sampleNames <- paste("sample", 1:5, sep="_")
  test_metadata <- data.frame(sample_name=sampleNames,
                              collection_date=dates,
                              latitude=latitudes,
                              longitude=longitudes)
  
  expect_equal(dataQC.guess.env_package.from.data(dataset=test_metadata, pckge.colnames=c("env_package", "ScientificName")),
               list(values=NULL, 
                    warningmessages=c("the env_package was not specified. An educated guess was made, but should be checked",
                                      "No env_package could be infered")))
  
  test_metadata$env_package <- c(rep("water", 5))
  expect_type(dataQC.guess.env_package.from.data(dataset=test_metadata),
               "list")
  test_metadata$env_package <- c("sea", "lake", "loam", "soil", "biofilm")
  expect_type(dataQC.guess.env_package.from.data(dataset=test_metadata),
              "list")
  test_metadata$env_package <- c("cement", "air", "sediment", "microbial_mat_biofilm", "human_associated")
  expect_type(dataQC.guess.env_package.from.data(dataset=test_metadata),
              "list")
  test_metadata$env_package <- c("human_gut", "human_oral", "human_skin", "human_vaginal", "wastewater_sludge")
  expect_type(dataQC.guess.env_package.from.data(dataset=test_metadata),
              "list")
  test_metadata$env_package <- c("skin", "lungs", "miscellaneous", "miscellaneous", "")
  expect_type(dataQC.guess.env_package.from.data(dataset=test_metadata),
              "list")
  
})

test_that("dataQC.TermsCheck gives correct output", {
  expect_equal(dataQC.TermsCheck(observed="ph", exp.standard="MIxS", exp.section=NA, fuzzy.match=TRUE,out.type="full"),
               list(terms_OK="ph", terms_wrongWithSolution=NULL, terms_notFound=NULL))
  expect_equal(dataQC.TermsCheck(observed="cond", exp.standard="MIxS", exp.section=NA, fuzzy.match=TRUE,out.type="full"),
               list(terms_OK=NULL, terms_wrongWithSolution=c(cond="conduc"), terms_notFound=NULL))
  expect_equal(dataQC.TermsCheck(observed="randomness", exp.standard="MIxS", exp.section=NA, fuzzy.match=TRUE,out.type="full"),
               list(terms_OK=NULL, terms_wrongWithSolution=NULL, terms_notFound="randomness"))
  
  expect_equal(dataQC.TermsCheck(observed="eventid", exp.standard="DwC", 
                                 exp.section=NA, fuzzy.match=TRUE,out.type="full"),
               list(terms_OK=NULL, terms_wrongWithSolution=c(eventid="eventID"), 
                    terms_notFound=NULL))
  expect_equal(dataQC.TermsCheck(observed="eventid", exp.standard="DwC", 
                                 exp.section=NA, fuzzy.match=TRUE,out.type="best_match"),
               c(eventid="eventid"))
  expect_equal(dataQC.TermsCheck(observed="eventid", exp.standard="DwC", 
                                 exp.section=NA, fuzzy.match=FALSE,out.type="best_match"),
               c(eventid="eventid"))
  
  expect_equal(dataQC.TermsCheck(observed="eventid", exp.standard="DwC", 
                                 exp.section="occurrence", fuzzy.match=TRUE,
                                 out.type="best_match"),
               c(eventid="eventid"))
  expect_equal(dataQC.TermsCheck(observed="eventid", exp.standard="DwC", 
                                 exp.section="event", fuzzy.match=TRUE,
                                 out.type="best_match"),
               c(eventid="eventid"))
  
  expect_equal(dataQC.TermsCheck(observed="sample", exp.standard="INSDC", 
                                 exp.section=NA, fuzzy.match=TRUE,out.type="full"),
               list(terms_OK=NULL, terms_wrongWithSolution=c(sample="INSDC_SampleID"), 
                    terms_notFound=NULL))
  
  expect_equal(dataQC.TermsCheck(observed="sample", exp.standard="INSDC", 
                                 exp.section=NA, fuzzy.match=TRUE,out.type="best_match"),
               c(sample="sample"))
  expect_equal(dataQC.TermsCheck(observed="sample", exp.standard="INSDC", 
                                 exp.section=NA, fuzzy.match=FALSE,out.type="best_match"),
               c(sample="sample"))
  
  expect_error(dataQC.TermsCheck(observed="randomness", exp.standard="other", 
                                 exp.section=NA, fuzzy.match=TRUE,out.type="full"))
})

test_that("dataQC.generate.footprintWKT gives correct output", {
  expect_equal(dataQC.generate.footprintWKT(data.frame(decimalLatitude="23", decimalLongitude=45, eventID="sample_1"), NA.val=NA),
               "POINT(45 23)")
  expect_equal(dataQC.generate.footprintWKT(data.frame(decimalLatitude=c(23, 24), decimalLongitude=c(45, -46), eventID=c("sample_1", "sample_2")), NA.val=NA),
               c("POINT(45 23)", "POINT(-46 24)"))
  expect_equal(dataQC.generate.footprintWKT(data.frame(decimalLatitude=c(23, 24, 25), 
                                                       decimalLongitude=c(45, -46, 47), 
                                                       eventID=c("sample_01", "sample_02", "sample_1"),
                                                       parentEventID=c("sample_1", "sample_1", NA)),
                                            NA.val=NA),
               c("POINT(45 23)", "POINT(-46 24)", "LINESTRING(45 23, -46 24)"))
  expect_equal(dataQC.generate.footprintWKT(data.frame(decimalLatitude=c(23, 24, 25, 27, 28), 
                                                       decimalLongitude=c(45, -46, 47, -48, 49), 
                                                       eventID=c("sample_01", "sample_02", "sample_1", "sample_04", "sample05"),
                                                       parentEventID=c("sample_1", "sample_1", NA, "sample_2", "sample_1")),
                                            NA.val=""),
               c("POINT(45 23)", "POINT(-46 24)", "", "POINT(-48 27)", "POINT(49 28)"))

})

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

test_that("dataQC.TaxonListFromData finds correct taxonomic columns", {
  dates <- c("2020-09-23", "2020", "16 Jan. 2020", "November 1998", "12/01/1999")
  latitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  longitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  sampleNames <- paste("sample", 1:5, sep="_")
  test_metadata <- data.frame(sample_name=sampleNames,
                              collection_date=dates,
                              latitude=latitudes,
                              longitude=longitudes)
  tax_sample <- c("Aulacoseira", "Calothrix confervicola", "unknown species", "Micrasterias cf. denticulata", "Calothrix sp.")
  phy_sample <- c("Bacilariophyta", "Cyanobacteria", "undetermined", "Chlorophta", "Cyanobacteria")
  test_metadata3 <- data.frame(test_metadata, 
                               genus=tax_sample)
  test_metadata4 <- data.frame(test_metadata, 
                               species=tax_sample)
  test_metadata5 <- data.frame(test_metadata, 
                               phylum=phy_sample,
                               genus=tax_sample)
  
  expect_equal(dataQC.TaxonListFromData(test_metadata3),tax_sample)
  expect_equal(dataQC.TaxonListFromData(test_metadata4),as.character(rep(NA, 5)))
  expect_equal(dataQC.TaxonListFromData(test_metadata5),tax_sample)
})

test_that("dataQC.taxaNames performs correctly", {
  tax_sample <- c("Aulacoseira", "Calothrix confervicola", "unknown species", "Micrasterias cf. denticulata", "Calothrix sp.")
  tax_sampl_OUT <- data.frame(speciesLevelName=c("Aulacoseira sp.", "Calothrix confervicola", "unknown species", "Micrasterias cf. denticulata", "Calothrix sp."),
                              scientificName=c("Aulacoseira", "Calothrix confervicola", "unknown species", "Micrasterias", "Calothrix"),
                              identificationQualifier =c("sp.", NA, NA, "cf. denticulata", "sp."))
  expect_equal(dataQC.taxaNames(tax_sample),
               tax_sampl_OUT)
})

test_that("dataQC.completeTaxaNamesFromRegistery performs correctly", {
  taxon <- "Aulacoseira"
  taxon_OUT <- data.frame(scientificName="Aulacoseira",
                          scientificNameID="urn:lsid:marinespecies.org:taxname:148959", aphID=148959,
                          kingdom="Chromista", phylum="Ochrophyta", class="Bacillariophyceae",
                          order="Aulacoseirales", family="Aulacoseiraceae", genus="Aulacoseira",
                          specificEpithet=as.character(NA), scientificNameAuthorship="G.H.K. Thwaites",
                          namePublishedInYear="1848")
  unknown_OUT <- data.frame(scientificName="unknown",
                            scientificNameID=NA, aphID=NA,
                            kingdom=NA, phylum=NA, class=NA,
                            order=NA, family=NA, genus=NA,
                            specificEpithet=NA, scientificNameAuthorship=NA,
                            namePublishedInYear=NA)
  
  expect_equal(dataQC.completeTaxaNamesFromRegistery(taxon), taxon_OUT)
  
  expect_equal(dataQC.completeTaxaNamesFromRegistery("unknown"), unknown_OUT)
})

test_that("dataQC.eventStructure performs as expected", {
  dates <- c("2020-09-23", "2020", "16 Jan. 2020", "November 1998", "12/01/1999")
  latitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  longitudes <- c(23, 45, -56.44, "47.5", "-88° 4\' 5\"")
  sampleNames <- paste("sample", 1:5, sep="_")
  test_metadata <- data.frame(sample_name=sampleNames,
                              collection_date=dates,
                              latitude=latitudes,
                              longitude=longitudes)
  test_metadata_OUT_1 <- data.frame(original_name=rownames(test_metadata),
                                  eventID=sampleNames,
                                  parentEventID=rep("", 5),
                                  type=rep("event", 5),
                                  row.names=rownames(test_metadata))
  test_metadata_OUT_2 <- data.frame(original_name=rownames(test_metadata),
                                    eventID=sampleNames,
                                    parentEventID=rep("", 5),
                                    type=rep("event", 5),
                                    project=rep("project_1", 5),
                                    row.names=rownames(test_metadata))
  test_metadata_OUT_3 <- data.frame(original_name=c(NA, rownames(test_metadata)),
                                    eventID=c("project_1", sampleNames),
                                    parentEventID=c(NA, rep("project_1", 5)),
                                    project=rep("project_1", 6),
                                    type=c("project", rep("event", 5)),
                                    original=c(FALSE, rep(TRUE, 5)),
                                    row.names=c("project_1", "11", "2", "3", "4", "5"))
  
  expect_equal(dataQC.eventStructure(dataset=test_metadata, eventID.col = "sample_name", parentEventID.col = NA,
                                     project.col = NA, project = NA, event.prefix = NA,
                                     complete.hierarchy=FALSE),
               test_metadata_OUT_1)
  expect_equal(dataQC.eventStructure(dataset=test_metadata, eventID.col = "sample_name", parentEventID.col = NA,
                                     project.col = NA, project = "project_1", event.prefix = NA,
                                     complete.hierarchy=FALSE),
               test_metadata_OUT_2)
  expect_equal(dataQC.eventStructure(dataset=test_metadata, eventID.col = "sample_name", parentEventID.col = NA,
                                     project.col = NA, project = "project_1", event.prefix = NA,
                                     complete.hierarchy=TRUE),
               test_metadata_OUT_3)
})


