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

test_that("FileNames.to.Table works", {
  #make mock dir with some empty files
  file_dir <- paste(getwd(), "tempdir_test", sep="/")
  dir.create(file_dir)
  file.create(paste(file_dir, "seq_sample1_1.fastq.gz", sep="/"))
  file.create(paste(file_dir, "seq_sample1_2.fastq.gz", sep="/"))
  
  fileNamesTable <- data.frame(NewName="", OldName="seq_sample1", 
                               ForwardName="seq_sample1_1.fastq.gz",
                               ReverseName="seq_sample1_2.fastq.gz",
                               row.names=c("seq_sample1"))
  
  
  expect_equal(FileNames.to.Table(file.dir=file_dir, paired=TRUE, seq.file.extension=".fastq.gz",
                                  pairedEnd.extension=c("_1", "_2")),
               fileNamesTable)
  
  # clean up
  closeAllConnections()
  unlink(file_dir, recursive=TRUE)
  
})

test_that("renameSequenceFiles works without user input", {
  #make mock dir with some empty files
  file_dir <- paste(getwd(), "tempdir_test", sep="/")
  dir.create(file_dir)
  file.create(paste(file_dir, "seq_sample1_1.fastq.gz", sep="/"))
  file.create(paste(file_dir, "seq_sample1_2.fastq.gz", sep="/"))
  
  fileNamesTable <- data.frame(NewName="", OldName="seq_sample1", 
                               ForwardName="seq_sample1_1.fastq.gz",
                               ReverseName="seq_sample1_2.fastq.gz",
                               row.names=c("seq_sample1"))
  
  fileNamesTable_rename <- data.frame(NewName="s1renamed", OldName="seq_sample1")
  
  expect_equal(renameSequenceFiles(name_df=fileNamesTable_rename, colNameOld="OldName", colNameNew="NewName",
                                   file.dir=file_dir, paired=TRUE, seq.file.extension=".fastq.gz",
                                   ask.input=F, pairedEnd.extension=c("_1", "_2")),
               2)
  
  # clean up
  closeAllConnections()
  unlink(file_dir, recursive=TRUE)
  
})

test_that("renameSequenceFiles works with user input", {
  #make mock dir with some empty files
  file_dir <- paste(getwd(), "tempdir_test", sep="/")
  dir.create(file_dir)
  file.create(paste(file_dir, "seq_sample1_1.fastq.gz", sep="/"))
  file.create(paste(file_dir, "seq_sample1_2.fastq.gz", sep="/"))
  fileNamesTable <- data.frame(NewName="", OldName="seq_sample1", 
                               ForwardName="seq_sample1_1.fastq.gz",
                               ReverseName="seq_sample1_2.fastq.gz",
                               row.names=c("seq_sample1"))
  fileNamesTable_rename <- data.frame(NewName="s1renamed", OldName="seq_sample1")
  
  #turn of user connection
  f<-file()
  options(mypkg.connection = f)
  ans <- c("y", #test1
           "n") #test2
  write(ans, f)
  
  expect_equal(renameSequenceFiles(name_df=fileNamesTable_rename, colNameOld="OldName", colNameNew="NewName",
                                   file.dir=file_dir, paired=TRUE, seq.file.extension=".fastq.gz",
                                   ask.input=T, pairedEnd.extension=c("_1", "_2")),
               2)
  expect_error(renameSequenceFiles(name_df=fileNamesTable_rename, colNameOld="OldName", colNameNew="NewName",
                                   file.dir=file_dir, paired=TRUE, seq.file.extension=".fastq.gz",
                                   ask.input=T, pairedEnd.extension=c("_1", "_2")))
  
  #reset connection
  options(mypkg.connection = stdin())
  close(f)
  # clean up
  closeAllConnections()
  unlink(file_dir, recursive=TRUE)

})


test_that("sync.metadata.sequenceFiles works: match", {
  #make mock dir with some empty files
  file_dir <- paste(getwd(), "tempdir_test", sep="/")
  dir.create(file_dir)
  file.create(paste(file_dir, "seq_sample1_1.fastq.gz", sep="/"))
  file.create(paste(file_dir, "seq_sample1_2.fastq.gz", sep="/"))
  
  test_metadata <- data.frame(sample_name="seq_sample1",
                              collection_date="2021-09-27",
                              lon_lat="54.7 88.9")
  
  expect_equal(sync.metadata.sequenceFiles("seq_sample1", file.dir=file_dir,
                                           paired=TRUE, seq.file.extension=".fastq.gz",
                                           pairedEnd.extension=c("_1", "_2")),
               list(redundant_metadata=NULL, redundant_seqFiles=NULL))
  expect_equal(sync.metadata.sequenceFiles("seq_sample2", file.dir=file_dir,
                                           paired=TRUE, seq.file.extension=".fastq.gz",
                                           pairedEnd.extension=c("_1", "_2")),
               list(redundant_metadata="seq_sample2", 
                    redundant_seqFiles=c("seq_sample1_1.fastq.gz", "seq_sample1_2.fastq.gz")))

  # clean up
  closeAllConnections()
  unlink(file_dir, recursive=TRUE)
  
  })

test_that("sync.metadata.sequenceFiles works: mismatch", {
  #make mock dir with some empty files
  file_dir <- paste(getwd(), "tempdir_test", sep="/")
  dir.create(file_dir)
  file.create(paste(file_dir, "seq_sample1_1.fastq.gz", sep="/"))
  file.create(paste(file_dir, "seq_sample1_2.fastq.gz", sep="/"))
  file.create(paste(file_dir, "extra_1.fastq.gz", sep="/"))
  
  expect_error(sync.metadata.sequenceFiles("seq_sample1", file.dir=file_dir,
                                           paired=TRUE, seq.file.extension=".fastq.gz",
                                           pairedEnd.extension=c("_1", "_2")))
  
  file.create(paste(file_dir, "extra_2.fastq.gz", sep="/"))
  
  expect_equal(sync.metadata.sequenceFiles(c("seq_sample1", "seq_sample2"), 
                                           file.dir=file_dir,
                                           paired=TRUE, seq.file.extension=".fastq.gz",
                                           pairedEnd.extension=c("_1", "_2")),
               list(redundant_metadata="seq_sample2", 
                    redundant_seqFiles=c("extra_1.fastq.gz", "extra_2.fastq.gz")))
  
  # clean up
  closeAllConnections()
  unlink(file_dir, recursive=TRUE)
  
})

test_that("prep.metadata.ENA works without user input", {
  sampleNames <- c("sample_1", "sample_2")
  test_MIxS <- new("MIxS.metadata",
                   data   = data.frame(var1=c(1,2), 
                                       var2=c(3,4), 
                                       eventID=sampleNames, 
                                       target_gene=c("16S", "18S"), 
                                       subspecf_gen_lin=c("Bacteria", "Eukaryota"),
                                       seq_meth=c("Illumina MiSeq", "Illumina MiSeq"),
                                       row.names=sampleNames),
                   section = c(var1="section1", var2="section1", eventID="miscellaneous",
                               target_gene="miscellaneous", subspecf_gen_lin="miscellaneous",
                               seq_meth="miscellaneous"),
                   units      = c(var1="unit1", var2="unit2", eventID="alphanumeric",
                                  target_gene="alphanumeric", subspecf_gen_lin="alphanumeric",
                                  seq_meth="alphanumeric"),
                   env_package = "water",
                   type = "versatile",
                   QC = TRUE)
  
  expect_error(prep.metadata.ENA(metadata=test_metadata, dest.dir=file_dir, file.name="testthat.csv"))
  
  #make mock dir with some empty files
  file_dir <- paste(getwd(), "tempdir_test", sep="/")
  dir.create(file_dir)
  file.create(paste(file_dir, "seq_sample1_1.fastq.gz", sep="/"))
  file.create(paste(file_dir, "seq_sample1_2.fastq.gz", sep="/"))
  
  
  expect_equal(prep.metadata.ENA(metadata=test_MIxS, dest.dir=file_dir, file.name="testthat.txt",
                                 sample_unique_name_prefix=NA, checklist_accession=NA,
                                 tax_name=NA, ask.input=FALSE,
                                 insert.size=NA, library.layout=NA,
                                 library.strategy=NA, library.selection=NA,
                                 seq.file.extension=".fastq.gz", pairedEnd.extension=c("_1", "_2")),
               NULL)
  expect_equal(prep.metadata.ENA(metadata=test_MIxS, dest.dir=file_dir, file.name="testthat.txt",
                                 sample_unique_name_prefix="sample_xx_", checklist_accession="ERC000025",
                                 tax_name="Bacteria", ask.input=FALSE,
                                 insert.size="350", library.layout="paired-end",
                                 library.strategy="WGS", library.selection="PCR",
                                 seq.file.extension=".fastq.gz", pairedEnd.extension=c("_1", "_2")),
               NULL)
  expect_error(prep.metadata.ENA(metadata=test_metadata, dest.dir=file_dir, file.name="testthat.csv",
                                 sample_unique_name_prefix=NA, checklist_accession=NA,
                                 tax_name=NA, ask.input=FALSE,
                                 insert.size=NA, library.layout=NA,
                                 library.strategy=NA, library.selection=NA,
                                 seq.file.extension=".fastq.gz", pairedEnd.extension=c("_1", "_2")))
  
  expect_error(prep.metadata.ENA(metadata=data.frame(), dest.dir=file_dir, file.name="testthat"))
  
  test_MIxS@env_package<-""
  
  expect_error(prep.metadata.ENA(metadata=test_MIxS, dest.dir=file_dir, file.name="testthat"))
  
  # clean up
  closeAllConnections()
  unlink(file_dir, recursive=TRUE)


  })

test_that("prep.metadata.ENA works with user input 1", {
  sampleNames <- c("sample_1", "sample_2")
  test_MIxS <- new("MIxS.metadata",
                   data   = data.frame(var1=c(1,2), 
                                       var2=c(3,4), 
                                       eventID=sampleNames, 
                                       target_gene=c("16S", "16S"), 
                                       subspecf_gen_lin=c("Bacteria", "Bacteria"),
                                       seq_meth=c("Illumina MiSeq", "Illumina MiSeq"),
                                       row.names=sampleNames),
                   section = c(var1="section1", var2="section1", eventID="miscellaneous",
                               target_gene="miscellaneous", subspecf_gen_lin="miscellaneous",
                               seq_meth="miscellaneous"),
                   units      = c(var1="unit1", var2="unit2", eventID="alphanumeric",
                                  target_gene="alphanumeric", subspecf_gen_lin="alphanumeric",
                                  seq_meth="alphanumeric"),
                   env_package = "water",
                   type = "versatile",
                   QC = TRUE)
  #make mock dir with some empty files
  file_dir <- paste(getwd(), "tempdir_test", sep="/")
  dir.create(file_dir)
  file.create(paste(file_dir, "seq_sample1_1.fastq.gz", sep="/"))
  file.create(paste(file_dir, "seq_sample1_2.fastq.gz", sep="/"))
  
  #turn of user connection
  f<-file()
  options(mypkg.connection = f)
  ans <- c("tesst", "Cyanobacteria", "water sample", "h","Illumina HiSeq 1500",
           "a", "h","RANDOM PCR", "h", "AMPLICON", "a", "300")
  write(ans, f)
  
  expect_equal(prep.metadata.ENA(metadata=test_MIxS, dest.dir=file_dir, 
                                 file.name="testthat",
                                 sample_unique_name_prefix=NA, checklist_accession=NA,
                                 tax_name=NA, ask.input=TRUE,
                                 insert.size=NA, library.layout=NA,
                                 library.strategy=NA, library.selection=NA,
                                 seq.file.extension=".fastq.gz", 
                                 pairedEnd.extension=c("_1", "_2")),
               NULL)
  
  #reset connection
  options(mypkg.connection = stdin())
  close(f)
  # clean up after last function
  unlink(file_dir, recursive=TRUE)
})

test_that("prep.metadata.ENA works with user input 2", {
  sampleNames <- c("sample_1", "sample_2")
  test_MIxS <- new("MIxS.metadata",
                   data   = data.frame(var1=c(1,2), 
                                       var2=c(3,4), 
                                       eventID=sampleNames, 
                                       lat_lon=c("20 30", "-45 66"),
                                       target_gene=c("16S", "16S"), 
                                       subspecf_gen_lin=c("Bacteria", "Bacteria"),
                                       investigation_type=c("mimarks-survey", "mimarks-survey"),
                                       seq_meth=c("Illumina HiSeq 1500", "Illumina HiSeq 1500"),
                                       sample_description=c("water", "soil"),
                                       row.names=sampleNames),
                   section = c(var1="section1", var2="section1", eventID="miscellaneous", lat_lon="miscellaneous",
                               target_gene="miscellaneous", subspecf_gen_lin="miscellaneous",
                               investigation_type="miscellaneous", seq_meth="miscellaneous",
                               sample_description="miscellaneous"),
                   units      = c(var1="unit1", var2="unit2", eventID="alphanumeric",  lat_lon="decimal degrees",
                                  target_gene="alphanumeric", subspecf_gen_lin="alphanumeric",
                                  investigation_type="alphanumeric", seq_meth="alphanumeric", 
                                  sample_description="alphanumeric"),
                   env_package = "water",
                   type = "versatile",
                   QC = TRUE)
  #make mock dir with some empty files
  file_dir <- paste(getwd(), "tempdir_test", sep="/")
  dir.create(file_dir)
  
  #turn of user connection
  f<-file()
  options(mypkg.connection = f)
  ans <- c("tesst", "c","unkown", "RANDOM PCR", 
           "AMPLICON", "b", "n")
  write(ans, f)
  
  expect_equal(prep.metadata.ENA(metadata=test_MIxS, dest.dir=file_dir, 
                                 file.name="testthat",
                                 sample_unique_name_prefix=NA, checklist_accession="ERC000019",
                                 tax_name=NA, ask.input=TRUE,
                                 insert.size=NA, library.layout=NA,
                                 library.strategy=NA, library.selection=NA,
                                 seq.file.extension=".fasta", 
                                 pairedEnd.extension=c("-fwd", "-rev")),
               NULL)
  
  expect_error(prep.metadata.ENA(metadata=test_MIxS, dest.dir=file_dir, file.name="testthat",
                                 sample_unique_name_prefix=NA, checklist_accession="xxx"))
  
  
  #reset connection
  options(mypkg.connection = stdin())
  close(f)
  # clean up after last function
  closeAllConnections()
  unlink(file_dir, recursive=TRUE)
})

test_that("prep.metadata.ENA works with user input 3", {
  sampleNames <- c("sample_1", "sample_2")
  test_MIxS <- new("MIxS.metadata",
                   data   = data.frame(var1=c(1,2), 
                                       var2=c(3,4), 
                                       misc_param=c("original_name:s1", "original_name:s2"), 
                                       decimalLatitude=c("20", "-45"),
                                       decimalLongitude=c("30", "66"),
                                       target_gene=c("16S", "16S"), 
                                       scientific_name=c("Bacteria", "Bacteria"),
                                       target_taxa=c("Bacteria", "Bacteria"),
                                       seq_meth=c("Illumina HiSeq 1500", "Illumina HiSeq 1500"),
                                       geo_loc_name=c("Costa Brava", "Belgium"),
                                       row.names=sampleNames),
                   section = c(var1="section1", var2="section1", misc_param="miscellaneous", 
                               decimalLatitude="miscellaneous", decimalLongitude="miscellaneous",
                               target_gene="miscellaneous", subspecf_gen_lin="miscellaneous",
                               target_taxa="miscellaneous", seq_meth="miscellaneous",
                               geo_loc_name="miscellaneous"),
                   units      = c(var1="unit1", var2="unit2", misc_param="alphanumeric",  
                                  decimalLatitude="decimal degrees", decimalLongitude="decimal degrees",
                                  target_gene="alphanumeric", subspecf_gen_lin="alphanumeric",
                                  target_taxa="alphanumeric", seq_meth="alphanumeric",
                                  geo_loc_name="alphanumeric"),
                   env_package = "not_specified",
                   type = "versatile",
                   QC = TRUE)
  #make mock dir with some empty files
  file_dir <- paste(getwd(), "tempdir_test", sep="/")
  dir.create(file_dir)
  
  #turn of user connection
  f<-file()
  options(mypkg.connection = f)
  ans <- c("tesst", "n", "h", "North Sea", "n", "e","n",  "n", "a", "200", #test 1
           "tesst", "n", "n", "n",  "d") #test 2
  write(ans, f)
  
  expect_equal(prep.metadata.ENA(metadata=test_MIxS, dest.dir=file_dir, 
                                 file.name="testthat",
                                 sample_unique_name_prefix=NA, checklist_accession="ERC000019",
                                 tax_name=NA, ask.input=TRUE,
                                 insert.size=NA, library.layout=NA,
                                 library.strategy=NA, library.selection=NA,
                                 seq.file.extension=".fasta", 
                                 pairedEnd.extension=c("-fwd", "-rev")),
               NULL)
  
  expect_error(prep.metadata.ENA(metadata=test_MIxS, dest.dir=file_dir, 
                                 file.name="testthat"))
  

  
  #reset connection
  options(mypkg.connection = stdin())
  close(f)
  # clean up after last function
  closeAllConnections()
  unlink(file_dir, recursive=TRUE)
})


closeAllConnections()



