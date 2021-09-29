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

#make mock dir with some empty files
file_dir <- paste(getwd(), "tempdir_test", sep="/")
dir.create(file_dir)
file.create(paste(file_dir, "seq_sample1_1.fastq.gz", sep="/"))
file.create(paste(file_dir, "seq_sample1_2.fastq.gz", sep="/"))

fileNamesTable <- data.frame(NewName="", OldName="seq_sample1", 
                             ForwardName="seq_sample1_1.fastq.gz",
                             ReverseName="seq_sample1_2.fastq.gz",
                             row.names=c("seq_sample1"))

test_that("FileNames.to.Table works", {
  expect_equal(FileNames.to.Table(file.dir=file_dir, paired=TRUE, seq.file.extension=".fastq.gz",
                                  pairedEnd.extension=c("_1", "_2")),
               fileNamesTable)
})

fileNamesTable_rename <- data.frame(NewName="s1renamed", OldName="seq_sample1")

test_that("renameSequenceFiles works", {
  expect_equal(renameSequenceFiles(name_df=fileNamesTable_rename, colNameOld="OldName", colNameNew="NewName",
                                   file.dir=file_dir, paired=TRUE, seq.file.extension=".fastq.gz",
                                   ask.input=F, pairedEnd.extension=c("_1", "_2")),
               2)
})

# clean up after last function
unlink(file_dir, recursive=TRUE)
# restart with a new directory
dir.create(file_dir)
file.create(paste(file_dir, "seq_sample1_1.fastq.gz", sep="/"))
file.create(paste(file_dir, "seq_sample1_2.fastq.gz", sep="/"))

test_metadata <- data.frame(sample_name="seq_sample1",
                            collection_date="2021-09-27",
                            lon_lat="54.7 88.9")

test_that("sync.metadata.sequenceFiles works", {
  expect_equal(sync.metadata.sequenceFiles("seq_sample1", file.dir=file_dir,
                                           paired=TRUE, seq.file.extension=".fastq.gz",
                                           pairedEnd.extension=c("_1", "_2")),
               list(redundant_metadata=NULL, redundant_seqFiles=NULL))
  expect_equal(sync.metadata.sequenceFiles("seq_sample2", file.dir=file_dir,
                                           paired=TRUE, seq.file.extension=".fastq.gz",
                                           pairedEnd.extension=c("_1", "_2")),
               list(redundant_metadata="seq_sample2", 
                    redundant_seqFiles=c("seq_sample1_1.fastq.gz", "seq_sample1_2.fastq.gz")))
})

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



test_that("prep.metadata.ENA works", {
  expect_equal(prep.metadata.ENA(metadata=test_MIxS, dest.dir=file_dir, file.name="testthat",
                                 sample_unique_name_prefix=NA, checklist_accession=NA,
                                 tax_name=NA, ask.input=FALSE,
                                 insert.size=NA, library.layout=NA,
                                 library.strategy=NA, library.selection=NA,
                                 seq.file.extension=".fastq.gz", pairedEnd.extension=c("_1", "_2")),
               NULL)
  expect_error(prep.metadata.ENA(metadata=test_metadata, dest.dir=file_dir, file.name="testthat",
                                 sample_unique_name_prefix=NA, checklist_accession=NA,
                                 tax_name=NA, ask.input=FALSE,
                                 insert.size=NA, library.layout=NA,
                                 library.strategy=NA, library.selection=NA,
                                 seq.file.extension=".fastq.gz", pairedEnd.extension=c("_1", "_2")))
})


# clean up
unlink(file_dir, recursive=TRUE)




