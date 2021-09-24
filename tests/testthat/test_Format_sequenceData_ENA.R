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
dir.create(paste(getwd(), "tempdir_test", sep="/"))


# clean up
unlink(paste(getwd(), "tempdir_test", sep="/"), recursive=TRUE)


FileNames.to.Table(file.dir=NULL, paired=TRUE, seq.file.extension=".fastq.gz",
                               pairedEnd.extension=c("_1", "_2"))

renameSequenceFiles <- function(name_df, colNameOld="OldName", colNameNew="NewName",
                                file.dir=NULL, paired=TRUE, seq.file.extension=".fastq.gz",
                                ask.input=TRUE, pairedEnd.extension=c("_1", "_2"))
  
  sync.metadata.sequenceFiles(Names, file.dir=NULL,
                              paired=TRUE, seq.file.extension=".fastq.gz",
                              pairedEnd.extension=c("_1", "_2"))