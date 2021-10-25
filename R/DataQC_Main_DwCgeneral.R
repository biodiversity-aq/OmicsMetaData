#==============================================================
# Author Maxime Sweetlove
# lisence CC-0 2020
# Part of the POLA3R website (successor or mARS.biodiversity.aq)
# version 1.0 (2020-01-28)
# file encdong UTF-8
#
# assumptions:
#    - use NA for missing values
#
#==============================================================
# data Quality Controll (QC) for DarwinCore
#==============================================================

#' Quality Controll (QC) for DarwinCore data
#' @author Maxime Sweetlove CC-0 2020
#' @family standardization functions
#' @description performs a general QC on a darwinCore file. Desired output is either and event (core) file, occurrence (core or extension) or extended Measurement or Fact (eMoF; extension) file. Not an Ecological dataset Language (EML) file.
#' @usage dataQC.DwC_general(dataset = NA, DwC.type = "event", ask.input = TRUE, complete.data=TRUE)
#' @param dataset data.frame. A dataframe with the data, structured with DarwinCore terms
#' @param DwC.type character. The type of DarwinCore of the output, either event or occurrence. If event, the output will have an event core with possible occurrence and eMoF extensions. If occurrence, the output will have an occurrence core with possibly an eMoF extension. Default event, if the parameter Event is not NA, out.type will be fixed as event.
#' @param ask.input logical. If TRUE, console input will be requested to the user when a problem occurs. Default TRUE
#' @param complete.data logical. If TRUE, data that has not been provided, but can be completed automatically will be added to the DarwinCore data.frame. For instance, footprintWKT can be generated from the coordinates, or higher taxonomic level names (like kingdom, phylum,...) can looked up with the species name. Defaut TRUE.
#' @details DarwinCore is the biodiversity data standard developed by TDWG, and is used by the Global Biodiversity Information Facility (GBIF). This function performs a basic and user-supervised quality control. This includes cheking all variables terms adhere to the DarwinCore vocabulary, listing other variables in an eMoF file or in the dynamicProperties field, and checking for obvious errors in the content of the data (typos, different NA values,...)
#' @return a dataframe that is formatted as either: an event or occurrence
#' @examples 
#' \dontrun{
#' test_event <- data.frame(eventID=c("sample1", "sample2"),
#'                          eventDate=c("2021-09-27", "2021-09-28"),
#'                          decimalLatitude=c("54.7", "33"),
#'                          decimalLongitude=c("88.9", "-48.4"),
#'                          row.names=c("sample1", "sample2"))
#' dataQC.DwC_general(dataset=test_event, DwC.type = "event", complete.data=TRUE)
#' }
#' @export
dataQC.DwC_general<-function(dataset = NA, DwC.type = "event", ask.input = TRUE,
                             complete.data=TRUE){
  #requires stringr and worrms
  warningmessages<-c()
  
  # check input
  if(tolower(DwC.type)=="event"){
    DwC.type<-"event"
    DwcType <- "Event"
    DwCLib<-"DwC_Event"
  }else if(tolower(DwC.type)=="occurrence"){
    DwC.type<-"occurrence"
    DwcType <- "Occurrence"
    DwCLib<-"DwC_Occurrence"
  }else if(tolower(DwC.type)=="emof"){
    DwC.type<-"emof"
    DwcType <- "eMoF"
    DwCLib<-"DwC_eMoF"
    
    #emof needs to be formatted correctly, too complex to do it automatically with unkown input
    cond1 <- "eventID" %in% colnames(dataset)
    cond2 <- "measurementValueID" %in% colnames(dataset)
    cond3 <- "measurementUnitID" %in% colnames(dataset)
    cond4 <- "measurementTypeID" %in% colnames(dataset)
    
    if(!all(c(cond1, cond2, cond3, cond4))){
      stop("the eMoF file is not correctly formatted. expected eventID, measurementValueID, measurementUnitID and measurementTypeID")
    } else{
      ask.input <- FALSE
    }
  }else{
    stop("invalid input for DwC.type")
  }
  
  # use NA for missing data
  dataset[dataset==""]<-NA
  
  # remove rows with all NA values
  dataset <- dataset[rowSums(is.na(dataset))<ncol(dataset),]
  
  # check colunmnames
  termsQC <- dataQC.TermsCheck(observed=colnames(dataset),
                               exp.standard = "DwC", exp.section = DwC.type,
                               fuzzy.match = TRUE, out.type = "full")
  
  # run over fuzzy matches with possible solution
  if(length(termsQC$terms_wrongWithSolution)>0 & ask.input){
    for(tm in names(termsQC$terms_wrongWithSolution)){
      #note: in terms_wrongWithSolution: names= observed term, value is most likely match from standard
      tm_obs <- tm
      tm_match <- unname((termsQC$terms_wrongWithSolution)[tm])
      message(paste("The column name \"", tm_obs,"\" is not a DarwinCore ", DwC.type, " term...\n", sep=""))
      message(paste("\tdid you mean \"", tm_match,"\"? (y/n)\n", sep=""))
      doNext <- tolower(readLines(con = getOption("mypkg.connection"), n=1))
      if(doNext %in% c("y", "yes")){
        colnames(dataset)[colnames(dataset)==tm_obs] <- tm_match
      }else if(!doNext %in% c("n", "no")){
        stop("invalid input, expected n or y")
      }
    }
  }
  
  
  # run over non-matched terms
  if(length(termsQC$terms_notFound)>0 & ask.input){
    for(tm in termsQC$terms_notFound){
      if(DwcType %in% c("Event", "Occurrence")){
        message(paste("The column name \"", tm,"\" is not a DarwinCore ", DwC.type, " term...\nPlease choose what to do:\n", sep=""))
        if(DwcType=="Event"){
          message(paste("\t1) drop the term\n\t2) add to dynamicProperties\n\t3) add to eventRemarks\n\t4) add to fieldNotes\n", sep=""))
        }else if(DwcType=="Occurrence"){
          message(paste("\t1) drop the term\n\t2) add to dynamicProperties\n\t3) add to eventRemarks\n\t4) add to fieldNotes",
                        "\n\t5) add to identificationRemarks\n\t6) add to taxonRemarks\n\t7) add to measurementRemarks\n\t8) add to occurrenceRemarks\n", sep=""))
        }
        doNext <- tolower(readLines(con = getOption("mypkg.connection"), n=1))
      }
      
      if(doNext == 1){
        # drop the term
        dataset <- dataset[,!colnames(dataset) %in% tm]
      }else if(doNext == 2){
        #add to dynamicProperties
        dynProp <- as.character(dataset[,tm])
        dynProp <- sapply(dynProp, function(x){
          if(x!="" & !is.na(x)){
            x<-paste("{\"", tm, "\":", x, "}", sep="")
          }else{x}
        })
        dynProp<-unname(dynProp)
        if("dynamicProperties" %in% colnames(dataset)){
          dynProp <- paste(dataset$dynamicProperties, dynProp, sep=", ")
          dynProp <- sapply(dynProp, function(x){
            gsub(", $", "", x, fixed=FALSE)
          })
        }
        dataset$dynamicProperties <- dynProp
      }else if(doNext %in% 3:8){
        if(doNext == 3){
          #add to eventRemark
          doName <-"eventRemark"
        }else if(doNext == 4){
          #add to fieldNotes
          doName <-"fieldNotes"
        }else if(doNext == 5){
          #add to identificationRemarks
          doName <-"identificationRemarks"
        }else if(doNext == 6){
          #add to taxonRemarks
          doName <-"taxonRemarks"
        }else if(doNext == 7){
          #add to measurementRemarks
          doName <-"measurementRemarks"
        }else if(doNext == 8){
          #add to occurrenceRemarks
          doName <-"occurrenceRemarks"
        }
        remarks <- as.character(dataset[,tm])
        remarks <- sapply(remarks, function(x){
          if(x!="" & !is.na(x)){
            x<-paste(tm, ":", x, sep="")
          }else{x}
        })
        remarks<-unname(remarks)
        
        if(doName %in% colnames(dataset)){
          remarks <- paste(dataset[,doName], remarks, sep=", ")
          remarks <- sapply(remarks, function(x){
            gsub(", $", "", x, fixed=FALSE)
          })
        }
        dataset[,doName] <- remarks
      }else{
        if(DwcType=="Event"){
          stop("invalid input, expected 1, 2, 3 or 4")
        }else if(DwcType=="Occurrence"){
          stop("invalid input, expected 1, 2, 3, ... or 8")
        }
      }
    }
  }
  
  # check presence of required terms
  req_terms <- as.character(TermsLib[TermsLib[,DwCLib]==2,]$name)
  if(!all(req_terms %in% colnames(dataset))){
    terms_short <- setdiff(req_terms, colnames(dataset))
    warningmessages <- multi.warnings(paste("Please provide data for the following required DwC terms that were not found:\n\t", paste(terms_short, collapse=","), sep=""), warningmessages)
    for(tm in terms_short){
      dataset[,tm]<-NA #""
    }
    ## user input for basisOfRecord
    if("basisOfRecord" %in% terms_short & ask.input){
      message("No basisOfRecord found. Please specify if the data is:\n\t1)HumanObservation\n\t2)MachineObservation\n\t3)LivingSpecimen\n\t4)PreservedSpecimen\n\t5)FossilSpecimen")
      doNext <- tolower(readLines(con = getOption("mypkg.connection"), n=1))
      if(doNext == 1){
        dataset$basisOfRecord <- rep("HumanObservation", nrow(dataset))
      }else if(doNext == 2){
        dataset$basisOfRecord <- rep("MachineObservation", nrow(dataset))
      }else if(doNext == 3){
        dataset$basisOfRecord <- rep("LivingSpecimen", nrow(dataset))
      }else if(doNext == 4){
        dataset$basisOfRecord <- rep("PreservedSpecimen", nrow(dataset))
      }else if(doNext == 5){
        dataset$basisOfRecord <- rep("FossilSpecimen", nrow(dataset))
      }else{
        stop("invalid input, expected 1, 2, 3, 4 or 5")
      }
    }
    ## user input for eventID
    if("eventID" %in% terms_short & ask.input){
      message("No eventID found.\n\tPlease type an eventID prefix, which will be used to generate a unique ID per event")
      doNext <- readLines(con = getOption("mypkg.connection"), n=1)
      if(doNext == ""){
        prefix<-"event_"
      }else{
        prefix<-paste(doNext, "_", sep="")
      }
      dataset$eventID <- paste(prefix,
                                stringr::str_pad(1:nrow(dataset), nchar(nrow(dataset)), pad = "0"),
                                sep="")
    }
    ## user input for occurrenceID
    if("occurrenceID" %in% terms_short & ask.input){
      message("No occurrenceID found.\n\tPlease type an occurrenceID prefix, which will be used to generate a unique ID per event")
      doNext <- readLines(con = getOption("mypkg.connection"), n=1)
      if(doNext == ""){
        prefix<-"occ_"
      }else{
        prefix<-paste(doNext, "_", sep="")
      }
      dataset$occurrenceID <- paste(prefix,
                                     stringr::str_pad(1:nrow(dataset), nchar(nrow(dataset)), pad = "0"),
                                     sep="")
    }
  }
  
  # some other checks
  if(!"footprintWKT" %in% colnames(dataset) &
     complete.data &
     DwC.type == "Event" &
     "decimalLatitude" %in% colnames(dataset) &
     "decimalLongitude" %in% colnames(dataset)){
    dataset$footprintWKT <- dataQC.generate.footprintWKT(dataset)
    warningmessages <- multi.warnings("added footprintWKT", warningmessages)
  }
  #if("dynamicProperties" %in% colnames(dataset)){
  #  structure dynamicProperties
  #}
  
  # check latitude-longitude
  
  
  # dealing with the collection date, and putting it in the YYYY-MM-DD format
  if(!"eventDate" %in% colnames(dataset)){
    if("year" %in% colnames(dataset) && !any(grepl("/", dataset$year))){
      dataset$eventDate <- dataset$year
      if("month" %in% colnames(dataset)){
        for(i in 1:nrow(dataset)){
          if(dataset[i,]$eventDate!="" &
             !is.na(dataset[i,]$eventDate)){
            if(dataset[i,]$month!="" &
               !is.na(dataset[i,]$month)){
              dataset[i,]$eventDate <- paste(dataset[i,]$eventDate, dataset[i,]$month, sep="-")
            }
          }else{
            dataset[i,]$eventDate <- NA
          }
        }
        if("day" %in% colnames(dataset)){
          for(i in 1:nrow(dataset)){
            if(dataset[i,]$eventDate!="" & !is.na(dataset[i,]$eventDate)){
              if(dataset[i,]$day!="" &
                 !is.na(dataset[i,]$day)){
                dataset[i,]$eventDate <- paste(dataset[i,]$eventDate, dataset[i,]$day, sep="-")
              }
            }
          }
        }
      }
    }
  }
  
  if("eventDate" %in% colnames(dataset)){
    QCDate <- dataQC.dateCheck(dataset, "eventDate")
    warningmessages<-c(QCDate$warningmessages, warningmessages)
    if(length(QCDate$values)==nrow(dataset)){
      dataset$eventDate <- QCDate$values
    }
  }

  
  #check taxa and look for scientificNameID
  if(DwC.type == "occurrence" & complete.data){
    # try to get the species names
    species <- dataQC.TaxonListFromData(dataset)
    
    species <- dataQC.taxaNames(species)
    
    # make a small table with the unique taxa to collect all the info from WORMS or GBIF
    message("Completing the taxonomic information (this might take a while)...\n")
    taxid_key <- dataQC.completeTaxaNamesFromRegistery(species$scientificName)
    
    # now use the taxid_key table to complete the data in dataset
    for(term in setdiff(colnames(taxid_key), c("aphID"))){
      dataset[,term]  <- as.character(species$scientificName)
      dataset[,term] <- unname(unlist(sapply(as.character(dataset[,term]),
                                              FUN = function(x){
                                                gsub(x,taxid_key[taxid_key$scientificName==x,][,term],x)
                                              })))
    }
    warningmessages <- multi.warnings("added scientificNameID and additional species data", warningmessages)
  }
  
  # return the input file with the executed adjustments
  return(dataset)
}

