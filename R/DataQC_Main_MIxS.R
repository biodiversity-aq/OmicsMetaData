#==============================================================
# Author Maxime Sweetlove
# lisence CC 4.0
# Part of the POLA3R website (successor or mARS.biodiversity.aq)
# version 1.0 (2020-01-28)
# file encdong UTF-8
#
# assumptions:
#    - use NA for missing values
#
#==============================================================
# data Quality Controll (QC) for MIxS
#==============================================================

#' format dataframes into a MIxS object
#' @author Maxime Sweetlove ccBY 4.0 2019
#' @family standardization functions
#' @description takes a dataframe with contextual data and metadata from a sequencing dataset and performs a basis Quality Controll. (see details)
#' @usage dataQC.MIxS(dataset = NA, ask.input=TRUE, add_to = NA, sample.names = NA)
#' @param dataset data.frame. The raw dataset downloaded from INSDC to be cleaned up. Rows are samples, columns variables. Units can be listed in the first or second row, and will be automatically detected if the row names of this row includes the word "units". Different units per sample are not allowed.
#' @param ask.input logical. If TRUE, console input will be requested to the user when a problem occurs (process runs user-supervised). Default TRUE
#' @param add_to a MIxS.metadata object. An already present dataset with quality-controlled metadata. must be formatted as MIxS.metadata to ensure the correct input format of the data.
#' @param sample.names character. The column with sample names to use. Use row.names for rownames. If NA the function will try to find sample names itself. default NA
#' @details Any sequencing project typically has important additional data associated with it. This goes from laboratory protocols, sequencing platform settings or environmental measurements. Thisfunction was develloped to sort through these metadata (provided in a dataframe), and perform a basic quality controll, correcting the most common mistakes, like incorrectly formatting the geographic coordinates, formatting dates, typos or variants of variable names, etc. To do this, the function makes use of a build-in dictionary of (MIxS) terms and their synonyms (that is: spelling errors, writing differences, true synonyms,...). Note that it is possible some terms are not recognized. In that case contact the author of the package to update the dictionary in the upcomming version.
#' @seealso get.BioProject.metadata.INSDC, get.sample.attributes.INSDC
#' @return a MIxS.metadata object that is compatible with the MIxS standard
#' @export
dataQC.MIxS <- function(dataset = NA, ask.input=TRUE, add_to = NA, sample.names = NA){
  warningmessages<-c()

  # 0. pre-process input
  # 0.1. check input data
  if(!is.data.frame(dataset)){
    stop("The input must be a dataframe, with samples as rows/ variables as columns")
  }
  if(!is.na(add_to) && !check.valid.MIxS.metadata(add_to)){
    stop("The input for the add_to argument must be a MIxS.metadata object to ensure correct merging of the datasets")
  }

  # 0.2 formatting
  # remove all factors
  dataset[] <- lapply(dataset, as.character)
  # remove empty columns
  dataset[dataset == ""] <-NA #set empty cells to NA
  dataset <- dataset[,colSums(is.na(dataset)) < nrow(dataset)]

  # clean up columnames
  colnames(dataset) <- gsub("[\\.]+", "_", colnames(dataset)) # replace double dots with underscore
  colnames(dataset) <- gsub("_$", "", colnames(dataset))  # remove trailing underscore

  # remove rows with all NA values
  dataset <- dataset[rowSums(is.na(dataset))<ncol(dataset),]
  
  # 0.3 check if data is in one-header table or if there are additional MiMARKS header lines
  # for additional MIxS headers: "environmental package", "units template" => only units of importance
  pre_def_units <- FALSE
  if(grepl("unit", tolower(row.names(dataset)[1]))){
    units <- data.frame(var_name=c(colnames(dataset)), unit= unlist(dataset[1,]), stringsAsFactors = FALSE)
    dataset <- data.frame(dataset[-1,], stringsAsFactors = FALSE)
    pre_def_units <- TRUE
    warningmessages <- multi.warnings("the units were taken from the row \"units template\" in the input data", warningmessages)
  } else if(grepl("unit", tolower(row.names(dataset)[2]))){
    units <- data.frame(var_name=c(colnames(dataset)), unit= unlist(dataset[2,]), stringsAsFactors = FALSE)
    dataset <- data.frame(dataset[-c(1,2),], stringsAsFactors = FALSE)
    pre_def_units <- TRUE
    warningmessages <- multi.warnings("the units were taken from the row \"units template\" in the input data", warningmessages)
  }
  
  # remove columns that were all NA
  dataset <- dataset[,colSums(is.na(dataset)) < nrow(dataset)]
  if(pre_def_units){#also drop removed columns from units
    units<-units[colnames(dataset),]
  }
  
  # check columnnames, and correct errors
  dataset_origcolNames <- dataset
  colnames(dataset) <- tolower(colnames(dataset)) # all to lowercase
  termsQC <- dataQC.TermsCheck(observed=colnames(dataset),
                               exp.standard = "MIxS", exp.section = NA,
                               fuzzy.match = FALSE, out.type = "full")
  for(tQC in names(termsQC$terms_wrongWithSolution)){
    if(!as.character(termsQC$terms_wrongWithSolution[tQC]) %in% colnames(dataset)){
      colnames(dataset)[colnames(dataset)==tQC] <- termsQC$terms_wrongWithSolution[tQC]
      if(pre_def_units){#also drop removed columns from units
        units[units$var_name==tQC,]$var_name <- termsQC$terms_wrongWithSolution[tQC]
        rownames(units)[rownames(units)==tQC] <- termsQC$terms_wrongWithSolution[tQC]
      }
    }
  }
  

  # 0.4 prepare output data:
  # make an empty output file to fill along the way
  New_dataset <- data.frame(row.names=rownames(dataset))

  # 1. looking for the original sample name
  datasetNames <- dataQC.findNames(dataset = dataset_origcolNames, ask.input=ask.input, sample.names=sample.names)
  New_dataset$original_name <- (datasetNames$Names)$original_name
  warningmessages <- multi.warnings(datasetNames$warningmessages, warningmessages)
  tryCatch({
    rownames(New_dataset) <- New_dataset$original_name
  },
  error=function(x){
    warningmessages <- multi.warnings("duplicate or missing original sample names", warningmessages)
  })
  if(!all((datasetNames$Names)$INSDC_SampleID=="") & !all(is.na((datasetNames$Names)$INSDC_SampleID))){
    New_dataset$INSDC_SampleID <- (datasetNames$Names)$INSDC_SampleID}
  if(!all((datasetNames$Names)$eventID=="") & !all(is.na((datasetNames$Names)$eventID))){
    New_dataset$eventID <- (datasetNames$Names)$eventID}
  if(!all((datasetNames$Names)$parentEventID=="") & !all(is.na((datasetNames$Names)$parentEventID))){
    New_dataset$parentEventID <- (datasetNames$Names)$parentEventID}
  if(!all((datasetNames$Names)$occurrenceID=="") & !all(is.na((datasetNames$Names)$occurrenceID))){
    New_dataset$occurrenceID <- (datasetNames$Names)$occurrenceID}
  
  # 2. some basic info from insdc
  TermsSyn_insdc<-TermsSyn[as.character(TermsLib[TermsLib$name_origin=="INSDC",]$name)]
  if("original_name" %in% colnames(New_dataset)){
    # if this column is present, do not change it.
    TermsSyn_insdc <- TermsSyn_insdc[!names(TermsSyn_insdc)=="original_name"]
  }
  for(item in names(TermsSyn_insdc)){
    item_shared <- intersect(TermsSyn_insdc[item][[1]], colnames(dataset))
    if(length(item_shared)==1){
      New_dataset[,item] <- dataset[,item_shared]
    }
  }

  # 3. dealing with latitude-longitude, and it's many possible formats...
  TermsSyn_latlon<-TermsSyn[as.character(TermsLib[TermsLib$name=="lat_lon",]$name)]
  TermsSyn_lat<-TermsSyn[as.character(TermsLib[TermsLib$name=="decimalLatitude",]$name)]
  TermsSyn_lon<-TermsSyn[as.character(TermsLib[TermsLib$name=="decimalLongitude",]$name)]

  QClatlon <- dataQC.LatitudeLongitudeCheck(dataset,
                                            latlon.colnames=list(TermsSyn_latlon[[1]],
                                                                 TermsSyn_lat[[1]],
                                                                 TermsSyn_lon[[1]]))
  warningmessages<-c(QClatlon$warningmessages, warningmessages)
  New_dataset$lat_lon <- QClatlon$values
  New_dataset$decimalLatitude <- sapply(New_dataset$lat_lon, function(x){strsplit(x, " ")[[1]][1]})
  New_dataset$decimalLongitude <- sapply(New_dataset$lat_lon, function(x){strsplit(x, " ")[[1]][2]})

  #change the units for the coordinates
  if(pre_def_units){
    for(unitx in c("lat_lon", "decimalLatitude", "decimalLongitude")){
      if(unitx %in% units$var_name){
        units[units$var_name==unitx,]$unit <- as.character(TermsLib[TermsLib$name==unitx,]$expected_unit)
      }else{
        units <- rbind(units, data.frame(var_name=unitx, unit=as.character(TermsLib[TermsLib$name==unitx,]$expected_unit)))
        rownames(units)[nrow(units)] <- unitx
      }
    }
  }


  # 4. dealing with the collection date, and putting it in the YYYY-MM-DD format
  TermsSyn_date<-TermsSyn[as.character(TermsLib[TermsLib$name=="collection_date",]$name)]
  QCDate <- dataQC.dateCheck(dataset, TermsSyn_date[[1]])
  warningmessages<-c(QCDate$warningmessages, warningmessages)
  if(length(QCDate$values)==nrow(dataset)){
    New_dataset$collection_date <- QCDate$values
  }
  
  # 5. the core MIxS terms
  TermsSyn_MIxS <- TermsSyn[as.character(TermsLib[TermsLib$MIxS_core>0,]$name)]
  TermsSyn_MIxS <- TermsSyn_MIxS[!names(TermsSyn_MIxS) %in% c("lat_lon", "collection_date")]
  for(item in names(TermsSyn_MIxS)){
    item_shared <- intersect(TermsSyn_MIxS[item][[1]], colnames(dataset))
    if(length(item_shared)==1){
      New_dataset[,item] <- dataset[,item_shared]
    } else if(length(item_shared)>1){
      New_dataset[,item] <- dataset[,item_shared[1]]
    }
  }

  # 6. the MIxS package terms
  # 6.1 find the best package/ask user if no package was specified
  env_package<-NA
  if(!"env_package" %in% colnames(dataset)){
    if(ask.input){
      message("No env_package was specified.\nPlease specify what to do next:\n1) Make an educated guess based on the data\n2) Ask user for the package name\n3) stop executing\n(type 1, 2 or 3)\n")
      doNext <- readline()
      if(doNext==1){
        env_package <- dataQC.guess.env_package.from.data(dataset)
        warningmessages<-c(warningmessages, env_package$warningmessages)
        env_package <- env_package$values
      }else if(doNext==2){
        message("Please provide a single MIxS environmental package\nThe choices are: air, built_environment, host_associated, human_associated,human_gut,\nhuman_oral, human_skin, human_vaginal,microbial_mat_biofilm,\nmiscellaneous_natural_or_artificial_environment,\nplant_associated, soil, sediment, wastewater_sludge, water\n")
        env_package <- readline()
        if(!env_package %in% colnames(TermsLib)){
          stop("incorrect environmental package provided. Be sure to use underscores and lowercase letters")
        } else{
          env_package <- rep(env_package, nrow(dataset))
        }
      }else if(doNext==3){
        stop("you chose to interrupt execution.")
      }else{
        stop("incorrect input. Interrupted execution.")
      }
    }else{
      env_package <- dataQC.guess.env_package.from.data(dataset)
      warningmessages<-c(warningmessages, env_package$warningmessages)
      env_package <- env_package$values
    }
  }else{# 6.2 check if package is valid or if the ENA checklist number needs to be converted to a package
    New_dataset$env_package <- dataset$env_package
    if(length(setdiff(unique(New_dataset$env_package), ENA_checklistAccession$env_package))>0 |
       length(setdiff(unique(New_dataset$env_package), ENA_checklistAccession$ena_package))>0){
      #no correct package name, check if it is one of the ENA accession numbers
      if(sum(grepl("ERC", unique(New_dataset$env_package)))==length(unique(New_dataset$env_package))){
        #convert all the ENA checklist accession numbers to MIxS packages
        ENA_checklistAccession$ena_checklist_accession
        for(i in 1:nrow(New_dataset)){
          pk <- ENA_checklistAccession[ENA_checklistAccession$ena_checklist_accession == New_dataset[i,]$env_package,]$env_package
          if(length(pk)>0 && ENA_checklistAccession[ENA_checklistAccession$env_package==pk,]$MIxS==TRUE){
            New_dataset[i,]$env_package <- pk
          } else{
            New_dataset[i,]$env_package <- NA
            warningmessages<-multi.warnings("For some samples, the given environmental package did not correspond to a MIxS package", warningmessages)
          }
        }
      }else{
        env_package <- dataQC.guess.env_package.from.data(dataset)
        env_package <- env_package$values
      }
    }
  }

  if(all(is.na(env_package)) || all(nchar(env_package)==0)){
    warningmessages<-multi.warnings("No env_package could be inferred", warningmessages)
  } else{
    New_dataset$env_package <- env_package
  }

  TermsSyn_MIxSpackage<-TermsSyn[as.character(TermsLib[TermsLib$name_origin=="MIxS" & TermsLib$MIxS_core==0,]$name)]
  for(item in names(TermsSyn_MIxSpackage)){
    item_shared <- intersect(TermsSyn_MIxSpackage[item][[1]], colnames(dataset))
    if(length(item_shared)==1){
      New_dataset[,item] <- dataset[,item_shared]
    } else if(length(item_shared)>1){
      New_dataset[,item] <- dataset[,item_shared[[1]]]
    }
  }

  # 7. any other additionalinformation
  # 7.1 already registered terms
  TermsSyn_add <- TermsSyn[as.character(TermsLib[TermsLib$name_origin %in% c("DwC", "miscellaneous"),]$name)]
  TermsSyn_add <- TermsSyn_add[!names(TermsSyn_add) %in% c("decimalLatitude", "decimalLongitude")]
  for(item in names(TermsSyn_add)){
    item_shared <- intersect(TermsSyn_add[item][[1]], colnames(dataset))
    if(length(item_shared)==1){
      New_dataset[,item] <- dataset[,item_shared]
    }
  }
  # 7.2 novel terms
  unknown_termsLib <- data.frame(origcol=colnames(dataset_origcolNames), newcol=tolower(colnames(dataset_origcolNames)))
  unknown_termsLib <- unknown_termsLib[unknown_termsLib$newcol %in% setdiff(colnames(dataset), unlist(TermsSyn)),]
  unknown_termsLib <- unknown_termsLib[!unknown_termsLib$newcol %in% c("decimallatitude", "decimallongitude", "parenteventid"),]
  
  if(length(unknown_termsLib$newcol) > 0){
    if(ask.input){
      t<-paste(paste("\t", c(1:length(unknown_termsLib$origcol)), ": ", unknown_termsLib$origcol, sep=""), collapse="\n")
      message(paste("The following unknown variables were encountered:\n",t,
                    "\n\t\tType \"y\" to add all /",
                    #"\n\t\tGive a comma-separated vector with the numbers to add /",
                    "\n\t\tType \"n\" to add none", sep=""))
      ctu <- readline()
      if(ctu %in% c("y", "Y", "yes", "YES", "Yes")){
        for(t in 1:nrow(unknown_termsLib)){
          newcolnum <- ncol(New_dataset)+1
          New_dataset[,newcolnum] <- dataset[,colnames(dataset)==unknown_termsLib[t,]$newcol]
          colnames(New_dataset)[newcolnum] <- unknown_termsLib[t,]$origcol
        }
      }
    }else{
      warningmessages<-multi.warnings("Some unknown variables present in the data", warningmessages)
      for(t in 1:nrow(unknown_termsLib)){
        newcolnum <- ncol(New_dataset)+1
        New_dataset[,newcolnum] <- dataset[,colnames(dataset)==unknown_termsLib[t,]$newcol]
        colnames(New_dataset)[newcolnum] <- unknown_termsLib[t,]$origcol
      }
    }
  }

  # 8. some additional quality controll
  if("investigation_type" %in% colnames(New_dataset)){
    New_dataset$investigation_type <- sapply(New_dataset$investigation_type, FUN=function(x){
      if(x %in% c("AMPLICON", "amplicon", "metabarcode")){x<-"mimarks-survey"
      }else if(x %in% c("WGS", "Metagenome")){x<-"metagenome"}
      return(x)})
  }else{
    if(ask.input){
      message("No investigation_type was found...\n\tPlease provide an investigation_type. Common ones include mimarks-survey or metagenome. Type n to ignore.\n")
      invtype <- readline()
      if(! invtype %in% c("n", "N")){
        New_dataset$investigation_type <- rep(invtype, nrow(New_dataset))
      }
    }
  }
  if("target_gene" %in% colnames(New_dataset)){
    New_dataset$target_gene <- sapply(New_dataset$target_gene, function(x){
      if(grepl("16S", x)){x<-"16S ssu rRNA"
      }else if(grepl("18S", toupper(x))){x<-"18S ssu rRNA"
      }else if(grepl("ITS", toupper(x))){x<-"ITS"
      }else if(grepl("COI", toupper(x))){x<-"COI"
      }
      return(x)})
  }
  if("specific_host" %in% colnames(New_dataset)){
    host_val <- setdiff(unique(New_dataset$specific_host), c(NA, "NA", "-", "not applicable"))
    if(identical(host_val, character(0))){
      New_dataset <- New_dataset[,!colnames(New_dataset) %in% "specific_host"]
    }
  }

  #if("eventID" %in% colnames(New_dataset)){
  #  if(length(unique(New_dataset$eventID))==nrow(New_dataset)){
  #    message("There are the same number of events as there are samples...\n\tkeep $eventID? (y/n)\n")
  #    ctu <- readline()
  #    if(ctu %in% c("n", "N", "no", "NO", "No")){
  #      New_dataset <- New_dataset[,!colnames(New_dataset) %in% "eventID"]
  #    }
  #  }
  #}

  # QC on length/depth/size measurements
  # if multiple units found: do nothing
  # if a string found that is not one of the expected units: leave it and work with the rest
  size_related <- c("depth", "elev", "alt_elev", "filter_size", "tot_depth_water_col")
  items_shared <- intersect(size_related, colnames(New_dataset))
  QC_units<-c()
  if(length(items_shared)>0){
    alternative_units<-data.frame(name=items_shared, unit_full=rep(NA, length(items_shared)))
    possible_units <- c("nanometer", "micrometer", "milimeter", "centimeter", "decimeter", "meter", "kilometer")
    for(name_unit in items_shared){
      vals <- as.vector(as.character(New_dataset[,colnames(New_dataset) %in% name_unit]))
      names(vals)<-row.names(New_dataset)
      #extract the units
      if(pre_def_units){
        val_units <- as.character(units[units$var_name==name_unit,]$unit)
        val_units <- rep(val_units,nrow(New_dataset))
      }else{
        val_units <- unlist(lapply(vals, function(v){gsub("[0-9]|\\.|,|-", "", v)} ))
      }
      val_units <- gsub(" ", "", tolower(val_units))
      val_units <- gsub("^nm$", "nanometer", tolower(val_units), fixed=FALSE)
      val_units <- gsub("^um$", "micrometer", tolower(val_units), fixed=FALSE)
      val_units <- gsub("^mm$", "milimeter", tolower(val_units), fixed=FALSE)
      val_units <- gsub("^cm$", "centimeter", tolower(val_units), fixed=FALSE)
      val_units <- gsub("^dm$", "decimeter", tolower(val_units), fixed=FALSE)
      val_units <- gsub("^km$", "kilometer", tolower(val_units), fixed=FALSE)
      val_units <- gsub("^m$", "meter", tolower(val_units))
      val_units <- intersect(unique(val_units), possible_units) #danger in this step: discards any text that is not an expected unit

      vals<-unlist(lapply(vals, function(v){gsub(",", ".", v)} ))
      if(length(val_units)==1){
        for(v in 1:length(vals)){
          if(grepl(val_units[1], vals[v])){
            vals[v]<-gsub(val_units[1], NA, vals[v])
          }
        }
        QC_units[name_unit]<-val_units[1]
        New_dataset[,colnames(New_dataset)==name_unit] <- c(vals[rownames(New_dataset)])
      }

    }
  }

  # 9. finalizing and formatting the output
  # 9.1 the units
  New_dataset_units <- c()
  if(!pre_def_units){
    # there were no pre defined units in a additional header line
    for(i in 1:ncol(New_dataset)){
      unit_i <- as.character(TermsLib[TermsLib$name %in% colnames(New_dataset)[i],]$expected_unit)
      if(length(unit_i)>0){
        New_dataset_units[colnames(New_dataset)[i]] <- unit_i
      } else{
        # if unit completely unkown, put alphanumeric
        New_dataset_units[colnames(New_dataset)[i]] <- "alphanumeric"
      }
    }
  }else{
    # there were pre defined units in a additional header line
    # user defined units obviously have priority over assumed standard units in the TermsLib file
    for(i in 1:ncol(New_dataset)){
      if(colnames(New_dataset)[i] %in% units$var_name){
        predef_unit <- as.character(units[units$var_name==colnames(New_dataset)[i],]$unit)[1]
        if(is.na(predef_unit) | predef_unit=="" | predef_unit=="NA"){
          predef_unit <- "alphanumeric"
        }
        New_dataset_units[colnames(New_dataset)[i]] <- predef_unit
      } else{
        # if unit completely unkown, put alphanumeric
        New_dataset_units[colnames(New_dataset)[i]] <- "alphanumeric"
      }
    }
  }
  # QC_units
  if(length(QC_units)>0){
    for(u in 1:length(QC_units)){
      New_dataset_units[names(QC_units)[u]] <- QC_units[u]
    }
  }


  # 9.2 concatenate all non-MIxS terms in the misc_param term
  #MIxS_terms <- setdiff(as.character(TermsLib[TermsLib$official_MIxS==TRUE,]$name), "misc_param")
  #misc_units <- New_dataset_units[which(!colnames(New_dataset) %in% MIxS_terms)]
  #New_dataset_units <- New_dataset_units[which(colnames(New_dataset) %in% MIxS_terms)]
  #misc_dataset <- New_dataset[,!colnames(New_dataset) %in% MIxS_terms, drop=FALSE]
  #New_dataset <- New_dataset[,colnames(New_dataset) %in% MIxS_terms, drop=FALSE]
  #if(ncol(misc_dataset)>0){
  #  for(cl in 1:ncol(misc_dataset)){
  #    if(!grepl("alphanumeric", misc_units[cl])){
  #      un <- paste("(", misc_units[cl], ")", sep="")
  #    }else{
  #      un<-""
  #    }
  #    misc_dataset[,cl]<-paste(colnames(misc_dataset)[cl], ":", misc_dataset[,cl], un, sep="")
  #  }
  #  New_dataset$misc_param<-apply(misc_dataset, 1, function(x) paste(x, collapse = ";"))
  #  New_dataset_units["misc_param"] <- "alphanumeric"
  #}

  # 9.3 the section
  New_dataset_section <- c()
  for(i in 1:ncol(New_dataset)){
    if(colnames(New_dataset)[i] %in%TermsLib$name){
      New_dataset_section[colnames(New_dataset)[i]] <- as.character(TermsLib[TermsLib$name %in% colnames(New_dataset)[i],]$MIxS_section)
    } else{
      New_dataset_section[colnames(New_dataset)[i]] <- "miscellaneous"
    }
  }

  # 9.4 env_package
  if("env_package" %in% colnames(New_dataset)){
    env_package <- unique(New_dataset$env_package)
    if(length(env_package)>1){
      env_package <- "multiple_packages"
    } else if(is.na(env_package) | is.null(env_package)){
      env_package <- "not_specified"
    }
  } else{
    env_package <- "not_specified"
  }

  # 9.5 warning messages
  if(length(warningmessages)>0){
    for(i in 1:length(warningmessages)){
      warningmessages[i] <- paste(i, warningmessages[i], sep=". ")
    }
    warningmessages <- c("Please consider the following warning messages carefully before proceding:", warningmessages)
    warning(paste(warningmessages, collapse='\n'))
  }

  # 9.6 convert to the right output format (data.frame or MIxS.metadata)
  New_dataset <- new("MIxS.metadata",
                      data   = New_dataset,
                      section = New_dataset_section,
                      units      = New_dataset_units,
                      env_package = as.character(env_package),
                      type = "versatile",
                      QC = TRUE
  )
  if(!is.na(add_to)){
    New_dataset <- combine.data(New_dataset, add_to)
  }
  return(New_dataset)
}
