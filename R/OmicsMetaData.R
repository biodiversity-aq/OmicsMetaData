#==============================================================
# The OmicsMetaData package
#       OmicsMetaData is a collection data management tools for microbial 'omics datasets.
#       They allow to download, structure, quqlity-controll and standardize microbial datasets
#==============================================================
# Author Maxime Sweetlove
# lisence CC-0
# Part of the POLA3R website (successor or mARS.biodiversity.aq)
# version 1.0 (2020-01-28)
# file encdong UTF-8
#
#==============================================================

#' OmicsMetaData: A package with tools to format and standardize microbial 'omics datasets.
#' 
#' This package provides 5 cathegories of tools, these are:
#' formating functions, standardization functions, quality control functions, data archiving functions and downloading data functions
#' In addition, there are also different libraries with terms of the MIxS and DarwinCore standards, term variants, synonyms and translations.
#' 
#' @section Classes:
#' MIxS.metadata  --  data formated in the MIxS standard
#' DwC.event --  data formated in DarwinCore (DwC) with event core
#' DwC.occurrence --  data formated in DwC with occurrence core
#' 
#' @section Methods for classes:
#' write.MIxS  --  write MIxS.metadata to a CSV file
#' check.valid.metadata.DwC  --  validator function for the DwC.event and DwC.occurrence classes
#' check.valid.MIxS.metadata  --  validator function for the MIxS.metadata class
#' 
#' @section Libraries:
#' TermsLib  --  central library with mapped terms of DwC, MIxS and miscellaneous missing terms
#' TermsSyn  --  library with synonyms for standard terms
#' TermsSyn_DwC  --  library with synonyms for DwC terms
#' TaxIDLib  --  non-exaustive library with some common INSDC taxon IDs
#' ENA_allowed_terms  --  terms accepted by ENA-EMBL
#' ENA_checklistAccession  --  checklist accessions accepted by ENA-EMBL
#' ENA_geoloc  --  geographic locations names accepted by ENA-EMBL
#' ENA_instrument --  instrument names accepted by ENA-EMBL
#' 
#' @section general functions:
#' term.definition  --  get the definition of a term
#' combine.data  --  combine data
#' combine.data.frame  --  combine different dataframes
#' commonTax.to.NCBI.TaxID   --  get the NCBI taxID of a taxon
#' coordinate.to.decimal  --  convert any coordinate to decimal coordinates
#' eMoF.to.wideTable  --  convert DwC eMoF (long format) to a wide table
#' wideTable.to.eMoF  --  convert wide table to a long table formatted as eMoF
#' find.dataset  --  find a column in a dataframe
#' get.boundingBox  --  make a bounding box from coordinates
#' multi.warnings  --  collect warning messages while a function runs
#'
#' @section Quality Control functions:
#' dataQC.completeTaxaNamesFromRegistery  --  get taxonomic information from WoRMS
#' dataQC.dateCheck  --  standardize dates to ISO
#' dataQC.DwC  --  automated check converting into DwC
#' dataQC.DwC_general  --  automated check converting into DwC
#' dataQC.eventStructure  --  create an hierarchical event structure
#' dataQC.findNames  --  detect sample names in a file
#' dataQC.generate.footprintWKT  --  generate a WKT from coordinates
#' dataQC.guess.env_package.from.data  --  guess the MIxS environmental package
#' dataQC.LatitudeLongitudeCheck  --  automated check for coordinates
#' dataQC.MIxS  --  automated check converting into MIxS
#' dataQC.taxaNames  --  clean out taxonomic names
#' dataQC.TaxonListFromData  --  find taxonomic names for samples
#' dataQC.TermsCheck  --  map a set of strings to standardized accepted terms
#'
#' @section upload-download omics data functions:
#' download.sequences.INSDC  --  download sequences from INSDC to R
#' FileNames.to.Table  --  get file names from a folder
#' get.insertSize  --  get the sequence lengths from a file
#' get.BioProject.metadata.INSDC  --  get metadata from INSDC (no API account required)
#' get.ENAName  --  get the ENA-EMBL variant of a MIxS term
#' get.sample.attributes.INSDC  --  get full list of metadata from INSDC (API account required)
#' prep.metadata.ENA  --  format metadata compliant to ENA-EMBL requirements
#' rename.sequenceFiles  --  automated renaming sequence files
#' sync.metadata.sequenceFiles  --  check is samples in R correspond to sequence files
#'
#' @docType package
#' @name OmicsMetaData
NULL

#' function to navigate through package
#' @export
OmicsMetaData.help <- function(){
  message(paste(sep="\n",
  "OmicsMetaData man pages:",
  "OmicsMetaData: A package with tools to format and standardize microbial omics datasets.",
  "",
  "Classes:",
  "MIxS.metadata  --  data formated in the MIxS standard",
  "DwC.event --  data formated in DarwinCore (DwC) with event core",
  "DwC.occurrence --  data formated in DwC with occurrence core",
  "",
  "Methods:",
  "write.MIxS  --  write MIxS.metadata to a CSV file",
  "check.valid.metadata.DwC  --  validator function for the DwC.event and DwC.occurrence classes",
  "check.valid.metadata.MIxS  --  validator function for the MIxS.metadata class",
  "",
  "Libraries:",
  "TermsLib  --  central library with mapped terms of DwC, MIxS and miscellaneous missing terms",
  "TermsSyn  --  library with synonyms for standard terms",
  "TermsSyn_DwC  --  library with synonyms for DwC terms",
  "TaxIDLib  --  non-exaustive library with some common INSDC taxon IDs",
  "ENA_allowed_terms  --  terms accepted by ENA-EMBL",
  "ENA_checklistAccession  --  checklist accessions accepted by ENA-EMBL",
  "ENA_geoloc  --  geographic locations names accepted by ENA-EMBL",
  "ENA_instrument --  instrument names accepted by ENA-EMBL",
  "",
  "general functions:",
  "term.definition  --  get the definition of a term",
  "combine.data  --  combine data",
  "combine.data.frame  --  combine different dataframes",
  "commonTax.to.NCBI.TaxID   --  get the NCBI taxID of a taxon",
  "coordinate.to.decimal  --  convert any coordinate to decimal coordinates",
  "eMoF.to.wideTable  --  convert DwC eMoF (long format) to a wide table",
  "wideTable.to.eMoF  --  convert wide table to a long table formatted as eMoF",
  "wideTab.to.hierarchicalTab  --  convert a wide table to a hierarchical table",
  "find.dataset  --  find a column in a dataframe",
  "get.boundingBox  --  make a bounding box from coordinates",
  "multi.warnings  --  collect warning messages while a function runs",
  "",
  "Quality Control functions:",
  "dataQC.completeTaxaNamesFromRegistery  --  get taxonomic information from WoRMS",
  "dataQC.dateCheck  --  standardize dates to ISO",
  "dataQC.DwC  --  automated check converting into DwC",
  "dataQC.DwC_general  --  automated check converting into DwC",
  "dataQC.eventStructure  --  create an hierarchical event structure",
  "dataQC.findNames  --  detect sample names in a file",
  "dataQC.generate.footprintWKT  --  generate a WKT from coordinates",
  "dataQC.guess.env_package.from.data  --  guess the MIxS environmental package",
  "dataQC.LatitudeLongitudeCheck  --  automated check for coordinates",
  "dataQC.MIxS  --  automated check converting into MIxS",
  "dataQC.taxaNames  --  clean out taxonomic names",
  "dataQC.TaxonListFromData  --  find taxonomic names for samples",
  "dataQC.TermsCheck  --  map a set of strings to standardized accepted terms",
  "",
  "upload-download omics data functions:",
  "download.sequences.INSDC  --  download sequences from INSDC to R",
  "FileNames.to.Table  --  get file names from a folder",
  "get.insertSize  --  get the sequence lengths from a file",
  "get.BioProject.metadata.INSDC  --  get metadata from INSDC (no API account required)",
  "get.ENAName  --  get the ENA-EMBL variant of a MIxS term",
  "get.sample.attributes.INSDC  --  get full list of metadata from INSDC (API account required)",
  "prep.metadata.ENA  --  format metadata compliant to ENA-EMBL requirements",
  "rename.sequenceFiles  --  automated renaming sequence files",
  "sync.metadata.sequenceFiles  --  check is samples in R correspond to sequence files"
  ))
}

#' Find the MIxS or DarwinCore standard term and definition of a variable
#' @author Maxime Sweetlove CC-0 2019
#' @description retrieve the MIxS or DarwinCore standard term and definition of a variable.
#' @param term a character string. The variable to look for among the MIxS and DarwinCore vocabularies
#' @details Standerdizing microbial sequence data, metadata and environmental data can be quite difficult given the plethora of standard terms already in existance. This function returns a definition of any term that is used on the POLAAAR portal at biodiversity.aq.
#' @return chracater string printed to the console. The best matching terms and their the definitions.
#' @export
term.definition <- function(term){
  if(term %in% TermsLib$name){
    def_out <- as.character(TermsLib[TermsLib$name==term,]$definition)
    out_message <- paste(term, "\t\n", def_out)
  }else{
    potential_match <- c()
    def_out <- c()
    for(ls in TermsSyn){
      if(grepl(term, ls)){
        lst <- names(TermsSyn[ls[1]])
        potential_match <- c(potential_match, lst)
        def_out <- c(def_out, as.character(TermsLib[TermsLib$name==lst,]$definition))
      }
    }
    
    if(length(potential_match)>1){
      out_message <- paste("Multiple matches found for \"" , term, "\"\n\n",
                           "\t", paste(c(1:length(potential_match)),
                                       rep(". ",length(potential_match)),
                                       potential_match,
                                       rep(" :\n",length(potential_match)),
                                       rep("\"",length(potential_match)),
                                       def_out,
                                       rep("\"",length(potential_match)),
                                       collapse="\n\t", sep=""), sep="")
    }else if(length(potential_match)==1){
      out_message <- paste("Best match for \"" , term, "\"\n\n",
                           potential_match, "\n", def_out, sep="")
    }else{
      out_message <-  paste("Could not find any matches for \"" , term, "\"\n",sep="")
    }
  }
  message(out_message)
  
}

# documentation for the data objects

#' central library with mapped terms of DwC, MIxS and miscellaneous missing terms
#' 
#' @author Maxime Sweetlove CC-0 2021
#' @name TermsLib
#' @docType data
#' @references \url{https://gensc.org/mixs/} \url{https://dwc.tdwg.org}
#' @keywords data, MIxS, DarwinCore
NULL

#' library with synonyms for standard terms
#' 
#' @author Maxime Sweetlove CC-0 2021
#' @name TermsSyn
#' @docType data
#' @references \url{https://gensc.org/mixs/} \url{https://dwc.tdwg.org}
#' @keywords data, MIxS, DarwinCore, INSDC, ENA, NCBI, POLA3R
NULL

#' library with synonyms for DwC terms
#' 
#' @author Maxime Sweetlove CC-0 2021
#' @name TermsSyn_DwC
#' @docType data
#' @references \url{https://dwc.tdwg.org}
#' @keywords data, DarwinCore
NULL

#' non-exaustive library with some common INSDC taxon IDs
#' 
#' @author Maxime Sweetlove CC-0 2021
#' @name TaxIDLib
#' @docType data
#' @references \url{https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi}
#' @keywords data, INSDC, NCBI, taxonomy
NULL

#' terms accepted by ENA-EMBL
#' 
#' @author Maxime Sweetlove CC-0 2021
#' @name ENA_allowed_terms
#' @docType data
#' @references \url{https://www.ebi.ac.uk/ena/browser/home}
#' @keywords data, INSDC, ENA
NULL

#' checklist accessions accepted by ENA-EMBL
#' 
#' @author Maxime Sweetlove CC-0 2021
#' @name ENA_checklistAccession
#' @docType data
#' @references \url{https://www.ebi.ac.uk/ena/browser/home}
#' @keywords data, INSDC, ENA
NULL

#' geographic locations names accepted by ENA-EMBL
#' 
#' @author Maxime Sweetlove CC-0 2021
#' @name ENA_geoloc
#' @docType data
#' @references \url{https://www.ebi.ac.uk/ena/browser/home}
#' @keywords data, INSDC, ENA
NULL

#' instrument names accepted by ENA-EMBL
#' 
#' @author Maxime Sweetlove CC-0 2021
#' @name ENA_instrument
#' @docType data
#' @references \url{https://www.ebi.ac.uk/ena/browser/home}
#' @keywords data, INSDC, ENA
NULL

#' experiment names accepted by ENA-EMBL
#' 
#' @author Maxime Sweetlove CC-0 2021
#' @name ENA_select
#' @docType data
#' @references \url{https://www.ebi.ac.uk/ena/browser/home}
#' @keywords data, INSDC, ENA
NULL

#' sequencing method names accepted by ENA-EMBL
#' 
#' @author Maxime Sweetlove CC-0 2021
#' @name ENA_strat
#' @docType data
#' @references \url{https://www.ebi.ac.uk/ena/browser/home}
#' @keywords data, INSDC, ENA
NULL

