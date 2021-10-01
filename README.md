# README

##The OmicsMetaData R-package

## Badges


## Package details
### Description
This R-package focuses on handling metadata and environmental data of 'omics datasets in a standardized way, paying special attention to data interoperability and re-use. Therefore we implement the Minimum Information about any (x) sequence (MIxS) and the DarwinCore (DwC) standards, which are the most widely used in the 'omics and biodiversity data communities. 

There are functions to retrieve such data from sequences stored at the INSDC databases together with the sequences themselves and tools that can help with some basic quality control on these metadata (e.g. violations to the data standards). User-provided data can be standardized with MIxS or DwC in a supervised way, and can be readied for upload and archiving on one of the INSDC databases. Finally, there are several tools for common manipulations in ecological molecular data, such as easy conversion between wide and long formats.


## Installation instructions
  - install with devtools

 > install.packages("devtools")
 > library(devtools)

when the GitHub repo was downloaded to the local machine:
 > devtools::install(_path_to_package_)

install directly from GutHub

 > devtools::install_github("https://github.com/biodiversity-aq/OmicsMetaData")


## Additional setup requirements
  - Some functions require a personal API key to access the INSDC databases. Such an API key can be obtained from NCBI by first registering an NCBI account at https://account.ncbi.nlm.nih.gov/signup/?back_url=, then go to the “Settings” page of your NCBI account, and go to the “API Key Management” area. Click the “Create an API Key” button, and copy the resulting key.


## Resources
  - This package connects to the API of INSDC (via NCBI) to download sequence data and associated metadata from the world's leading nucleotide sequence databases. INSDC is a consortium of European, American and Japanese interconnected databases that house all sorts of nucleotide sequence data. More information on the API client of NCBI can be found here: https://www.ncbi.nlm.nih.gov/home/develop/api/

  - This packages also contains function that wrap and integrates several worrms package functions to obtain taxonomy information from the WoRMS database in a better structured way. The WoRMS (World Registry of Marine Species) is a LifeWatch database that houses the (near) complete taxonomic backbone of Marine and Antarctic (marine and terrestrial) species. In the database, each taxonomic unit, whether it is a species, genus or phylum, is identified by a unique ID (aphID) that is unchainable, even when taxonomic names are updated and changed. The aphID helps to identify synonym names and older names that are no longer in use. As such, each taxon is unambiguously identifiable, now and in the future as well. Extensive documentation can be found at this link: http://www.marinespecies.org/about.php.

  - This package used the Minimum on Any (x) Sequence (MIxS) standard, developed by the Genomics Standards Consortium (GSC). This is a data standard for nucleotide sequence data and it's metadata. More information can be found here: https://gensc.org/mixs/. Development of the latest version can be found here: https://github.com/GenomicsStandardsConsortium/mixs

  - This package also uses the DarwinCore data standard. DarwinCore is a data standard for biodiversity occurrence data, and is developed and maintained by TDWG. More information can be found here: https://dwc.tdwg.org


##Brief demonstration usage

 > library(OmicsMetaData)
 > OmicsMetaData.help()
 > 
 > dataset <- get.BioProject.metadata.INSDC("PRJNA305344")
 > dataset.MIxS <- dataQC.MIxS(dataset)

##Citation information
Please cite as:
Sweetlove M. 2021, OmicsMetaData: an R package to download, format and standardize genomic meta- and environmental data.

##Code of conduct
Please note that this package is released with a [Contributor
Code of Conduct](https://ropensci.org/code-of-conduct/). 
By contributing to this project, you agree to abide by its terms.


