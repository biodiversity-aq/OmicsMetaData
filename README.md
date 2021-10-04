# README

##The OmicsMetaData R-package

## Badges


## Package details
### Description
This R-package provides a suite of tools to help format and standardize metadata and environmental data of 'omics datasets. The aim is to help improve data interoperability and re-use in biodiversity 'omics fields, such as metabarcoding (e.g. eDNA, amplicon sequencing of microbes) and metagenomics (e.g. shotgun meta genome sequencing). More specifically, this package offers the users tools to help them integrate online open access data and metadata into their analyses, and standardize their de novo data and metadata following the Minimum Information about any (x) sequence (MIxS) or the DarwinCore (DwC) standards to use it in integrated analyses with other data, or archive it on online nucleotide sequence data repositories (e.g. ENA, SRA, GenBank,..) in accordance to the FAIR principles.


Therefore, the tools in the package cover (1) data (DNA sequences) and metadata (any other linked information or measurements that describe the environment from where the DNA was extracted) retrieval from INSDC database, (2) basic quality control on metadata (i.e. detecting violations against the rules of the data standards and ensuring the minimal metadata is provided), and (3) format data following the MIxS or DarwinCore data standards, including the use of the vocabularies and adherence to the rules set out the the data standards. 


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
###International Nucleotide Sequence Database Consortium (INSDC)
This package connects to the API of INSDC (via NCBI) to download sequence data and associated metadata from the world's leading nucleotide sequence databases. INSDC is a consortium of European, American and Japanese interconnected databases that house all sorts of nucleotide sequence data. More information on the API client of NCBI can be found here: https://www.ncbi.nlm.nih.gov/home/develop/api/

###World Registry of Marine Species (WoRMS) and worrms
This packages also contains function that wrap and integrates several worrms package (Chamberlain, 2020; Horton et al. 2021) functions to obtain taxonomy information from the WoRMS database in a better structured way. The WoRMS (World Registry of Marine Species) is a LifeWatch database that houses the (near) complete taxonomic backbone of Marine and Antarctic (marine and terrestrial) species. In the database, each taxonomic unit, whether it is a species, genus or phylum, is identified by a unique ID (aphID) that is unchainable, even when taxonomic names are updated and changed. The aphID helps to identify synonym names and older names that are no longer in use. As such, each taxon is unambiguously identifiable, now and in the future as well. Extensive documentation can be found at this link: http://www.marinespecies.org/about.php.

###Minimum on Any (x) Sequence (MIxS)
This package used the Minimum on Any (x) Sequence (MIxS) standard, developed by the Genomics Standards Consortium (GSC) (Yilmaz et al. 2011). This is a data standard for nucleotide sequence data and it's metadata. More information can be found here: https://gensc.org/mixs/. Development of the latest version can be found here: https://github.com/GenomicsStandardsConsortium/mixs

###DarwinCore
This package also uses the DarwinCore data standard (Darwin Core Task Group, 2009). DarwinCore is a data standard for biodiversity occurrence data, and is developed and maintained by TDWG. More information can be found here: https://dwc.tdwg.org

###References
Yilmaz, P., Gilbert, J., Knight, R. et al. The genomic standards consortium: bringing standards to life for microbial ecology. ISME J 5, 1565–1567 (2011). https://doi.org/10.1038/ismej.2011.39

Darwin Core Task Group. 2009. Darwin Core. Biodiversity Information Standards (TDWG) http://www.tdwg.org/standards/450

Chamberlain, S. (2020). worrms: World Register of Marine Species (WoRMS) Client. R package version 0.4.2. https://CRAN.R-project.org/package=worrms

Horton, T., Kroh, A., et al. (2021). World Register of Marine Species. Available from https://www.marinespecies.org at VLIZ. doi:10.14284/170 



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

##lisence
This package is distributed under a GNU General Public License v3 (GLP-3.0) https://www.gnu.org/licenses/gpl-3.0.en.html



