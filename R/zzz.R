#==============================================================
# Author Maxime Sweetlove
# Part of the POLA3R website (successor or mARS.biodiversity.aq)
# version 1.0 (2021-09-21)
# file encdong UTF-8
#
#==============================================================
# default options for ReadLines in function
#==============================================================
# here the default option for the ReadLines function is set to stdin()
#the reason for this is so that the user input can be manipulated during the unittests
#see https://stackoverflow.com/questions/41372146/test-interaction-with-users-in-r-package

.onAttach <- function(libname, pkgname){
  options(mypkg.connection = stdin())
}
