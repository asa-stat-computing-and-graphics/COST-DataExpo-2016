#' Create .rda "master" file from .sas7bdat files
#'
#' @param dir the target directory for the output
#' @param desired.files a vector of .sas7bdat file names to merge together
#' @return A data frame of merged .sas7bdat files
#' @export
mergeGivenYear <- function(dir = getwd(), desired.files = c())
{
  old.dir <- setwd(dir)
  filenames <- list.files(pattern = "*.sas7bdat", full.names = TRUE)
  for (name in desired.files)
  {
    file <- sas7bdat::read.sas7bdat(name)
    name <- paste(name,".txt")
    write.table(file, name, sep="\t")
    file <- read.table(name)
    master.data <- merge(master.data, file)
    unlink(name) ##Delete the .txt file
    print(name)
  }
  setwd(old.dir)
  master.data
}
