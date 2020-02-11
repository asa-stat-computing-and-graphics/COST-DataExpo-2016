# Change this to wherever you've cloned the repo
base_dir <- "~/TaxonomyOfBusCrashes-/"
# Change this to wherever you've downloaded the GES data
data_base_dir <- "~/TaxonomyOfBusCrashes-/data/"

######################## Don't change anything below this line ################

# Extraction from raw data
extracted_data_dir <- file.path(base_dir, "intermediate")
dir.create(extracted_data_dir)

yrs <- c("05","06","07","08","09","10","11","12","13","14","15")
base_dir_backup <- base_dir
for (i in seq_along(yrs)) {
  print(paste0("Extracting data for year: ", yrs[i]))
  source(paste0(base_dir, "data_", yrs[i], "_extract.R"))
}

source(file.path(base_dir, "combine_05_09.R"))
source(file.path(base_dir, "combine_10_15.R"))
source(file.path(base_dir, "combine_to_reduced_05_09.R"))
source(file.path(base_dir, "combine_to_reduced_10_15.R"))

# Run the analysis and generate output
source(file.path(base_dir, "master.R"))
