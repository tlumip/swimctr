# This script builds the binary RData dataset stored in the data directory and
# distributed with the package. The user should never need to run this unless
# they fork and rebuild the package. It is assumed that the program is run from
# the R folder, and will find the individual data files in the data-raw
# directory, as suggested in Wickham's "R Packages" book.
#library(stringr)

# Start by getting a listing of files in the data-raw folder, which we'll read
# into our environment.
#contents <- list.files(path = "../data-raw")

# We'll read each file in turn
#for (this_file in contents) {
  # Start by extracting the filename, which we'll assume is everything up to,
  # but not including, the final period
#  fullname <- strsplit(this_file, "\\.")[[1]]
#  filename <- fullname[1]
#  extension <- fullname[2]
#  if (extension != "csv") {
#    print(paste("Skipping processing of", fullname), quote = FALSE)
#    next
#  }

  # Read the file into a tibble
#  assign(filename, readr::read_csv(file.path("../data-raw", this_file)))
#}

# Remove the temporary variables we created above
#rm(list = c("contents", "extension", "filename", "fullname", "this_file"))

# And finally, save the results in the data folder
#save.image(file = "../data/data.RData")
