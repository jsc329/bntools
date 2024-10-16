
#' Load a custom birdlist and convert it to BirdNET species list format
#'
#' @param ebird_file The custom species list file you want to read in. Must have a column that provides either the common name or scientific name of your birds of interest. Defaults to reading in a column called "COMMONNAME".
#' @param bn_version The version of BirdNET you plan to use, "2.1", "2.2", "2.4"
#' @param output_file The name of your output file, must end in ".txt"
#' @param name_col Specify the name of your column that holds the common names of species. Can only specify this name_col value or the sci_col value, but not both.
#' @param sci_col Specify the name of your column that holds scientific names of species. Can only specify this name_col value or the sci_col value, but not both.
#' @param del_ebird_file Set to TRUE if you want to delete your source species list on completion. Recommend keeping set to FALSE.
#' @param ebird_dir The directory your eBird list is coming from, defaults to your current working directory.
#' @param out_dir The directory your BirdNET output will be written to, defaults to your current working directory.
#' @param test_run Set to TRUE if you do not want an output file. Recommend setting to TRUE when loading in your custom list for the first time.
#' @param add_species Supply a list of bird names that are missing from your custom list. Can provide a single species like so, add_species = "Western Meadowlark", multiple species are added using add_species = c("Western Meadowlark", "Western Bluebird"), or by specifying a dataframe column. Additional species must match the column you provided to the function. If you provided an entry for name_col then provide common names, if you provided an entry for the sci_col then provide scientific names.
#'
#' @return A table of birds that did not match the master BirdNET species list
#' @export
#' @importFrom utils read.csv read.table write.table
#'
#' @examples
#'   # Create a temporary file
#'   file_path <- system.file("extdata", "input_birdlist.csv", package = "bntools")
#'   # Use the function to read the CSV
#'   # For further manipulation of the list of species without matches
#'   # please store the function output to a variable, as seen below in the example.
#' missing_table <- make_bn_speclist(ebird_file = file_path,
#' output = "birdnet_list.txt", name_col = "COMMONNAME", test_run = TRUE)
#'
make_bn_speclist <- function(ebird_file, bn_version = "2.4", output_file, name_col = NULL, sci_col = NULL, del_ebird_file = F, ebird_dir = getwd(), out_dir = getwd(),
                             test_run = F, add_species = NULL){

setwd(ebird_dir)
  # The ebird file, or other custom species list file
  # that a user is reading in
user_created_file <- read.csv(ebird_file)

# Get rid of the ebird species
# list if this flag is T

if (del_ebird_file){

file.remove(ebird_file)

}

# check if filename ends in txt
if (!stringr::word(output_file, sep = "\\.", start = -1) == "txt"){

  stop("Your output file name must end with a .txt")
}

# Check if the column you specified is in your input file
if (!is.null(name_col)){
  if (!name_col %in% colnames(user_created_file)){

    stop("The column you specified does not exist in your input file.")
  }
}

if (!is.null(sci_col)){
  if (!sci_col %in% colnames(user_created_file)){

    stop("The column you specified does not exist in your input file.")

  }
}

# First bring in the correct list
# for BirdNET version
if (bn_version == "2.2"){

  birdnet <- bn22

}
if (bn_version == "2.4"){

  birdnet <- bn24

}
if (bn_version == "2.1"){

  birdnet <- bn21
}
if (!bn_version %in% c("2.2", "2.1", "2.4")){
  stop("Wrong bn_version number. Must be either 2.2, 2.1, or 2.4")
}

if (!bn_version %in% c("2.2", "2.1", "2.4")){
  stop("Wrong bn_version number. Must be either 2.2, 2.1, or 2.4")
}

# First check and see if we have
# any entries specifying the column
# to check against BirdNET
if (is.null(name_col) & is.null(sci_col)){

  stop("No name column specified")
}

if (!is.null(name_col) & !is.null(sci_col)){

  stop("Specify either, name_col, or sci_col, but not both")
}

# Extract data from the column
# that has been specified by the user
# Check again common name
if (!is.null(name_col)){

  # Names that we want to
  # filter out of the BirdNET list
  names <- user_created_file[,name_col]
  # put names to lower case
  low_names <- tolower(names)

  # If there are species the user wants to
  # add in do that here
  if (length(add_species) > 0){

    low_names <- c(low_names, tolower(add_species))
    # Check to make sure there aren't duplicates
    low_names <- low_names[!duplicated(low_names)]

  }



  # See which BirdNET values it matches with
  #out_list <- filter(birdnet, tolower(COMMONNAME) %in% low_names)
  # Do this without tidyverse
  out_list <- birdnet[tolower(birdnet$COMMONNAME) %in% low_names , ]
  # See which values are in the input list
  # but are not in birdnet
  miss_list <- low_names[!low_names %in% tolower(birdnet$COMMONNAME)]


}else{

  # Check against scientific name

  # Names that we want to
  # filter out of the BirdNET list
  names <- user_created_file[,sci_col]
  # put names to lower case
  low_names <- tolower(names)

  if (length(add_species) > 0){

    low_names <- c(low_names, tolower(add_species))
    # Check to make sure there aren't duplicates
    low_names <- low_names[!duplicated(low_names)]

  }

  # See which BirdNET values it matches with
  #out_list <- filter(birdnet, tolower(SCINAME) %in% low_names)
  out_list <- birdnet[tolower(birdnet$SCINAME) %in% low_names , ]
  # See which values are in the input list
  # but are not in birdnet
  miss_list <- low_names[!low_names %in% tolower(birdnet$SCINAME)]

}

# Now format the data for use in BirdNET
#out_list <- select(out_list, birdnet_code)
out_list <- out_list[, "birdnet_code"]

# Have a catch that stops
# it from writing out if
# test_run = T

if (!test_run) {

setwd(out_dir)
# Write it out
write.table(out_list,
            file = output_file,
            sep = "\n",
            col.names = F,
            row.names = F,
            quote = F)

}

# Return how long the list was
cat(paste0(length(out_list),
           " of ", nrow(user_created_file),
           " species in your species list matched the BirdNET species list.\n",
           "The values returned by this function show which entries are unmatched."))

# Return something empty if all
# species matched
if (length(miss_list) == 0){

  out_miss <- data.frame(COMMONNAME = "NONE", SCINAME = "NONE")
  return(out_miss)

}else{

  # If we're talking about common name
  # output which species are missing
  if (!is.null(name_col)){
    out_miss <- data.frame(COMMONNAME = miss_list)
    #return(out_miss)
    out_miss

  }else{
    out_miss <- data.frame(SCINAME = miss_list)

    #return(out_miss)
    out_miss

  }
}




}
