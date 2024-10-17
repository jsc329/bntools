
#' Scrape eBird species from hotspot
#'
#' @param hotspot_id the ID number given by eBird to a given hotspot or region. You can provide multiple region codes at once by passing a vector of strings, such as hotspot_id = c("L11704882", "US-CA-051").
#' @param out_name the middle portion of the output filename. The filename will always be of the form "hotspot_species_out_name.csv", where "out_name" is replaced with whatever string is passed to the out_name argument. Defaults to the supplied hotspot_id.
#' @param out_dir the location where your list will export to. Defaults to current working directory.
#' @param test_run TRUE if you don't want to immediately write a csv out, FALSE to write out csv. This argument is almost exclusively used for package testing.
#'
#' @return Nothing, only an exported CSV
#' @export
#' @importFrom utils read.csv
#'
#' @examples
#' # Export the species list from Surprise Canyon, CA
#' # URL for this hotspot is here: https://ebird.org/hotspot/L11704882
#' # use the bit after the last /
#' # I set test run here to be TRUE so that a csv isn't written out
#' # change this to FALSE when you want to use this function
#' get_eblist("L11704882", test_run = TRUE)
get_eblist <- function(hotspot_id, out_name = hotspot_id, out_dir = getwd(), test_run = F){


  # First check to see if multiple hotspot ids were
  # specified, if so run this code block
  if (length(hotspot_id) > 1){

    # Also check if the default
    # out_name has not been changed
    # If so, just call it "combo"
    # to denote it's the aggregation of
    # three different hotspots
    if (length(out_name) > 1){

      out_name <- "combo"

    }

    temp_spot_data <- NULL

    # Now loop through all those spots
    for (spot in hotspot_id){

      ebird_page <- rvest::read_html(
        paste0("https://ebird.org/printableList?regionCode=",
               spot, "&yr=all&m="))

      # This section searches
      # for the divider "subitem"
      # and converts it to text
      species_entries <- ebird_page |>
        rvest::html_nodes("div.subitem") |>
        rvest::html_text()

      # This creates a dataframe
      # with the common names of species detected
      # at a given location
      hot_specs <- data.frame(COMMONNAME = species_entries)

      # Bind to our output
      temp_spot_data <- rbind(temp_spot_data, hot_specs)
    }

    # Clean up the output to only retain
    # unique entries in the species name column
    hot_specs <- data.frame(COMMONNAME = unique(temp_spot_data$COMMONNAME))

  }else{

# Section that runs if there's a single hotspot specified


  # Download the printable page for
  # an ebird hotspot or other division
  # of interest such as county
  ebird_page <- rvest::read_html(
    paste0("https://ebird.org/printableList?regionCode=",
           hotspot_id, "&yr=all&m="))

  # This section searches
  # for the divider "subitem"
  # and converts it to text
  species_entries <- ebird_page |>
    rvest::html_nodes("div.subitem") |>
    rvest::html_text()

  # This creates a dataframe
  # with the common names of species detected
  # at a given location
  hot_specs <- data.frame(COMMONNAME = species_entries)

  }

  setwd(out_dir)

  # Check if this is
  # a test run and stop it from
  # writing out

  if (!test_run){

  utils::write.csv(hot_specs,
            paste0("hotspot_species_",
                   out_name,
                   ".csv"), row.names = F)

    cat(paste0("There were ", nrow(hot_specs),
               " entries scraped from the eBird region(s) you specified.\n",
               "You wrote hotspot_species_", hotspot_id, ".csv",
               " to the ", getwd(), " directory."))
  }else{

    # If someone decided to do a test run report what number
    # of entries we got out
  cat(paste0("There were ", nrow(hot_specs),
             " entries scraped from the eBird region(s) you specified.\n",
             "You did not write an output CSV.\n Set test_run = FALSE to write CSV."))
  }

}





