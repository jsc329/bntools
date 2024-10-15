
#' Scrape eBird species from hotspot
#'
#' @param hotspot_id the ID number given by eBird to a given hotspot or region
#' @param out_name the middle portion of the output filename, see example
#' @param out_dir the location where your list will export to. Defaults to current working directory
#' @param test_run TRUE if you don't want to immediately write a csv out, FALSE for write out csv
#'
#' @return Nothing, only an exported CSV
#' @export
#' @importFrom utils read.csv
#'
#' @examples
#' # Export the species list from Surprise Canyon, CA
#' # URL for this hotspot is here: https://ebird.org/hotspot/L11704882
#' # use the bit after the last /
#' # I set test run here to be true so that a csv isn't written out
#' # change this to F when you want to use this function
#' get_eblist("L11704882", test_run = TRUE)
get_eblist <- function(hotspot_id, out_name = hotspot_id, out_dir = getwd(), test_run = F){

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

  setwd(out_dir)

  # Check if this is
  # a test run and stop it from
  # writing out

  if (!test_run){

  utils::write.csv(hot_specs,
            paste0("hotspot_species_",
                   hotspot_id,
                   ".csv"), row.names = F)
  }
}
