
#' Scrape eBird species from hotspot
#'
#' @param hotspot_id the ID number given by eBird to a given hotspot or region
#' @param out_name the middle portion of the output filename, see example
#' @param out_dir the location where your list will export to. Defaults to current working directory
#'
#' @return Nothing, only an exported CSV
#' @export
#'
#' @examples
#' # Export the species list from Surprise Canyon, CA
#' # URL for this hotspot is here: https://ebird.org/hotspot/L11704882
#' # use the bit after the last /
#' get_eblist("L11704882")
get_eblist <- function(hotspot_id, out_name = hotspot_id, out_dir = getwd()){

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

  utils::write.csv(hot_specs,
            paste0("hotspot_species_",
                   hotspot_id,
                   ".csv"), row.names = F)
}
