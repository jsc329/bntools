
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

  write.csv(hot_specs,
            paste0("hotspot_species_",
                   hotspot_id,
                   ".csv"), row.names = F)
}
