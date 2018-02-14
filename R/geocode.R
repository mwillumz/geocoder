#' Geocode using census geocoder
#'
#' @description
#' Use the census geocoder to lookup lat/long and census geographies.
#'
#' @param data Data set to be geocoded.
#' @param benchmark Benchmark
#' @param vintage Vintage
#' @import dplyr
#' @export
geocode <- function(data, benchmark = "Current", vintage = "Current"){

  File <- tempfile(fileext = ".csv")
  readr::write_csv(data, File, col_names = FALSE)

  Resp <- httr::POST("https://geocoding.geo.census.gov/geocoder/geographies/addressbatch",
               body=list(benchmark = paste("Public_AR", benchmark, sep = "_"),
                         vintage = paste(vintage, benchmark, sep = "_"),
                         addressFile=upload_file(File)),
               encode="multipart") %>%
    httr::content("text", encoding = "UTF-8")

  readr::read_csv(Resp, col_names = c("id", "address_input", "match", "match_type",
                               "address_output", "lat_long", "tiger_id",
                               "side", "statefp", "countyfp", "tractce",
                               "blockce"))
}
