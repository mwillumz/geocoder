#' Get census data via `censusapi`
#'
#' @description
#' Gets census data with a modified geography parameter
#'
#' @param vars Variables.
#' @param geo Geography.
#' @param key Census API key.
#' @import dplyr
#' @export
get_census <- function(vars = 'B01001_001E', geo, key = '0a05d377432e0e7c88dd138d65f79082a5c49eec'){

  Geotypes <- setdiff(names(geo), c('geotype', 'geoid'))

  #Translate to api style
  GeotypeApi <- filter(geocoder:::GeoTranslate, purrr::map_lgl(geocoder, ~setequal(Geotypes, .x))) %>%
    pull(censusapi) %>%
    unlist()

  Region <- paste(last(GeotypeApi), "*", sep = ":")

  names(Geotypes) <- GeotypeApi

  geo <- rename(geo, !!! Geotypes)

  Data <- if(length(Geotypes) > 1){

    RegionIns <- select(geo, setdiff(GeotypeApi, last(GeotypeApi))) %>%
      distinct() %>%
      mutate(id = row_number()) %>%
      tidyr::gather(key = "key", value = "value", -id) %>%
      group_by(id) %>%
      mutate(thing = paste(key, value, sep = ":")) %>%
      summarise(huh = paste(thing, collapse = "+")) %>%
      pull('huh')

    lapply(RegionIns, function(i){
      censusapi::getCensus('acs/acs5', 2016, key = key, vars = vars,
                region = Region, regionin = i) %>%
        tidyr::unite(geoid, GeotypeApi, sep = '') %>%
        filter(geoid %in% geo$geoid) %>%
        mutate(geotype = last(Geotypes))
    }) %>%
      bind_rows()
  } else{
    RegionIn <- NULL
    censusapi::getCensus('acs/acs5', 2016, key = key, vars = vars,
              region = Region, regionin = RegionIn) %>%
      tidyr::unite(geoid, GeotypeApi, sep = '') %>%
      filter(geoid %in% geo$geoid) %>%
      mutate(geotype = last(Geotypes))
  }

  return(Data)
}
