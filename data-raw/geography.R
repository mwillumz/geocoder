library(rvest)
library(tidyverse)

GeoTranslate <- read_html("https://api.census.gov/data/2016/acs/acs5/geography.html") %>%
  html_nodes("table") %>%
  html_table(fill = TRUE) %>%
  data.frame() %>%
  filter(Reference.Date != Geography.Level) %>%
  mutate(censusapi = str_split(`Geography.Hierarchy`, "› ")) %>%
  mutate(geocoder = map(censusapi, ~ recode(., state = "statefp", county = "countyfp",
                                 `county subdivision` = 'cousubfp',
                                 tract = 'tractce', `block group` = 'blkgrpce',
                                 place = 'placefp',
                                 `american indian area/alaska native area/hawaiian home land` = 'aiannhce',
                                 `metropolitan statistical area/micropolitan statistical area` = 'cbsafp',
                                 `combined statistical area` = 'csafp',
                                 `new england city and town area` = 'nectafp',
                                 `urban area` = 'uace10',
                                 `congressional district` = 'cdXXXfp',
                                 `state legislative district (upper chamber)` = 'sldust',
                                 `state legislative district (lower chamber)` = 'sldlst',
                                 `public use microdata area` = 'pumace10',
                                 `school district (elementary)` = 'elsdlea',
                                 `school district (secondary)` = 'scsdlea',
                                 `school district (unified)` = 'unsdlea',
                                 `alaska native regional corporation` = 'anrcfp',
                                 `zip code tabulation area` = 'zcta5ce10')
  )
  ) %>%
  select(censusapi, geocoder)

devtools::use_data(GeoTranslate, internal = TRUE)
