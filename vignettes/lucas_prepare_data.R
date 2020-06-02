library(dplyr)

load('../data/shapes/lucas/extracted_EU_2018.RData')
# 337k points but some of them are poorly geolocated
lucas = readr::read_csv('../data/shapes/lucas/EU_2018_190611.csv', guess_max = 300000)
urban = readr::read_tsv('../data/urban/tiled_EU_osm_labels_coordinates.csv')
mapping = openxlsx::read.xlsx('../data/shapes/lucas/LUCAS crop types.xlsx')
names(data) = tolower(names(data))
names(lucas) = tolower(names(lucas))
names(urban) = tolower(names(urban))
names(mapping) = tolower(names(mapping))
urban = urban %>%
  filter(class == 1) %>%
  rename(ruralclass = class) %>%
  mutate(classname = 'Artificial areas') %>%
  inner_join(data)
lucas = lucas %>%
  inner_join(mapping) %>%
  inner_join(data)
lucas = bind_rows(lucas, urban)
names(lucas) = gsub('-', '.', names(lucas))
save(lucas, file = '../data/shapes/lucas/merged_EU_2018.RData')

##### Relative to doymaxndvi  ####
cols = grep('m1$', names(lucas), value = TRUE)
indDates = tibble(ind = cols) %>%
  mutate(
    indmonth = as.integer(sub('^.*(..)m1$', '\\1', ind)),
    indind = substring(ind, 1, nchar(ind) - 4)
  )
lucasL = lucas %>%
  filter(!is.na(doymaxndvi2_2018y1)) %>%
  mutate(monthmax = as.integer(round(doymaxndvi2_2018y1 / 30))) %>%
  select(point_id, monthmax, !!cols) %>%
  tidyr::pivot_longer(cols, names_to = 'ind') %>%
  inner_join(indDates) %>%
  mutate(ind2 = sprintf('%s%02dm1', indind, indmonth - monthmax + 12L))
lucasRel = lucasL %>%
  select(point_id, ind2, value) %>%
  tidyr::pivot_wider(names_from = 'ind2', values_from = 'value')
cols2 = setdiff(names(lucas), cols)
lucasRel = lucas %>%
  select(!!cols2) %>%
  inner_join(lucasRel)

save(lucasRel, file = '../data/shapes/lucas/mergedRel_EU_2018.RData')
