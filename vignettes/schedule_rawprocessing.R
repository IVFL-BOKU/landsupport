library(sentinel2)
library(dplyr)

date2months = function(x, k = 1) {
  return(as.integer(substr(x, 1, 4)) * 12 + floor(as.integer(substr(x, 6, 7)) / k) * k - 1)
}
months2date = function(x, k = 1, end = FALSE) {
  x = x + if_else(end, 1L, 0L)
  date = as.Date(sprintf('%04d-%02d-01', floor(x / 12), 1 + (x %% 12) * k)) - if_else(end, 1L, 0L)
  return(as.character(date))
}

bands = c('B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B11', 'B12', 'SCL', 'LAI', 'TCI', 'FAPAR', 'FCOVER')
monthMin = date2months('2020-03')
monthMax = date2months('2020-04')
S2_initialize_user('landsupport', 'CbYwN9cNvp')
roiEu = S2_query_roi(regionId = 'EU_cube')
stopifnot(nrow(roiEu) == 1)

i = list()
for (month in monthMin:monthMax) {
  cat(months2date(month), '\n')
  i[[length(i) + 1]] = S2_query_image(regionId = roiEu$regionId, cloudCovMax = 50, dateMin = months2date(month), dateMax = months2date(month, end = TRUE)) %>%
    as_tibble()
}
i = bind_rows(i)
i = i %>%
  semi_join(i %>% group_by(band) %>% summarize(resolution = min(resolution)) %>% ungroup()) %>%
  filter(band %in% bands)
regions = S2_query_roi(regionId = 'granule_%') %>%
  select(regionId) %>%
  mutate(granuleId = as.integer(substring(regionId, 9)))
g = i %>%
  group_by(granuleId, date, utm, orbit, cloudCov) %>%
  filter(processDate == max(processDate)) %>%
  summarize(
    processed = sum(atmCorr == max(atmCorr) & atmCorr > 0) == length(bands),
    atmCorr = max(atmCorr),
    owned = all(!is.na(url[atmCorr == max(atmCorr)]))
  ) %>%
  ungroup() %>%
  mutate(
    month = date2months(date)
  ) %>%
  left_join(regions)
save(i, g, file = 'all_granules.RData')

muo = g %>%
  select(month, utm, orbit) %>%
  distinct() %>%
  tidyr::complete()
muo = muo %>%
  left_join(g) %>%
  group_by(month, utm, orbit) %>%
  arrange(cloudCov) %>%
  summarize(
    nAcq = n(),
    nProcessed = coalesce(sum(processed), 0L),
    nOwned = coalesce(sum(owned), 0L),
    nReady = coalesce(sum(processed & owned), 0L),
    cc1 = cloudCov[1],
    cc2 = lead(cloudCov)[1],
    cc3 = lead(cloudCov, 2)[1],
    cc4 = lead(cloudCov, 3)[1],
  ) %>%
  ungroup() %>%
  mutate(
    ccc1 = cc1,
    ccc2 = cc1 * cc2 / 100,
    ccc3 = cc1 * cc2 * cc3 / 10000,
    ccc4 = cc1 * cc2 * cc3 * cc4 / 1000000,
    ccThresh = (roiEu$dwnldThresh / 100) ^ 4 * 100
  ) %>%
  mutate(
    includeRank = case_when(ccc3 > ccThresh ~ 4, ccc2 > ccThresh ~ 3, TRUE ~ 2)
  )

### stats
stats = table(c('not ready', 'ready')[(muo$nReady > 0L) + 1L], c('no acceptable cc', 'some acceptable cc')[(muo$cc1 <= roiEu$dwnldThresh) + 1L])
stats                   # how many {month x utm x orbit} already have data and how many may have
stats / nrow(muo) * 100 # same in percents
stats2 = muo %>%                 # what is a distribution of number of acqusitions per {month x utm x orbit}
  select(month, utm, orbit, includeRank) %>%
  inner_join(g) %>%
  group_by(month, utm, orbit) %>%
  filter(row_number() <= includeRank & cloudCov < roiEu$dwnldThresh | cloudCov <= roiEu$cloudCovMax) %>%
  summarize(n = n(), ready = sum(processed)) %>%
  ungroup() %>%
  select(n, ready)
table(stats2$n)                # overall
table(stats2$ready)            # ready
table(stats2$n - stats2$ready) # to be processed
c('#granules' = sum(stats2$n), '#ready' = sum(stats2$ready), '#to be processed' = sum(stats2$n) - sum(stats2$ready))

### roiToCreate
toAdd = muo %>%
  select(month, utm, orbit, includeRank) %>%
  inner_join(g) %>%
  group_by(month, utm, orbit) %>%
  arrange(month, utm, orbit, cloudCov) %>%
  mutate(
    wanted = row_number() <= includeRank & cloudCov < roiEu$dwnldThresh | cloudCov <= roiEu$cloudCovMax,
    bought = !is.na(regionId)
  ) %>%
  ungroup()
table(c('not wanted', 'wanted')[toAdd$wanted + 1L], c('roi', 'no roi')[is.na(toAdd$regionId) + 1L])
toAdd = toAdd %>%
  filter(wanted & is.na(regionId))

save(toAdd, file = '~/Pulpit/cube.RData')
load('~/Pulpit/cube.RData')
for (j in seq_along(toAdd$granuleId)) {
  if (!toAdd$bought[j]) {
    try({
      S2_buy_granule(toAdd$granuleId[j], 'force')
      toAdd$bought[j] = TRUE
    })
  }
  if (j %% 50 == 0) {
    cat(j, '\n')
    save(toAdd, file = '~/Pulpit/cube.RData')
  }
}
