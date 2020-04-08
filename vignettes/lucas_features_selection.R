library(mlr3)
library(mlr3learners)
library(mlr3filters)
library(dplyr)
future::plan('multiprocess', workers = 4)
load('../data/shapes/lucas/mergedRel_EU_2018.RData')
load('../data/shapes/lucas/merged_EU_2018.RData')

#### Data preparation ####
dataCl = lucas %>%
  #filter(!is.na(obs_type)) %>%
  group_by(classname) %>%
  #filter((obs_type == 1 | obs_dist <= quantile(obs_dist, 0.75, na.rm = TRUE)) & n() >= 1000) %>%
  filter(n() >= 1000) %>%
  ungroup() %>%
  mutate(id = row_number())
dataClRel = lucasRel %>%
  semi_join(dataCl %>% select(point_id)) %>%
  mutate(id = 1000000L + row_number())
dataCl$classnameF = factor(dataCl$classname)
dataClRel$classnameF = factor(dataClRel$classname)

cols = list(
  colsBenchmark = c('id', 'classnameF', 'lc_1900.01.01'),
  colsYearly = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE)),
  cols58 = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.0[5678]m1', names(dataCl), value = TRUE)),
  cols48 = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.0[45678]m1', names(dataCl), value = TRUE)),
  cols59 = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.0[56789]m1', names(dataCl), value = TRUE)),
  cols49 = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.0[456789]m1', names(dataCl), value = TRUE)),
  cols38 = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.0[345678]m1', names(dataCl), value = TRUE)),
  rel13 = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataClRel), value = TRUE), grep('^[fnlb].*_2018.1[123]m1', names(dataClRel), value = TRUE)),
  rel03 = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataClRel), value = TRUE), grep('^[fnlb].*_2018.1[0123]m1', names(dataClRel), value = TRUE)),
  rel04 = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataClRel), value = TRUE), grep('^[fnlb].*_2018.1[01234]m1', names(dataClRel), value = TRUE))
)
data = list()
for (i in seq_along(cols)) {
  j = cols[[i]]
  if (grepl('rel', names(cols)[i])) {
    d = dataClRel %>% select(!!j) %>% filter(rowSums(is.na(.)) == 0)
  } else {
    d = dataCl %>% select(!!j) %>% filter(rowSums(is.na(.)) == 0)
  }
  data[[i]] = TaskClassif$new(id = paste0('crop', sub('cols', '', names(cols)[i])), target = 'classnameF', backend = DataBackendDataTable$new(as.data.table(d), 'id'))
}
names(data) = names(cols)
validCount = tibble(
  task_id = sapply(data, function(x){x$id}),
  n_obs = sapply(data, function(x){x$nrow}),
  valid_obs = sapply(data, function(x){x$nrow}) / nrow(dataCl)
) %>%
  arrange(desc(valid_obs)) %>%
  mutate(drop = valid_obs - lag(valid_obs))
validCount

#### benchmark_grid ####
results = benchmark(benchmark_grid(
  tasks = data['colsYearly'],
  learners = list(lrn('classif.ranger')),
  resamplings = list(rsmp('holdout', ratio = 0.66), rsmp('holdout', ratio = 0.66), rsmp('holdout', ratio = 0.66))
))
save(results, validCount, file = 'vignettes/lucas_features_selection.RData')
results$aggregate(list(msr('classif.acc'))) %>% inner_join(validCount) %>% arrange(desc(classif.acc)) %>% as_tibble()

#### features count reduction ####
filter = flt('information_gain')
learner = lrn('classif.ranger')
learner$param_set$values = list(num.threads = 1)
dataTmp = list()
for (i in c('cols49', 'cols58', 'colsYearly')) {
  inf = as.data.table(filter$calculate(data[[i]])) %>% as_tibble()
  for (j in nrow(inf):1) {
    tmp = data[[i]]$clone()
    tmp$id = paste0(i, '_', j, '_', inf$feature[j])
    tmp$col_roles$feature = inf$feature[1:j]
    dataTmp = append(dataTmp, tmp)
  }
}
resultsFR = benchmark(benchmark_grid(
  tasks = dataTmp,
  learners = learner,
  resamplings = list(rsmp('holdout', ratio = 0.66), rsmp('holdout', ratio = 0.66), rsmp('holdout', ratio = 0.66))
))
save(resultsFR, results, file = 'vignettes/lucas_features_selection.RData')