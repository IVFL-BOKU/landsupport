library(mlr3)
library(mlr3learners)
library(mlr3filters)
library(dplyr)
devtools::load_all()
load('vignettes/mergedRel_EU_2018.RData')
load('vignettes/merged_EU_2018.RData')

#### Data preparation ####
dataCl = lucas %>%
  group_by(classname) %>%
  filter(n() >= 1000) %>%
  filter(obs_type == 1 | classname %in% c('Inland water', 'Rice') & obs_dist < quantile(obs_dist, 0.75, na.rm = TRUE) | classname == 'Artificial areas' & is.na(obs_type) & row_number() %% 1000 < 115) %>%
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

#### comparing different feature sets ####
learner = lrn('classif.ranger')
learner$param_set$values = list(num.threads = 38)
results = benchmark(benchmark_grid(
  tasks = data,
  learners = learner,
  resamplings = list(rsmp('holdout', ratio = 0.66), rsmp('holdout', ratio = 0.66), rsmp('holdout', ratio = 0.66))
))
save(results, validCount, file = 'vignettes/lucas_features_selection.RData')
results$aggregate(list(msr('classif.acc'))) %>% inner_join(validCount) %>% arrange(desc(classif.acc)) %>% as_tibble() %>% group_by(task_id) %>% summarize(acc = mean(classif.acc)) %>% inner_join(validCount) %>% arrange(desc(acc)) %>% select(task_id, valid_obs, acc) %>% mutate(valid_obs = 100 * valid_obs, acc = 100 * acc)

#### selected feature sets reduction ####
filter = flt('information_gain')
learner = lrn('classif.ranger')
learner$param_set$values = list(num.threads = 38)
dataTmp = list()
for (i in c('cols48', 'cols58', 'colsYearly')) {
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
predFR = extractPredictions(resultsFR)
save(resultsFR, results, predFR, file = 'vignettes/lucas_features_selection.RData')

predAgg = predFR %>%
  group_by(task, uhash, truth, response) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(
    set = sub('_.*$', '', task),
    step = as.numeric(sub('^.*_([0-9]+)_.*$', '\\1', task)),
    var = sub('^[^_]+_[^_]+_', '', task)
  )
predAgg = predAgg %>%
  group_by(set, step, var) %>%
  summarize(N = sum(n), accuracy = 100 * sum(if_else(truth == response, n, 0L)) / sum(n)) %>%
  arrange(set, desc(step)) %>%
  group_by(set) %>%
  mutate(
    drop = accuracy - lag(accuracy),
    dropAbs = accuracy - first(accuracy),
    var = lag(var),
    relative_features_count = 100 * (step / first(step))
  )
predAgg %>% print(n = 300)
dict = c(cols48 = 'April-August', cols58 = 'May-August', 'colsYearly' = 'yearly only')
predAgg %>%
  mutate(`features set` = dict[set]) %>%
  ggplot(aes(x = step, y = accuracy, group = `features set`, color = `features set`)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE, linetype = 2) +
  theme_bw() +
  xlab('number of features') +
  ylab('classification accuracy [%]') +
  ggtitle('Impact of reducing features count on classification accuracy')

#### error matrix for selected sets ####
selected = grep('48_54|58_38|Yearly_22', unique(predFR$task), value = TRUE)
truth = predFR %>%
  filter(task %in% selected) %>%
  group_by(task, truth, response) %>%
  summarize(n = n()) %>%
  group_by(task, truth) %>%
  mutate(pt = 100 * n / sum(n)) %>%
  group_by(task, response) %>%
  mutate(pr = 100 * n / sum(n)) %>%
  ungroup()
truth %>%
  filter(grepl('^cols48', task)) %>%
  select(truth, response, pt) %>%
  mutate(pt = round(pt, 1)) %>%
  tidyr::complete() %>%
  tidyr::pivot_wider(names_from = 'truth', values_from = 'pt') %>%
  arrange(response) %>%
  readr::write_csv('../deliverable 4.2/truth_48.csv', na = '0')
truth %>%
  filter(grepl('^cols48', task)) %>%
  group_by(truth) %>%
  summarize(n = sum(n)) %>%
  ungroup() %>%
  mutate(p = 100 * n / sum(n))
truth %>%
  filter(grepl('^cols48', task) & truth == response) %>%
  inner_join(truthP48 %>% select(-n)) %>%
  ggplot(aes(x = p, y = pt, label = truth)) +
  geom_point() +
  ggrepel::geom_text_repel() +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100)) +
  ggtitle('Classification accuracy vs class coverage') +
  xlab('Percentage of observations covered by a class') +
  ylab('Classification accuracy [%]')

#### final models ####
bestSets = c(cols48 = 54, cols58 = 38, colsYearly = 22)
filter = flt('information_gain')
models = list()
for (i in c('cols48', 'cols58', 'colsYearly')) {
  inf = as.data.table(filter$calculate(data[[i]])) %>% as_tibble()
  dataTmp = data[[i]]$clone()
  dataTmp$col_roles$feature = inf$feature[1:bestSets[i]]
  learner = lrn('classif.ranger', predict_type = 'prob')
  learner$param_set$values = list(num.threads = 38)
  learner$train(dataTmp)
  models[[i]] = list(learner = learner, cols = inf$feature[1:bestSets[i]], levels = dataTmp$levels()$classnameF)
}
save(models, file = 'vignettes/lucas_features_selection_models.RData')

