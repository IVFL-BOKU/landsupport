library(mlr3)
library(mlr3learners)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(doParallel, quietly = TRUE)
source('R/extractPredictions.R')
#future::plan('multiprocess', workers = 24)
load('vignettes/merged_EU_2018.RData')

#### data preparation ####
dataCl = lucas %>%
  group_by(classname) %>%
  filter(n() >= 1000) %>%
  filter(obs_type == 1 | classname %in% c('Inland water', 'Rice') & obs_dist < quantile(obs_dist, 0.75, na.rm = TRUE) | classname == 'Artificial areas' & is.na(obs_type) & row_number() %% 1000 < 115) %>%
  ungroup() %>%
  mutate(id = row_number())
rm(lucas)
dataCl$classnameF = factor(dataCl$classname)

tibble(col = names(dataCl), p = 100 * colSums(is.na(dataCl)) / nrow(dataCl)) %>% filter(grepl('lai.*m1', col)) %>% arrange(col) %>% print(n = 24)
cols = list(
  colsBenchmark = c('id', 'classnameF', 'lc_1900.01.01'),
  colsYearly = c('id', 'classnameF', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE)),
  cols48 = c('id', 'classnameF', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.0[45678]m1', names(dataCl), value = TRUE))
)
data = list()
for (i in seq_along(cols)) {
  j = cols[[i]]
  d = dataCl %>% select(!!j) %>% filter(rowSums(is.na(.)) == 0)
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
learners = lapply(c('classif.naive_bayes', 'classif.kknn', 'classif.xgboost', 'classif.lda', 'classif.ranger', 'classif.svm'), function(x){lrn(x)})
results = benchmark(benchmark_grid(data, learners, list(rsmp('holdout', ratio = 0.66), rsmp('holdout', ratio = 0.66), rsmp('holdout', ratio = 0.66))))
print('saving')
save(results, file = 'vignettes/lucas_algo_selection.RData')
pred = extractPredictions(results)
pred %>%
  group_by(task, learner) %>%
  summarize(accuracy = sum(truth == response) / n()) %>%
  arrange(desc(accuracy)) %>%
  print(n = 100)
