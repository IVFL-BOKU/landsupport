library(mlr3)
library(mlr3learners)
library(dplyr)
library(ggplot2)
devtools::load_all()
future::plan('multiprocess', workers = 4)
load('../data/shapes/lucas/merged_EU_2018.RData')
lucas = lucas %>%
  mutate(id = row_number(), classnameF = classname)

#### Basic distributions ####
lucas %>%
  filter(!is.na(obs_type)) %>%
  group_by(classname) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(p = 100 * n / sum(n)) %>%
  arrange(p) %>%
  print(n = 100)

lucas %>%
  filter(!is.na(obs_type)) %>%
  mutate(obs_type = c('visible <=100m', 'visible >100m', 'photo-interpretation', 'not visible', 'out of scope', 'out of scope' ,'photo-interpretation')[obs_type]) %>%
  mutate(obs_type = factor(obs_type, c('visible <=100m', 'visible >100m', 'photo-interpretation'))) %>%
  group_by(classname, obs_type) %>%
  summarize(count = n()) %>%
  group_by(classname) %>%
  mutate(percentage = 100 * count / sum(count), capped_count = if_else(count < 1000L, count, 1000L)) %>%
  ungroup() %>%
  ggplot(aes(x = obs_type, y = classname, size = capped_count, color = percentage)) +
  geom_point()

#### First scenarios ####
cols = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(lucas), value = TRUE), grep('^[fnlb].*_2018.0[45678]m1', names(lucas), value = TRUE))
d = lucas %>%
  filter(!is.na(obs_dist) & 0 == rowSums(is.na(lucas[, cols]))) %>%
  group_by(classname) %>%
  filter(obs_type %in% 1:2 | obs_dist < quantile(obs_dist, 0.75)) %>%
  ungroup()
d$classnameF = factor(d$classnameF)
d2 = TaskClassif$new(id = 's3', target = 'classnameF', backend = DataBackendDataTable$new(as.data.table(d[, cols]), 'id'))
d = d %>%
  filter(obs_type == 1)
d1$classnameF = factor(d1$classnameF)
d1 = TaskClassif$new(id = 's1', target = 'classnameF', backend = DataBackendDataTable$new(as.data.table(d[, cols]), 'id'))
results = benchmark(benchmark_grid(list(d2, d1), list(lrn('classif.ranger')), list(rsmp('cv', folds = 5))))
save(results, file = 'vignettes/lucas_points_selection.RData')
results$aggregate(list(msr('classif.acc')))
pres = extractPredictions(results)
tm = pres %>%
  group_by(task, truth) %>%
  summarize(p = 100 * sum(truth == response) / n(), n = n()) %>%
  ungroup() %>%
  arrange(task, p, truth)
print(tm, n = 100)
full_join(
  tm %>% filter(task == 's1') %>% select(-task) %>% rename(p_s1 = p, n_s1 = n),
  tm %>% filter(task == 's3') %>% select(-task) %>% rename(p_s2 = p, n_s2 = n)
) %>%
  mutate(s1_gain = p_s1 - p_s2) %>%
  arrange(s1_gain) %>%
  print(n = 100)

#### With additional artificial areas ####
cols = c('id', 'classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(lucas), value = TRUE), grep('^[fnlb].*_2018.0[45678]m1', names(lucas), value = TRUE))
d = lucas %>%
  filter(0 == rowSums(is.na(lucas[, cols]))) %>%
  group_by(classname) %>%
  filter(n() >= 1000) %>%
  filter(obs_type == 1 | classname %in% c('Inland water', 'Rice') & obs_dist < quantile(obs_dist, 0.75, na.rm = TRUE) | classname == 'Artificial areas' & is.na(obs_type) & row_number() %% 1000 < 115) %>%
  ungroup()
d$classnameF = factor(d$classnameF)
dAA = TaskClassif$new(id = 's3', target = 'classnameF', backend = DataBackendDataTable$new(as.data.table(dAA[, cols]), 'id'))
resultsAA = benchmark(benchmark_grid(list(dd), list(lrn('classif.ranger')), list(rsmp('cv', folds = 5))))
save(results, resultsAA, file = 'vignettes/lucas_points_selection.RData')
resultsAA$aggregate(list(msr('classif.acc')))
presAA = extractPredictions(resultsAA)
presAA %>%
  group_by(task, truth) %>%
  summarize(p = 100 * sum(truth == response) / n(), n = n()) %>%
  ungroup() %>%
  arrange(task, p, truth) %>%
  print(n = 100)
