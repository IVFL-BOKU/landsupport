library(dplyr)
library(ggplot2)
library(randomForest)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3filters)
library(mlr3tuning)
library(paradox)
future::plan('multiprocess', workers = 6)

# from lucac_prepare_data.R
load('../data/shapes/lucas/merged_EU_2018.RData')

#### all classes ####
dataCl = lucas %>% as_tibble() %>% select(-geometry) %>% filter(!is.na(classname))
dataCl$classnameF = factor(dataCl$classname)
dataCl %>% group_by(classname) %>% summarize(n = n(), p = 100 * n() / nrow(.)) %>% arrange(n) %>% print(n = 100)
dataCl = dataCl %>% group_by(classname) %>% filter(n() >= 100) %>% ungroup()
dataCl$classnameF = factor(as.character(dataCl$classname))
dataCl$lc_1900.01.01 = factor(dataCl$lc_1900.01.01)
#### features selection ####
cols = list(
  colsBenchmark = c('classnameF', 'lc_1900.01.01'),
  colsYearly = c('classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE)),
  cols58 = c('classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.0[5678]m1', names(dataCl), value = TRUE)),
  cols48 = c('classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.0[45678]m1', names(dataCl), value = TRUE)),
  cols59 = c('classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.0[56789]m1', names(dataCl), value = TRUE)),
  cols49 = c('classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.0[456789]m1', names(dataCl), value = TRUE)),
  cols50 = c('classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.[01][567890]m1', names(dataCl), value = TRUE)),
  cols40 = c('classnameF', 'lc_1900.01.01', 'rain_1900.01.01', 'temp_1900.01.01', grep('^(doy|nd|mn|nd|bs|bl).*_2018y1$', names(dataCl), value = TRUE), grep('^[fnlb].*_2018.[01][4567890]m1', names(dataCl), value = TRUE))
)
data = list()
for (i in seq_along(cols)) {
  j = cols[[i]]
  data[[i]] = TaskClassif$new(
    id = paste0('crop', sub('cols', '', names(cols)[i])),
    target = 'classnameF',
    backend = dataCl %>% select(!!j) %>% filter(rowSums(is.na(.)) == 0)
  )
}
validCount = tibble(
  task_id = sapply(data, function(x){x$id}),
  valid_obs = sapply(data, function(x){x$nrow}) / nrow(dataCl)
) %>%
  arrange(desc(valid_obs)) %>%
  mutate(drop = valid_obs - lag(valid_obs))
validCount
#### fetures selection - features ranking ####
selectForest = lrn('classif.ranger', importance = 'impurity', respect.unordered.factors = 'order')
resamplerCv = rsmp('cv', folds = 5)
features = lapply(data[-1], function(x){
  filter = flt('importance', learner = selectForest)
  filter$calculate(x)
  tibble(
    task_id = x$id,
    filter = list(filter),
    prediction = list(benchmark(benchmark_grid(tasks = x, learners = selectForest, resamplings = resamplerCv)))
  )
}) %>%
  bind_rows() %>%
  mutate(
    data = purrr::map(filter, as.data.table),
    classif.acc = unlist(purrr::map(prediction, function(x){x$aggregate(msr('classif.acc'))$classif.acc}))
  ) %>%
  inner_join(validCount %>% select(-drop))
featuresRanking = features %>%
  tidyr::unnest(data) %>%
  group_by(task_id) %>% arrange(desc(score)) %>% mutate(rank = row_number()) %>%
  group_by(feature) %>% arrange(desc(valid_obs), rank) %>% filter(row_number() == 1) %>%
  ungroup() %>% arrange(desc(valid_obs), rank, desc(score))
featuresRanking %>% print(n = 40)


#### fetures selection - models ranking ####
learnForest = lrn('classif.ranger', predict_type = 'prob', respect.unordered.factors = 'order')
minDiff = 0.002
tmpValidObs = 1
tmpCols = c('classnameF')
results = list()
for (i in seq_along(featuresRanking$feature)) {
  if (featuresRanking$valid_obs[i] >= tmpValidObs) {
    next
  }
  ii = length(results) + 1
  tmpCols = c(tmpCols, featuresRanking$feature[i])
  results[[ii]] = benchmark(
    benchmark_grid(
      tasks = TaskClassif$new(id = paste0('cropRank', ii), target = 'classnameF', backend = dataCl %>% select(!!tmpCols) %>% filter(rowSums(is.na(.)) == 0)),
      learners = learnForest,
      resamplings = rsmp('cv', folds = 5)
    )
  )
  tmpAcc = results[[ii]]$aggregate(msr('classif.acc'))$classif.acc
  tmpDiff = NA_integer_
  if (ii > 1) {
    tmpDiff = tmpAcc - results[[ii - 1]]$aggregate(msr('classif.acc'))$classif.acc
    if (tmpDiff < minDiff) {
      tmpValidObs = featuresRanking$valid_obs[i]
    }
  }
  cat(ii, featuresRanking$feature[i], tmpAcc, tmpDiff, featuresRanking$valid_obs[i], '\n')
}

# design & estimation
design = benchmark_grid(
  tasks = data,
  learners = list(learnForest),
#  learners = list(learnForest, learnBayes, learnRpart, learnSvm, learnQda, learnGlmnet, learnKknn, learnLda, learnXgboost),
  resamplings = rsmp('cv', folds = 5)
)
results = benchmark(design)
save(results, file = 'vignettes/lucas_analyze_models_class.RData')
results$aggregate(list(msr('classif.acc'))) %>% inner_join(validCount) %>% arrange(desc(classif.acc), desc(valid_obs))
modelsCl = list(
  # prefer learnForest because it's faster, the accuracy is only marginally lower and svm caused troubles with predictions on full raster data
  list(learner = learnForest$train(dataAll), cols = colsAll[-1], levels = levels(dataCl$classnameF)),
  list(learner = learnForest$train(dataYearly), cols = colsYearly[-1], levels = levels(dataCl$classnameF))
)
save(modelsCl, file = '/eodc/private/boku/ACube2/models/ML/cl.RData')

#### hyperparameters tuning ####
tunerForest = AutoTuner$new(
  learner = learnForest,
  resampling = rsmp('cv', folds = 3),
  measures = msr('classif.acc'),
  tune_ps = ParamSet$new(list(
    ParamInt$new('num.trees', lower = 250, upper = 750),
    ParamInt$new('min.node.size', lower = 1, upper = 10),
    ParamFct$new('splitrule', levels = c('gini', 'extratrees'))
  )),
  terminator = term('model_time', secs = 3600),
  tuner = tnr('random_search')
)
tunerSvm = AutoTuner$new(
  learner = learnSvm,
  resampling = rsmp('cv', folds = 3),
  measures = msr('classif.acc'),
  tune_ps = ParamSet$new(list(
    ParamFct$new('kernel', levels = c('linear', 'polynomial', 'radial', 'sigmoid'))
  )),
  terminator = term('model_time', secs = 3600),
  tuner = tnr('random_search')
)
design = benchmark_grid(
  tasks = list(dataYearly, dataAll),
  learners = list(tunerForest, tunerSvm),
  resamplings = rsmp('cv', folds = 5)
)
results2 = benchmark(design)
# improvment on a 4th digit - negligible
results2$aggregate(list(msr('classif.acc'))) %>% inner_join(validCount) %>% arrange(desc(valid_obs), desc(classif.acc))
results$aggregate(list(msr('classif.acc'))) %>% inner_join(validCount) %>% filter(learner_id %in% c('classif.ranger', 'classif.svm') & task_id %in% c('cropYearly', 'cropAll')) %>% arrange(desc(valid_obs), desc(classif.acc))

#### WINTER SUMMER ####
dataWs = lucas %>% as_tibble() %>% select(-geometry) %>% filter(!is.na(winter))
nas = tibble(
  col = names(dataWs),
  sat = !names(dataWs) %in% names(dataWs)[1:107],
  pna = 100 * colSums(is.na(dataWs)) / nrow(dataWs)
) %>%
  mutate(
    month = if_else(grepl('-[0-9][0-9]m1', col), suppressWarnings(as.integer(sub('^.*-([0-9]+)m1$', '\\1', col))), NA_integer_),
  ) %>%
  mutate(
    month = if_else(sat & grepl('y1$', col), 0L, month)
  )
tibble(na = 100 * colSums(is.na(dataWs)) / nrow(dataWs)) %>% ggplot(aes(x = na)) + geom_histogram() + ggtitle('number of variables with a given percentage of missing values')
#### base model ####
100 * table(dataWs$winter, dataWs$ws_2018y1) / nrow(dataWs) # 68% of correct classifications - not so bad for such a naive model
# data
dataWs$winterF = factor(dataWs$winter)
colsBasic = c('winterF', 'doymaxndvi2_2018y1', 'rain_1900.01.01', 'temp_1900.01.01')
colsYearly = c('winterF', 'doymaxndvi2_2018y1', 'rain_1900.01.01', 'temp_1900.01.01', 'ndvi2q05_2018y1', 'ndvi2q50_2018y1', 'ndvi2q98_2018y1', 'ndti2q05_2018y1', 'ndti2q50_2018y1', 'ndti2q98_2018y1', 'mndwi2q05_2018y1', 'mndwi2q50_2018y1', 'mndwi2q98_2018y1', 'ndbi2q05_2018y1', 'ndbi2q50_2018y1', 'ndbi2q98_2018y1', 'bsi2q05_2018y1', 'bsi2q50_2018y1', 'bsi2q98_2018y1', 'blfei2q05_2018y1', 'blfei2q50_2018y1', 'blfei2q98_2018y1')
colsMonthlyMayAug = c('winterF', 'rain_1900.01.01', 'temp_1900.01.01', 'lai2_2018.05m1', 'lai2_2018.06m1', 'lai2_2018.07m1', 'lai2_2018.08m1', 'ndvi2_2018.05m1', 'ndvi2_2018.06m1', 'ndvi2_2018.07m1', 'ndvi2_2018.08m1', 'fapar2_2018.05m1', 'fapar2_2018.06m1', 'fapar2_2018.07m1', 'fapar2_2018.08m1', 'fcover2_2018.05m1', 'fcover2_2018.06m1', 'fcover2_2018.07m1', 'fcover2_2018.08m1')
colsAll = c('winterF', 'doymaxndvi2_2018y1', 'rain_1900.01.01', 'temp_1900.01.01', 'ndvi2q05_2018y1', 'ndvi2q50_2018y1', 'ndvi2q98_2018y1', 'ndti2q05_2018y1', 'ndti2q50_2018y1', 'ndti2q98_2018y1', 'mndwi2q05_2018y1', 'mndwi2q50_2018y1', 'mndwi2q98_2018y1', 'ndbi2q05_2018y1', 'ndbi2q50_2018y1', 'ndbi2q98_2018y1', 'bsi2q05_2018y1', 'bsi2q50_2018y1', 'bsi2q98_2018y1', 'blfei2q05_2018y1', 'blfei2q50_2018y1', 'blfei2q98_2018y1', 'lai2_2018.05m1', 'lai2_2018.06m1', 'lai2_2018.07m1', 'lai2_2018.08m1', 'ndvi2_2018.05m1', 'ndvi2_2018.06m1', 'ndvi2_2018.07m1', 'ndvi2_2018.08m1', 'fapar2_2018.05m1', 'fapar2_2018.06m1', 'fapar2_2018.07m1', 'fapar2_2018.08m1', 'fcover2_2018.05m1', 'fcover2_2018.06m1', 'fcover2_2018.07m1', 'fcover2_2018.08m1')
dataBasic         = TaskClassif$new(id = 'WinterSummerBasic',         target = 'winterF', backend = dataWs %>% select(!!colsBasic) %>% filter(rowSums(is.na(.)) == 0))
dataYearly        = TaskClassif$new(id = 'WinterSummerYearly',        target = 'winterF', backend = dataWs %>% select(!!colsYearly) %>% filter(rowSums(is.na(.)) == 0))
dataMonthlyMayAug = TaskClassif$new(id = 'WinterSummerMonthlyMayAug', target = 'winterF', backend = dataWs %>% select(!!colsMonthlyMayAug) %>% filter(rowSums(is.na(.)) == 0))
dataAll           = TaskClassif$new(id = 'WinterSummerAll',           target = 'winterF', backend = dataWs %>% select(!!colsAll) %>% filter(rowSums(is.na(.)) == 0))
validCount = tibble(
  task_id = c('WinterSummerBasic', 'WinterSummerYearly', 'WinterSummerMonthlyMayAug', 'WinterSummerAll'),
  valid_obs = c(dataBasic$nrow, dataYearly$nrow, dataMonthlyMayAug$nrow, dataAll$nrow) / nrow(dataWs)
)
# learners
learnForest = lrn('classif.ranger', predict_type = 'prob')
learnBayes = lrn('classif.naive_bayes', predict_type = 'prob')
learnLog = lrn('classif.log_reg', predict_type = 'prob')
# sampler
sampler = rsmp('cv', folds = 5)
# design
design = benchmark_grid(
  tasks = list(dataBasic, dataYearly, dataMonthlyMayAug, dataAll),
  learners = list(learnForest, learnBayes, learnLog),
  resamplings = sampler
)
results = benchmark(design)
results$aggregate(list(msr('classif.acc'))) %>% inner_join(validCount) %>% arrange(desc(classif.acc))
autoplot(results, type = 'roc')
results$resample_result(1)$predictions()
modelsWs = list(
  # prefer learnForest because it's faster, the accuracy is only marginally lower and svm caused troubles with predictions on full raster data
  list(learner = learnForest$train(dataAll), cols = colsAll[-1], levels = levels(dataWs$winterF)),
  list(learner = learnForest$train(dataYearly), cols = colsYearly[-1], levels = levels(dataWs$winterF))
)
save(modelsWs, file = '/eodc/private/boku/ACube2/models/ML/ws.RData')
