#' Trains and saves a classification model(s)
#' @param targetDir directory storing computed model results
#' @param tmpDir directory for temporary files
#' @param modelName name of the model - output data are organized according to
#'   the model name
#' @param trainData dataframe with the training dataset (must contain all
#'   variables mentioned in the \code{featuresList} parameter)
#' @param targetVar variable in \code{trainData} storing the true value
#' @param featuresList names of variables to be used as predictors (if it's a
#'   list, a model will be computed for every list element)
#' @param period period to be used in output data filenames
#' @param nCores number of cores used during model training
#' @param skipExisting should model training be skipped if a model file already
#'   exists?
#' @return path to the file storing trained model(s)
#' @export
#' @import dplyr
prepareLandcoverModel = function(targetDir, tmpDir, modelName, trainData, targetVar, featuresList, period, nCores, skipExisting = TRUE) {
  if (!is.list(featuresList)) {
    featuresList = list('model' = featuresList)
  }
  trainData[, targetVar] = factor(unlist(trainData[, targetVar]))

  outFile = getTilePath(targetDir, modelName, period, 'MODEL', ext = 'RData')
  if (!file.exists(outFile) | skipExisting == FALSE) {
    models = list()
    for (i in seq_along(featuresList)) {
      features = c(targetVar, featuresList[[i]])
      data = trainData %>%
        dplyr::select(!!features)
      data = data %>%
        dplyr::filter(0 == rowSums(is.na(data))) %>%
        dplyr::mutate(.id = dplyr::row_number()) %>%
        mlr3::as.data.table()
      data = mlr3::TaskClassif$new(id = names(featuresList)[i], target = targetVar, backend = mlr3::DataBackendDataTable$new(data, '.id'))
      learner = mlr3::lrn('classif.ranger')
      learner$param_set$values = list(num.threads = nCores)
      learner$train(data)
      models[[names(featuresList)[i]]] = list(learner = learner, cols = featuresList[[i]], levels = data$levels()[[targetVar]])
    }

    tmpFile = paste0(tmpDir, '/', basename(outFile))
    save(models, file = tmpFile)
    createDirs(outFile)
    file.rename(tmpFile, outFile)
  }
  return(outFile)
}
