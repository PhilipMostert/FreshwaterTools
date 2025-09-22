#' @title MakeLikelihoods
#' @description Create a bru_obs_list object of likelihoods for each dataset.
#'
#' @param data A \code{metric_graph_data} object.
#' @param components Components to include from the model. Must be the output of \code{\link{CreateComponents}}.
#' @param effectsList A named list containing the vectors of named effects for each likelihood. If \code{NULL} for a likelihood, will use all effects in components.
#' @param formulaList A named list containing the formulas for each likelihood.
#' @param predictionFormula A named list of formulas for predicting on. Defaults to \code{NULL} which does not create any prediction formulas. The response variable for the formula needs to be y
#' @param likelihoodFamily A named list of the statistical families for each likelihood.
#'
#' @return A \link[inlabru]{bru_obs_list} object containing the likelihoods to be used by \link[inlabru]{bru}.
#' @export
#' @import MetricGraph
#' @importFrom inlabru bru_obs
#'

MakeLikelihoods <- function(data,
                            components,
                            effectsList = list(PO = NULL, PA = NULL, Counts = NULL),
                            formulaList = list(PO = y ~ ., PA = y ~ ., Counts = y ~ .),
                            likelihoodFamily = list(PO = 'poisson', PA = 'binomial', Counts = 'poisson'),
                            predictionFormula = NULL) {

  if (!inherits(data, 'metric_graph_data') & !inherits(data, 'sf')) stop ('data needs to be a metric_graph_data or sf object.')

  requiredVars <- c('y', 'likelihood', 'species', 'PA_intercept', 'PO_intercept', 'Counts_intercept', 'e', '.edge_number',
                    '.distance_on_edge', '.group', 'dataset_name')

  if (!any(requiredVars %in% names(data))) stop(cat('The following variables are required to be in the data object:\n',
                                                 paste0(requiredVars, collapse = ', ')))

  dataIn <- names(split(data, data$likelihood))

  dotFormula <- sapply(formulaList[dataIn], function(x) x[[3]] == '.')
  nullEffects <- sapply(effectsList[dataIn], is.null)

  if (any(dotFormula + nullEffects == 2)) stop ('At least one likelihood has both a NULL effectsList and a NULL RHS of a formula. Please provide effectsList and formulaList.')

  liks <- lapply(split(data, data$likelihood), function(x) {

    dataType <- unique(x$likelihood)

    bru_obs(data = x,
            family = likelihoodFamily[[dataType]],
            formula = formulaList[[dataType]],
            used = bru_used(effect = effectsList[[dataType]]),
            E = x$e,
            control.family = list(link = ifelse(any(c('binomial', 'Binomial') %in%  likelihoodFamily[[dataType]]), 'cloglog', 'log')),
            tag = dataType)

  })

  if (!is.null(predictionFormula)) {

    if (!inherits(predictionFormula, 'list')) stop ('predictionFormula needs to be a named list of the prediction formulas.')

    if (is.null(names(predictionFormula))) names(predictionFormula) <- paste0('Predictions', seq(1, length(predictionFormula)))

    predictionFormula <- sapply(predictionFormula, function(x) {

      if (length(x) == 2) x

      resp <- all.vars(x)[attr(terms(x), "response")]
      if (!identical(resp, 'y')) x <- update.formula(x, formula('y ~ .'))
      x

      })

    if (!'Prediction' %in% names(unique(data$dataset_name)))predictionData <- data[data$dataset_name %in% 'IPS',]
    else predictionData <- data[data$dataset_name %in% 'Prediction',]

    predictionData$y <- NA

    predLiks <- lapply(names(predictionFormula), function(x) {

      bru_obs(data = predictionData, family = 'poisson',
              formula = predictionFormula[[x]],
              E = predictionData$e, tag = x)

    })
    names(predLiks) <- names(predictionFormula)
    liks <- do.call(c, list(liks, predLiks))
  }

  allLiks <- do.call(c, liks)
  allLiks

  ##Make an option for a 'predict' likelihood


}
