#' @title extractCovariates
#' @description Assign covariate information to the observation data and integration points
#' @param data A \code{metric_graph_data} object.
#' @param covariates A spatial raster in a format usable by \pkg{terra}.
#' @param used Which spatial covariates to include in the model.
#'
#' @import MetricGraph
#' @export
#' @return A data.frame object containing the covariate information.

ExtractCovariates <- function(data, covariates, used = NULL) {

  if (!inherits(data, 'metric_graph_data')) stop ('data needs to be a metric_graph_data object.')

  if (!inherits(covariates, 'SpatRaster')) stop ('covariates needs to be a SpatRaster object.')

  if (is.null(used)) used <- names(covariates)
  else
    if (!all(used %in% names(covariates))) stop ('Some of the covariates specified in used are not in the raster stack.')

  covs <- terra::extract(terra::subset(covariates, used),  st_sf(data), ID = FALSE)
  names(covs) <- used
  if (any(is.na(covs))) {

    naRows <- lapply(covs, function(x) which(is.na(x)))
    naCovs <- names(naRows)[sapply(naRows, length) > 0]

    for(covName in naCovs){
      covs[naRows[[cov]], covName] <-
        PointedSDMs::nearestValue(matrix(st_coordinates(covs[naRows[[covName]],])[,c("X","Y")], ncol = 2), covariates[covName])

    }

  }

  covs

}
