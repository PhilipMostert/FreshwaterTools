#' @title CreateComponents
#' @description Create components required by \pkg{inlabru}.
#' @param fixedEffects A vector of the names of the fixed effects included in the model.
#' @param likelihoodIntercept Create an intercept for each likelihood. Defaults to \code{TRUE}.
#' @param speciesIntercept Create an intercept for the species. Defaults to \code{FALSE}.
#' @param speciesInterceptModel The model for the species intercept. Defaults to xxx.
#' @param speciesSpatial Create a pecies spatial effect included in the model. Defaults to \code{TRUE}.
#' @param speciesSpatialModel The model for the species spatial. Defaults to xxx.
#' @param biasSpatial Include a bias spatial effect in the model (logical).
#' @param customComponents Custom components to include in the model.
#' @param fixedPriors Priors for the fixed (environmental and observation effects). Must be a named vector containing: \code{prior.mean} and \code{prior.prec}.

CreateComponents <- function(fixedEffects,
                             likeLihoodIntercept = TRUE,
                             speciesIntercept = FALSE,
                             speciesInterceptModel = list(),
                             speciesSpatial = TRUE,
                             speciesSpatialModel = list(),
                             biasSpatial = FALSE,
                             customComponents,
                             fixedPriors = c(prior.mean = 0, prior.prec = 1)) {

  ##Paste and seperate environmental + po, pa, counts
  #Paste biasSpatial if needed

  #Create spatial effect (based on some subsets).

  #Include custom to everywhere

  #Return a list with:
   #Components usable by inlabru
   #A list of the poEffects, paEffects, countsEffects to use in MakeLikelihoods


}

