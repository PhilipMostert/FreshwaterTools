#' @title CreateIPS
#' @description Create integration points and add them to the model.
#' @param graph A graph from the \pkg{MetricGraph} package.
#' @param sf Return integration points as an \texit{sf object}. Defaults to \code{TRUE}.
#' @param proj Projection for the integration points. Defaults to \code{NULL}.
#' @param species A vector of species to include in the analysis. Defaults to \code{NULL} which means single-species model.
#' @param ... Additional arguments to be used by \code{.$build_mesh} (if a mesh has not been provided).
#'
#' @import MetricGraph
#' @export
#' @return A \pkg{sf} \textit{POINTS} object containing the IPS with the needed information.

CreateIPS <- function(graph, sf = TRUE, proj = NULL, species,...) {

  if (!inherits(graph, 'metric_graph')) stop ('The graph needs to be a "metric_graph object."')

  if (is.null(graph$mesh)) stop ('The graph requires a mesh. Add one using $build_mesh().')

  if (sf & is.null(proj)) stop('proj cannot be NULL if sf is TRUE.')

  graph$compute_fem()

  ipsWeights <- graph$mesh$weights

  ips <- tibble::tibble(y = rep(0, length(ipsWeights)),
                        e = ipsWeights,
                        dataset_name = 'IPS',
                        .edge_number = graph$mesh$VtE[,1],
                        .distance_on_edge = graph$mesh$VtE[,2],
                        PO_intercept = 1, ## This should change to PO_intercept I think
                        PA_intercept = NA, ##
                        Counts_intercept = NA, ##
                        .group = 'IPS',
                        .coord_x = graph$mesh$V[,1],
                        .coord_y = graph$mesh$V[,2],
                        likelihood = 'PO') ##Will probably also need a repl col for species. But can do that with fm_cprod

   # ips <- st_as_sf(ips,
   #                 coords = c('.coord_x', '.coord_x'),
   #                 crs = st_crs(graph$.__enclos_env__$private$proj4string))

  if (!is.null(graph$.__enclos_env__$private$data$species)) species <- graph$.__enclos_env__$private$data$species


  if (!is.null(species)) {

    ips <- fm_cprod(ips, data.frame(species = species))
    ips$.block_origin <- NULL
    ips$.block <- NULL
    ips$weight <- NULL
    ips$.group <- paste0('IPS.', ips$species)

  }

  if (sf == TRUE) ips <- st_as_sf(ips, coords = c('.coord_x', '.coord_y'), crs = proj)

  ips
  #graph$add_observations(ips, normalized = TRUE)

}

