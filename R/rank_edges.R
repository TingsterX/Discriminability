#' Rank Edges
#'
#' A function that ranks all the entries in each graph.
#'
#' @param graphs [[n]][nroi, nroi] the graphs to rank.
#' @param normalize=FALSE : a boolean indicating whether to normalize so the rank for a graph has values betweeen 0 and 1.
#' @return rgraphs [[n]][nroi, nroi] the ranked graphs.
#' @author Eric Bridgeford
#' @export
discr.rank_edges <- function(graphs, normalize=FALSE) {
  rgraphs <- sapply(graphs, function(g) {
    d <- dim(g)  # the dimensions of the graph
    # use the stats ranking function
    rgraphs <- array(rank(graphs[,,i], ties.method="average"), c(d[1], d[2]))
    if (normalize) {
      # normalize values to fall btwn 0 and 1
      rg[,,i] <- ( rg[,,i] - min(rg[,,i]) ) / (max(rg[,,i]) - min(rg[,,i]))
    }
    return(rg)
  }, simplify=FALSE)
  return(rgraphs)
}
