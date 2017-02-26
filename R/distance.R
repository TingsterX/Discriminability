#' Distance
#'
#' A function that returns a distance matrix given a collection of graphs. Note that this function uses the frobenius norm to compute the distance between all pairs of graphs.
#'
#' @param graphs [nroi, nroi, n]: a collection of graphs for n subjects, where each graph is a square matrix.
#' @return dist [n, n]: a matrix indicating the pairwise distances between all graphs passed in.
#' @author Gregory Kiar
#' @export
distance <- function(graphs) {
  library("stats")
  dim_graphs <- dim(graphs) # get the dims of the graphs
                            # expect dim_graphs[1] to be nrois, dim_graphs[2] to be nois, dim_graphs[3] to be numsubs
  reshape_graphs <- t(array(graphs, dim=c(dim_graphs[1]*dim_graphs[2], dim_graphs[3])))
  dist_graphs <- dist(reshape_graphs, diag=TRUE, upper=TRUE) # use stats dist function
  dist <- array(matrix(as.matrix(dist_graphs)), dim=c(dim_graphs[3],dim_graphs[3]))
  return(dist)
}
