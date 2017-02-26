#' Divergence to Distance
#' #'
#' a utility to convert divergence matrices to one matrix where each entry is the distance between each pair of matrices.
#'
#' @author Eric Bridgeford
#' @keywords divergence, distance
#' @param div [[subs]][roi, rois]: the kullback-leibler divergence for each pair of rois.
#' @param dist [nsub, nsub]: the hellinger distance between each pair of subjects.
#' @export
div2dist <- function(div) {
  nsub <- length(div)
  dist <- array(NaN, dim=c(nsub, nsub))
  for (sub1 in 1:nsub) {
    for (sub2 in 1:nsub) {
      dist[sub1, sub2] <- hell_dist(div[[sub1]], div[[sub2]])
    }
  }
  return(dist)
}
