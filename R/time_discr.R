#' Time Domain Discriminability
#'
#' A function that performs discriminability in the time domain using the correlation.
#'
#' @param signal [[n]][nt, nroi]: the signal for each of the n subjects, containing an array of nt observations for nroi rois.
#' @param ids [n]: the ids for each scan corresponding to the signal from above.
#' @param rank=FALSE : a boolean indicating whether to do unranked (FALSE) or ranked (TRUE).
#' @return d [1]: the discriminability statistic for the data.
#' @return dist [n, n]: the distance matrix associated with the data.
#' @return distplot : a plot of the distance matrix.
#' @return kdeplot : a plot of the density estimate of the intra vs inter subject distances.
#' @return combinedplot : a multiplot showing the dist plot and the kde plot.
#' @author Eric Bridgeford
#' @export
time_discr <- function(signal, ids, rank=FALSE) {
  corr <- obs2corr(signal)

  ## Change Convention from preferred vara[[sub]][array] to vara[sub,array] for use with old code ---------
  nroi <- dim(corr[[1]])[1]
  nscans <- length(corr)
  wgraphs <- array(rep(NaN, nroi*nroi*nscans), c(nroi, nroi, nscans))

  counter <- 1
  for (subject in 1:nscans) {
    wgraphs[,,counter] <- corr[[subject]]
    counter <- counter + 1
  }

  if (rank == TRUE) {
    wgraphs <- rank_matrices(wgraphs)
  }

  D <- log(distance(wgraphs))
  discrstat <- discr(rdf(D, ids))

  kdeobj <- kde_subject(D, ids)
  kde_dist <- data.frame(x=kdeobj[[1]]$y, y=kdeobj[[2]]$y, distance=kdeobj[[1]]$x)
  colnames(kde_dist) <- c("intra", "inter", "distance")
  meltkde <- melt(kde_dist, id="distance")
  colnames(meltkde) <- c("distance", "Relationship", "Probability")

  distance_plot <- ggplot(melt(D), aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    scale_fill_gradientn(colours=c("darkblue","blue","purple","green","yellow"),
                         name="log dist") +
    xlab("Scan") + ylab("Scan") + ggtitle(sprintf('Distance Matrix between Pairs of Scans, d=%.4f', discrstat))
  kde_plot <- ggplot() +
    geom_ribbon(data=meltkde, aes(x=distance, ymax=Probability, fill=Relationship), ymin=0, alpha=0.5) +
    ggtitle('Density Estimate of Scan Distances')
  dual_plot <- multiplot(distance_plot, kde_plot, layout=matrix(c(1,2), nrow=1, byrow=TRUE))
  return(list(discr = discrstat, dist = D, distplot = distance_plot, kdeplot = kde_plot, combinedplot = dual_plot))
}
