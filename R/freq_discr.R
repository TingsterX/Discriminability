#' Frequency Discriminability
#'
#' A function that performs discriminability in the frequency domain.
#'
#' @param signal [[n]][nt, nroi]: the signal for each of the n subjects, containing an array of nt observations for nroi rois.
#' @param ids [n]: the ids for each scan corresponding to the signal from above.
#' @param tr=2.5 [1]: the repetition time of the dataset. NULL for none.
#' @param lc=0.01 [1]: the lower cutoff for highpass filtering. NULL for none.
#' @param spec='amp' : the spectrum to work with. 'amp' for amplitude, 'pow' for power.
#' @param rank=FALSE : a boolean indicating whether to do unranked (FALSE) or ranked (TRUE).
#' @param fsize=10: the default font size for the plot text.
#' @return d [1]: the discriminability statistic for the data.
#' @return dist [n, n]: the distance matrix associated with the data.
#' @return distplot : a plot of the distance matrix.
#' @return kdeplot : a plot of the density estimate of the intra vs inter subject distances.
#' @return combinedplot : a multiplot showing the dist plot and the kde plot.
#' @author Eric Bridgeford
#' @export
freq_discr <- function(signal, ids, tr, lc, spec='amp', rank=FALSE, fsize=15) {
  require(reshape2)
  require(Rmisc)
  require(ggplot2)
  if (spec == 'amp') {
    spec_sig <- obs2amp(signal, tr = tr, lc=lc)
  } else if (spec == 'pow') {
    spec_sig <- obs2pow(signal, tr = tr, lc=lc)
  }
  div <- freq2div(spec_sig)
  if (rank==TRUE) {
    div <- sapply(div, function(x) array(rank(x), dim=dim(x)), USE.NAMES=TRUE, simplify=FALSE)
  }
  D <- div2dist(div)
  discrspec <- discr(rdf(D, ids))

  kdeobj <- kde_subject(D, ids)
  kde_dist <- data.frame(x=kdeobj[[1]]$y, y=kdeobj[[2]]$y, distance=kdeobj[[1]]$x)
  colnames(kde_dist) <- c("intra", "inter", "distance")
  meltkde <- melt(kde_dist, id="distance")
  colnames(meltkde) <- c("distance", "Relationship", "Probability")

  distance_plot <- ggplot(melt(D), aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    scale_fill_gradientn(colours=c("darkblue","blue","purple","green","yellow"),name="distance") +
    xlab("Scan") +
    ylab("Scan") +
    ggtitle(sprintf('Distance Matrix between Pairs of Scans, d=%.4f', discrspec)) +
    theme(text=element_text(size=fsize))
  kde_plot <- ggplot() +
    geom_ribbon(data=meltkde, aes(x=distance, ymax=Probability, fill=Relationship), ymin=0, alpha=0.5) +
    ggtitle('Density Estimate of Scan Distances') +
    theme(text=element_text(size=fsize))
  dual_plot <- multiplot(distance_plot, kde_plot, layout=matrix(c(1,2), nrow=1, byrow=TRUE))
  return(list(d=discrspec, dist=D, distplot=distance_plot, kdeplot = kde_plot, combinedplot=dual_plot))
}
