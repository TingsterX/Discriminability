#' Frequency to Divergence
#'
#' a function that converts lists of frequency domain signals to divergence matrices.
#'
#' @param freq [[n]][nbin, nroi] a list of n frequency spectrum signals with nbin bins and nroi rois.
#' @return div_mtx [[n]][nroi, nroi] a list of n divergence matrices where each edge is a divergence between two pairs of rois.
#' @keywords frequency, divergence
#' @export
freq2div <- function(freq) {
  sapply(freq, function(x) {
    nroi <- dim(x)[2]
    div_mtx <- array(NaN, dim=c(nroi, nroi))
    for (roi1 in 1:nroi) {
      for (roi2 in 1:nroi) {
        div_mtx[roi1, roi2] <- kl_div(x[,roi1, drop=FALSE], x[,roi2, drop=FALSE])
      }
    }
    return(div_mtx)
  }, USE.NAMES=TRUE, simplify=FALSE)
}
