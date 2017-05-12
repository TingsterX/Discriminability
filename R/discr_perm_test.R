#' Discriminability Permutation Test
#'
#' A function that permutes the labels of a distance matrix to obtain an empirical pvalue associated with whether the raw score is due to random chance.
#'
#' @param D[nxn]: the distance matrix to run a permutation test for. An [nxn] matrix.
#' @param labels[n]: the labels organized appropriately with the distance matrix. Label 1 should correspond to the first column, 2 the second, and so on.
#' @param nperm=1000: the number of permutations to perform.
#' @return discrs: a list of the permutated discriminability scores.
#' @return d0: the score before any permutations are applied.
#' @return emp_discr: the empirical discriminability score.
#' @return pval: the pvalue associated with the permutation test.
#' @author Eric Bridgeford
#' @export
discr_perm_test <- function(D, labels, nperm=1000, verbose=FALSE) {
  require('gtools')
  s0 <- discr(rdf(D, labels))
  discrs <- numeric(nperm)

  for (i in 1:nperm) {
    if (verbose) {
      print(i)
    }
    label_perm <- permute(labels)
    discrs[i] <- discr(rdf(D, label_perm))
  }

  pval <- signif((sum(discrs <= -abs(d0)) + sum(discrs >= abs(d0)))/nperm)
  return(list(discrs=discrs, d0=d0, emp_discr=mean(discrs), pval=pval))
}
