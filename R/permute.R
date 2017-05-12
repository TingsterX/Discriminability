#' Discriminability Permutation Test
#'
#' A function that permutes the labels of a disctance matrix to obtain an empirical pvalue associated with whether the raw score is due
#' to random chance.
#'
#' @param D[nxn]: the distance matrix to run a permutation test for. An [nxn] matrix.
#' @param labels[n]: the labels organized appropriately with the distance matrix. Label 1 should correspond to the first column, 2 the second, and so on.
#' @param nperm=1000: the number of permutations to perform.
#' @return scores: a list of the permutated discriminabiility scores.
#' @return s0: the score before any permutations are applied.
#' @return pval: the pvalue associated with the permutation test.
#' @author Eric Bridgeford
#' @export
discr_perm_test <- function(D, labels, nperm=1000) {
  d0 <- discr(rdf(D, labels))
  discrs <- numeric(nperm)

  for (i in 1:nperm) {
    print(i)
    label_perm <- permute(labels)
    discrs[i] <- discr(rdf(D, label_perm))
  }

  pval <- signif((sum(discrs <= -abs(d0)) + sum(discrs >= abs(d0)))/1000)
  return(list(scores=discrs, s0=d0, pval=pval))
}
