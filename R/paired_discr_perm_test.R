#' Paired Discriminability Permutation Test
#'
#' A function that permutes the rdf values associated with the classes in two distance matrices to determine whether a difference in discriminability score between the two matrices is significant.
#'
#' @param D1[nxn]: the first distance matrix to run a permutation test for. An [nxn] matrix.
#' @param D2[nxn]: the second distance matrix to run a permutation test for. An [nxn] matrix.
#' @param labels1[n]: the first labels organized appropriately with the first distance matrix. Label 1 should correspond to the first column, 2 the second, and so on.
#' @param labels2[n]: the second labels organized appropriately with the second distance matrix. Label 1 should correspond to the first column, 2 the second, and so on.
#' @param nperm=1000: the number of permutations to perform.
#' @return scores: a list of the permutated discriminability scores.
#' @return s0: the score before any permutations are applied.
#' @return emp_discr: the empirical discriminability score.
#' @return pval: the pvalue associated with the permutation test.
#' @author Eric Bridgeford
#' @export
paired_discr_perm_test <- function(D1, D2, labels1, labels2, nperm=1000, verbose=FALSE) {
  require('gtools')
  rdf1 <- rdf(D1, labels1)
  rdf2 <- rdf(D2, labels2)
  d0 <- discr(rdf1) - discr(rdf2)
  discrs <- numeric(nperm)
  rdfs <- c(rdf1, rdf2)

  for (i in 1:nperm) {
    if (verbose) {
      print(i)
    }
    perm_rdfs <- permute(rdfs)
    perm1 <- perm_rdfs[1:length(rdf1)]
    perm2 <- perm_rdfs[(length(rdf1)+1):length(perm_rdfs)]
    discrs[i] <- discr(perm1) - discr(perm2)
  }

  pval <- signif((sum(discrs <= -abs(d0)) + sum(discrs >= abs(d0)))/nperm)
  return(list(scores=discrs, s0=d0, emp_discr=mean(discrs), pval=pval))
}
