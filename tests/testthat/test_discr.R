library(Discriminability)
context('Tests for discriminability code')
require(fmriutils)

#-----Basic Tests----------------------------------------------#
nroi <- 2
n <- 2
nscan <- 2
graphs <- array(NaN, dim=c(nroi, nroi, n*nscan))
# two random graphss
g1 <- array(runif(nroi*nroi), dim=c(nroi, nroi))
g2 <- array(runif(nroi*nroi), dim=c(nroi, nroi))
graphs[,,1] <- g1
graphs[,,2] <- g2
graphs[,,3] <- g1
graphs[,,4] <- g2
graphs <- fmriu.array2list(graphs)
test_that("Discriminability in perfect match case works effectively", {
  labels <- c(0, 1, 0, 1)
  discr.discr(discr.rdf(graphs, labels))
})
