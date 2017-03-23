#' Square plot
#'
#' A function that plots a square matrix. Good for things like Correlation matrices.
#'
#' @param in [nroi, nroi] or [nt, nroi]: the input. Can either be a square matrix of featuresxfeatures, or an array of observationsxfeatures (see itype parameter).
#' @param title="": the title for the square plot.
#' @param xlabel="ROIs": the x label for the square plot.
#' @param ylabel="ROIs": the y label for the square plot.
#' @param legend="": the legend title for the square plot.
#' @param itype="sq": the shape of the input. If "sq", the plot will be generated as is for the input signal. If "ts", we will assume the input is observationsxfeatures, and will correlate the features first.
#' @return sqplot : a plot of the square.
#' @author Eric Bridgeford
#' @export
plot_square <- function(mtx, title="",xlabel="ROI", ylabel="ROI", legend="metric", itype="sq") {
  if (itype == "ts") {
    mtx <- cor(mtx)  # if a timeseries is passed in, correlate the features first
  }
  require(reshape2)
  require(ggplot2)
  sqplot <- ggplot(melt(mtx), aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    scale_fill_gradientn(colours=c("darkblue","blue","purple","green","yellow"), name=legend) +
    xlab(xlabel) +
    ylab(ylabel) +
    ggtitle(title)
  return(sqplot)
}
