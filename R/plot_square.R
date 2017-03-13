#' Square plot
#'
#' A function that plots a square matrix. Good for things like Correlation matrices.
#'
#' @param signal [[n]][nt, nroi]: the signal for each of the n subjects, containing an array of nt observations for nroi rois.
#' @param title="": the title for the square plot.
#' @param xlabel="ROIs": the x label for the square plot.
#' @param ylabel="ROIs": the y label for the square plot.
#' @param legend="": the legend title for the square plot.
#' @return sqplot : a plot of the square.
#' @author Eric Bridgeford
#' @export
plot_square <- function(mtx, title="",xlabel="ROI", ylabel="ROI", legend="metric") {
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
