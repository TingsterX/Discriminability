#' Timeseries plot
#'
#' A function that plots a timeseries.
#'
#' @param ts[nt, nroi]: the signal, containing an array of nt observations for nroi rois.
#' @param title="": the title for the timeseries plot.
#' @param xlabel="TRs": the x label for the timeseries plot.
#' @param ylabel="intensity": the y label for the timeseries plot.
#' @param legend="ROIs": the legend title for the timeseries plot.
#' @return tsplot : a plot of the timeseries.
#' @author Eric Bridgeford
#' @export
plot_timeseries <- function(ts, title="",xlabel="TRs", ylabel="intensity", legend="ROI") {
  require(reshape2)
  require(ggplot2)
  tsm = melt(ts)
  colnames(tsm) <- c(xlabel, legend, ylabel)
  tsplot <- ggplot(data=tsm, aes_string(x=xlabel, y=ylabel, group=legend, color=legend)) +
    geom_line(alpha=.2) +
    scale_y_continuous() +
    ylab(ylabel) +
    xlab(xlabel) +
    ggtitle(title)
  return(tsplot)
}
