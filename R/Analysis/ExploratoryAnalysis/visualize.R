########################################################
# Exploratory Time Series Visualizations
########################################################

plot.gts <- function(data, ix.locations = NULL, ix.times = NULL){
  # Plot a fastVAR formatted time series
  #
  # Args:
  #  data: A fastVAR formatted time series. Rows correspond to
  #        times and columns correspond to locations.
  #  ix.locations: A vector containing columns of data to plot.
  #  ix.times: A vector containing times of the data to plot.
  require(ggplot2)
  require(reshape2)
  if(!(is.null(ix.locations) && is.null(ix.locations))){
    data <- data[ix.times, ix.locations]
  }
  p <- ggplot(melt(data)) + geom_line(aes(y = value, x = Var1, col = Var2))
  return(p)
}
