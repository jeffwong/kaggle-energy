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

ccf.matrix = function(mts) {
   numSeries = ncol(mts)
   x = matrix(0, numSeries, numSeries)
   diag(x) = 1
   for (i in 1:numSeries) {
	   if(i == numSeries) break
	   for (j in (i+1):numSeries) {
		   mts.ccf = ccf(mts[,i], mts[,j], plot=F)$acf[,1,]
		   x[j,i] = x[i,j] = mts.ccf[which.max(abs(mts.ccf))]
	   }
   }
   x = as.data.frame(x)
   colnames(x) = colnames(mts)
   rownames(x) = colnames(mts)
   return (x)
}

plot.ccf.matrix = function(mts) {
    require(ggplot2)
    mts.ccf = ccf.matrix(mts)
    item1 = rep(colnames(mts), each=ncol(mts))
    item2 = rep(colnames(mts), ncol(mts))
    mts.ccf = melt(mts.ccf)
    mts.ccf = cbind(mts.ccf, item1, item2)
    ggplot(mts.ccf, aes(x=item1, y=item2)) + geom_tile(aes(fill=value),
           colour='white') + scale_fill_gradient(low = 'white', high='steelblue') +
           xlab("Series") + ylab("Series") +
           opts(title="Maximum Absolute Cross Correlation")
}
