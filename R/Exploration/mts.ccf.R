setClass("CCFMatrix", representation = "list", S3methods = T)

ccf.matrix = function(mts, include0=F, ...) {
    numSeries = ncol(mts)
    x = matrix(0, numSeries, numSeries)
    lagorder = matrix(0, numSeries, numSeries)
    diag(x) = 1
    for (i in 1:numSeries) {
	    if(i == numSeries) break
	    for (j in (i+1):numSeries) {
  		    mts.ccf = ccf(mts[,i], mts[,j], plot=F, ...)$acf[,1,]
            if (include0)
                index = which.max(abs(mts.ccf)) 
            else {
                ccf.order = order(abs(mts.ccf))
                index = ccf.order[length(ccf.order)]
                if(index == (length(mts.ccf) + 1 )/ 2)
                    index = ccf.order[length(ccf.order)-1]
            }
    	    x[j,i] = x[i,j] = mts.ccf[index]
            lagorder[j,i] = lagorder[i,j] = -(length(mts.ccf)-1)/2 + index - 1
	   }
   }
   x = as.data.frame(x)
   colnames(x) = colnames(mts)
   rownames(x) = colnames(mts)
   lagorder = as.data.frame(lagorder)
   colnames(lagorder) = colnames(mts)
   rownames(lagorder) = colnames(mts)
   return (structure(list(ccfMatrix = x, peakLag = lagorder), class="CCFMatrix"))
}
 
plot.CCFMatrix = function(ccfmatrix, ...) {
    require(ggplot2)
    ccfmatrix.ccf = ccf.matrix(ccfmatrix, ...)
    item1 = rep(colnames(ccfmatrix$ccfMatrix), each=ncol(ccfmatrix$ccfMatrix))
    item2 = rep(colnames(ccfmatrix$ccfMatrix), ncol(ccfmatrix$ccfMatrix))
    ccfmatrix.ccf = melt(ccfmatrix.ccf$ccfMatrix)
    ccfmatrix.ccf = cbind(ccfmatrix.ccf$ccfMatrix, item1, item2)
    ggplot(ccfmatrix.ccf, aes(x=item1, y=item2)) + geom_tile(aes(fill=value),
           colour='white') + scale_fill_gradient(low = 'white', high='steelblue') +
           xlab("Series") + ylab("Series") + 
           opts(title="Maximum Absolute Cross Correlation")
}
