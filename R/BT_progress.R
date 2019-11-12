#' Progress bar for multiple BT analyses
#'
#' A progress bar for BayesTraits analyses
#' 
#' @param  ... Not requiered
#' @export

BT_progress <- function(...)	{
  vectOfBar <- c(...)*100
  numOfBar <- length(vectOfBar)
  graphics::plot(c(0,100), c(0,numOfBar), type='n', xlab='', ylab='', yaxt='n', mar=c(3,3,3,3))
  for(i in 1:numOfBar) {
    graphics::rect(0, 0.1+i-1, vectOfBar[i], 0.9+i-1, col=grDevices::rainbow(numOfBar)[i])
    graphics::text(0.5, 0.5+i-1, paste('Status ', i, ': ', round(vectOfBar[i],2), '%', sep=''), adj=0)
  }
  graphics::title('Progress...')
}
