#' Interactive Plotting for Functional Data
#' 
#' Generic function for interactive plotting of functional data analysis results
#' 
#' @param x object to be plotted. Currenlty, only allowed data type is "fpca".
#' @param ... additional arguments passed to plotting functions
#' 
#' @author Jeff Goldsmith \email{jeff.goldsmith@@columbia.edu}, 
#' Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#' @seealso \code{\link{plot.interactive.fpca}}
#' @export
#' 
#' @examples
#' 
#' library(refund)
#' 
#' data(cd4)
#' fpca.cd4 = fpca(cd4, var=TRUE)
#' plot_interactive(fpca.cd4)
#' 
#' data(DTI)
#' fpca.dti = fpca(DTI$cca)
#' plot_interactive(fpca.dti)
#' 
#' 
plot_interactive <- function(x, ...){
  UseMethod("plot_interactive")
}