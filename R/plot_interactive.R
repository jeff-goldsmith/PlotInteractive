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
#' library(dplyr)
#' 
#' ##### FPCA Example #####
#' 
#' data(cd4)
#' fpca.cd4 = fpca(cd4, var=TRUE)
#' plot_interactive(fpca.cd4)
#' 
#' 
#' ##### FPCA Example #####
#' 
#' data(DTI)
#' DTI = subset(DTI, select = c(cca, case, pasat))
#' DTI = DTI[complete.cases(DTI),]
#' DTI$gender = factor(sample(c("male","female"), dim(DTI)[1], replace = TRUE))
#' DTI$status = factor(sample(c("RRMS", "SPMS", "PPMS"), dim(DTI)[1], replace = TRUE))
#' 
#' fosr.dti = fosr_gls(cca ~ pasat * gender + status, data = DTI)
#' 
plot_interactive <- function(x, ...){
  UseMethod("plot_interactive")
}