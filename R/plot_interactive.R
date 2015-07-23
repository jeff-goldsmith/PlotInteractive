#' Interactive Plotting for Functional Data
#' 
#' Generic function for interactive plotting of functional data analysis results
#' 
#' @param x object to be plotted. Currenlty, allowed data types are \code{fpca} and \code{fosr}.
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
#' 
#' ##### FPCA Example #####
#' 
#' data(cd4)
#' fpca.cd4 = fpca(cd4, var=TRUE)
#' plot_interactive(fpca.cd4)
#' 
#' 
#' ##### FoSR Example #####
#' 
#' data(DTI)
#' DTI = subset(DTI, select = c(cca, case, pasat))
#' DTI = DTI[complete.cases(DTI),]
#' DTI$gender = factor(sample(c("male","female"), dim(DTI)[1], replace = TRUE))
#' DTI$status = factor(sample(c("RRMS", "SPMS", "PPMS"), dim(DTI)[1], replace = TRUE))
#' 
#' fosr.dti1 = fosr_gls(cca ~ pasat, data = DTI)
#' plot_interactive(fosr.dti1)
#' 
#' fosr.dti2 = fosr_gls(cca ~ pasat * gender + status, data = DTI)
#' plot_interactive(fosr.dti2)
#' 
#' 
#' ##### FoSR Example with outliers #####
#' 
#' DTI$cca[1,] = DTI$cca[1,] + .4
#' DTI$cca[2,] = DTI$cca[2,] + .4
#' 
#' fosr.dti3 = fosr_gls(cca ~ pasat * gender + status, data = DTI)
#' plot_interactive(fosr.dti3)
plot_interactive <- function(x, ...){
  UseMethod("plot_interactive")
}