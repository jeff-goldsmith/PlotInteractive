#' Interactive Plotting for Functional Principal Components Analysis
#' 
#' Produces an interactive plot illustrating a functional principal components 
#' analysis.
#' 
#' @param fpca.obj fpca object to be plotted. 
#' @param xlab x axis label
#' @param ylab y axis label
#' @param title plot title
#' 
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu},
#' Jeff Goldsmith \email{jeff.goldsmith@@columbia.edu} 
#' 
#' @seealso \code{\link{plot.interactive}}
#' @import shiny
#' @import ggplot2
#' @export
#' 
#' @examples
#' 
#' library(refund)
#' data(cd4)
#' fpca.cd4 = fpca.sc(cd4, var=TRUE)
#' plot_interactive.fpca(cd4)
#' 
plot_interactive.fpca = function(fpca.obj, xlab = "", ylab="", title = "") {
  
  varpercent = lapply(fpca.obj$evalues, function(i){100*round(i/sum(fpca.obj$evalues),3)}) # calculates percent variance explained
  calls <- as.list(rep(NA, fpca.obj$npc))
  PCs <- rep(NA, fpca.obj$npc)
  for(i in 1:fpca.obj$npc){
    
    PCnum = paste("PC", i, sep="")
    
    calls[[i]] =  eval(call("sliderInput", inputId= PCnum, label = paste(PCnum, ": ", varpercent[[i]],  "% Variance", sep=""),
                            min = -2, max = 2, step = .1, value = 0, format = "#.# SD"))
                  PCs[i] = PCnum
  }  
    
  shinyApp(
    ui = fluidPage(
      h3("Interactive plot of FPCA"),
      fluidRow(
        column(3,
               h4("FPC Score Values"),
               "[Sliders indicate FPC score values in SDs of the score distribution.]",
               hr(),
               eval(calls)
        ),
        column(9, 
               h4("Mean and Fitted Curve Given Scores"),
               plotOutput('fpca_plot')
        )
      )
      
    ),
    server = function(input, output) {
      
      ## use call statement outside of reactive to make list of input[[PC[i]]] thing? 
      mu = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu))
      efunctions = fpca.obj$efunctions
      sqrt.evalues = diag(sqrt(fpca.obj$evalues))
      
      dataInput <- reactive({
        
        ## use an apply function here to make the code cleaner
        PCweights = rep(NA, length(PCs))
        for(i in 1:length(PCs)){
          PCweights[i] = input[[PCs[i]]]
        }
        
        as.data.frame(cbind(1:length(fpca.obj$mu), as.matrix(fpca.obj$mu)+efunctions %*% sqrt.evalues %*% PCweights ))
        
      })
      
      output$fpca_plot <- renderPlot(         
        
        ############################################################
        ggplot(mu, aes(x=V1, y=V2))+geom_line(lwd=2, aes( color= "mu"))+theme_bw()+
          scale_x_continuous(breaks=seq(0,length(fpca.obj$mu)-1, length=6), labels = paste0(c(0,0.2,0.4,0.6,0.8,1)))+
          geom_line(data=dataInput(),lwd = 1.5, aes(color = "subject"))+
          xlab(xlab)+ylab(ylab)+ggtitle(title)+
          scale_color_manual("Line Legend", values = c(mu = "cornflowerblue", subject = "lightcoral"),
                             labels = c("Mean", "Subject"))+ 
          theme(legend.key = element_blank())+
          ylim(c(range(fpca.obj$Yhat)[1],range(fpca.obj$Yhat)[2]))
        ###################################################

      )
    }
  )
}