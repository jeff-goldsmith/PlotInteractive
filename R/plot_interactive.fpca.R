#' Interactive Plotting for Functional Principal Component Analysis
#' 
#' Produces an interactive plot illustrating a functional principal component 
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
#' fpca.cd4 = fpca(cd4, var=TRUE)
#' plot_interactive(fpca.cd4)
#' 
plot_interactive.fpca = function(fpca.obj, xlab = "", ylab="", title = "") {
    
  ################################
  ## code for processing tabs
  ################################
  
  ## Tab 2: scree plot
  
  scree <- data.frame(k = rep(1:fpca.obj$npc, 2), 
                      lambda = c(fpca.obj$evalues, cumsum(fpca.obj$evalues)/ sum(fpca.obj$evalues)),
                      type = rep(c("Eigenvalue", "Percent Variance Explained"), each = fpca.obj$npc))
  
  ## Tab 3: linear combination of PCs
  
  varpercent = lapply(fpca.obj$evalues, function(i){100*round(i/sum(fpca.obj$evalues),3)}) # calculates percent variance explained
  calls <- as.list(rep(NA, fpca.obj$npc))
  PCs <- rep(NA, fpca.obj$npc)
  for(i in 1:fpca.obj$npc){
    
    PCnum = paste("PC", i, sep="")
    
    calls[[i]] =  eval(call("sliderInput", inputId= PCnum, label = paste(PCnum, ": ", varpercent[[i]],  "% Variance", sep=""),
                            min = -2, max = 2, step = .1, value = 0, post = " SD"))
    
    PCs[i] = PCnum
  }  
  
  ## Tab 4: extreme subjects
  
  rows = 1:dim(fpca.obj$score)[1]
  scoreIDs = cbind(rows, fpca.obj$score)
  
  #################################
  ## App
  #################################
  
  shinyApp(
    
  #################################
  ## UI
  #################################
    
    ui = navbarPage(title = strong(style = "color: #ACD6FF; padding: 0px 0px 10px 10px; opacity: 0.95; ", "FPCA Plot"), windowTitle = "PlotInteractive", 
                    collapsible = FALSE, id = "nav",
                    inverse = TRUE, header = NULL,
                    tabPanel("Mean +/- FPCs", icon = icon("stats", lib = "glyphicon"),
                             column(3,
                                    selectInput("PCchoice", label = h4("Select FPC"), choices = 1:fpca.obj$npc, selected = 1),
                                    hr(),
                                    helpText("Solid black line indicates population mean. For the selected FPC, blue and red lines 
                                             indicate the populations mean +/- the FPC times 2 SDs of the associated score distribution.")
                                    ),
                             column(9, h4("Mean and FPCs"),
                                    plotOutput('muPCplot')
                                    )
                            ),
                    tabPanel("Scree Plot", icon = icon("medkit"),
                             column(3, hr(),
                                    helpText("Scree plots are displayed to the right. The first panel shows the plot of eigenvalues, and 
                                             the second plot shows the cumulative percent variance explained.")
                                    ),
                             column(9, h4("Scree Plots"), 
                                    plotOutput('scree')
                                    )     
                            ),
                    tabPanel("PC Linear Combinations",
                             withMathJax(),
                             column(3,
                                      h4("FPC Score Values"),
                                      eval(calls),
                                      hr(),
                                      helpText("Sliders indicate FPC score values in SDs of the score distribution; plot shows the linear 
                                             combination of mean and FPCs with the specified scores.")
                                    ),
                             column(9, h4("Linear Combination of Mean and FPCs"), 
                                      plotOutput('fpca_plot')
                                    )
                             ),
                    tabPanel("Score Extrema",
                             column(3,
                                      selectInput("PCchoice2", label = h4("Select PC"), choices = 1:fpca.obj$npc, selected = 1),
                                      hr(),
                                      helpText("Observed data and fitted values for the curves with the smallest and largest scores 
                                                for the selected principal component.")
                                    ),
                             column(9, h4("Subjects with extreme score values"),
                                      plotOutput("extrema")
                                    )
                             )
                    ),
    
    #################################
    ## Server
    #################################

    server = function(input, output){
      
      mu = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu))
      efunctions = fpca.obj$efunctions; sqrt.evalues = diag(sqrt(fpca.obj$evalues))      
      scaled_efunctions = efunctions %*% sqrt.evalues
      
      ## Reactive Code for Tab 2

      dataInput2 <- reactive({
        PCchoice = as.numeric(input$PCchoice)
        scaled_efunctions[,PCchoice]
      })
      
      ## Reactive code for Tab 3

      dataInput <- reactive({
        PCweights = rep(NA, length(PCs))
        for(i in 1:length(PCs)){PCweights[i] = input[[PCs[i]]]}
        
        as.data.frame(cbind(1:length(fpca.obj$mu), as.matrix(fpca.obj$mu)+efunctions %*% sqrt.evalues %*% PCweights ))
      })
      
      ## Reactive Code for Tab 4

      dataInput3 <- reactive({
        PCchoice2 = as.numeric(input$PCchoice2)
        minIDs = scoreIDs[order(scoreIDs[,(PCchoice2+1)]),][1:2,1]; maxIDs = scoreIDs[order(-scoreIDs[,(PCchoice2+1)]),][1:2,1]
        as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu, fpca.obj$Yhat[minIDs[1],], fpca.obj$Yhat[maxIDs[1],],
                            fpca.obj$Y[minIDs[1],], fpca.obj$Y[maxIDs[1],]))
      })      
      
      
      ## Tab 1 plot
      output$muPCplot <- renderPlot(
        ggplot(mu, aes(x = V1, y = V2)) + geom_line(lwd=1) + theme_bw() +
          geom_point(data = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu + 2*dataInput2())), color = "blue", size = 4, shape = '+')+
          geom_point(data = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu - 2*dataInput2())), color = "red", size = 4, shape = "-")+
          scale_x_continuous(breaks = seq(0, length(fpca.obj$mu)-1, length=6), labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1)))+
          xlab(xlab) + ylab(ylab) + ylim(c(range(fpca.obj$Yhat)[1], range(fpca.obj$Yhat)[2]))+
          ggtitle(bquote(psi[.(input$PCchoice)]~(t) ~ "," ~.(100*round(fpca.obj$evalues[as.numeric(input$PCchoice)]/sum(fpca.obj$evalues),3)) ~ "% Variance"))
      )
      
      ## Tab 2 Plots (Scree Plots)
      output$scree <- renderPlot(
        ggplot(scree, aes(x=k, y=lambda))+geom_line(linetype=1, lwd=1.5, color="black")+
          geom_point(size = 4, color = "black")+ theme_bw() + xlab("Principal Component") + ylab("") +
          facet_wrap(~type, scales = "free_y") + ylim(0, NA)
      )
      
      ## Tab 3 Plot
      output$fpca_plot <- renderPlot(       
        ggplot(mu, aes(x=V1, y=V2))+geom_line(lwd=1, aes( color= "mu"))+theme_bw()+
          scale_x_continuous(breaks = seq(0, length(fpca.obj$mu)-1, length=6), labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1)))+
          geom_line(data = dataInput(), lwd = 1.5, aes(color = "subject")) + xlab(xlab) + ylab(ylab) + ggtitle(title)+
          scale_color_manual("Line Legend", values = c(mu = "black", subject = "cornflowerblue"), guide = FALSE)+ 
          theme(legend.key = element_blank()) + ylim(c(range(fpca.obj$Yhat)[1], range(fpca.obj$Yhat)[2]))
      )
            
      ## Tab 4 plots
      output$extrema <- renderPlot(
        ggplot(data = dataInput3(), aes(x=V1,y=V2)) + geom_line(lwd=1) + theme_bw() +
          scale_x_continuous(breaks = seq(0, length(fpca.obj$mu)-1, length=6), labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1)))+
          geom_line(data = dataInput3(), aes(y=V3), size=1, color = "blue") +
          geom_line(data = dataInput3(), aes(y=V4), size=1, color = "red")+
          geom_point(data = dataInput3(), aes(y=V5), color = "blue") +
          geom_point(data = dataInput3(), aes(y=V6), color = "red")+
          xlab(xlab) + ylab(ylab) + ylim(c(range(fpca.obj$Yhat)[1], range(fpca.obj$Yhat)[2]))
      )
      
      
      
    } ## end server
    )
}

