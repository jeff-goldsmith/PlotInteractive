#' Interactive Plotting for Functional-on-Scalar Regressions
#' 
#' Produces an interactive plot illustrating a function-on-scalar 
#' regression analysis. So far, there's very little here.
#' 
#' @param fosr.obj fosr object to be plotted. 
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
#' library(dplyr)
#' 
#' set.seed(101)
#' 
#' data(DTI)
#' DTI = subset(DTI, select = c(cca, case, pasat))
#' DTI = DTI[complete.cases(DTI),]
#' DTI$gender = sample(c(0,1), dim(DTI)[1], replace = TRUE)
#' 
#' fosr.dti = fosr_gls(cca ~ pasat + gender, data = DTI)
#' 
#' plot_interactive(fosr.dti)
#' 

plot_interactive.fosr = function(fosr.obj, xlab = "", ylab="", title = "") {
    
  ################################
  ## code for processing tabs
  ################################
  
  ## Tab 1: fitted values
  
  ## Tab 2: coefficient functions
  p = dim(fosr.obj$beta.hat)[1]
  D = dim(fosr.obj$beta.hat)[2]
  grid = 1:D
  
  #################################
  ## App
  #################################
  
  shinyApp(
    
  #################################
  ## UI
  #################################
    
    ui = navbarPage(title = strong(style = "color: #ACD6FF; padding: 0px 0px 10px 10px; opacity: 0.95; ", "FoSR Plot"), windowTitle = "PlotInteractive", 
                    collapsible = FALSE, id = "nav",
                    inverse = TRUE, header = NULL,
                    tabPanel("Fitted Values", icon = icon("stats", lib = "glyphicon"),
                             column(3,
                                    hr(),
                                    helpText("This plot is a work in progress.")
                                    ),
                             column(9, h4("Fitted Value"),
                                    plotOutput('FittedValPlot')
                                    )
                            ),
                    tabPanel("Coefficient Functions", icon = icon("medkit"),
                             column(3, selectInput("CoefChoice", label = h4("Select Coefficient"), choices = 1:p, selected = 1),
                                    hr(),
                                    helpText("Coefficient functions and confidence bounds.")
                                    ),
                             column(9, h4(""), 
                                    plotOutput('coef')
                                    )     
                            )
                    ),
    
    #################################
    ## Server
    #################################

    server = function(input, output){

      mu = data.frame(grid = grid,
                      mu = fosr.obj$beta.hat[1,])
      
      ## Reactive Code for Tab 2
      
      dataInput3 <- reactive({
        CoefChoice = as.numeric(input$CoefChoice)
        data.frame(grid = grid,
                   coef = fosr.obj$beta.hat[CoefChoice,],
                   UB =  fosr.obj$beta.UB[CoefChoice,],
                   LB = fosr.obj$beta.LB[CoefChoice,])
      })      
      
        
      
      
      ## Tab 1 plot
      output$FittedValPlot <- renderPlot(
        ggplot(mu, aes(x = grid, y = mu)) + geom_line(lwd=1) + theme_bw() +
          xlab(xlab) + ylab(ylab)
      )
      
      ## Tab 2 Plot
      output$coef <- renderPlot(
        ggplot(dataInput3(), aes(x=grid, y=coef))+geom_line(linetype=1, lwd=1.5, color="black")+
          geom_line(data = dataInput3(), aes(y=UB), size=1, color = "blue") +
          geom_line(data = dataInput3(), aes(y=LB), size=1, color = "blue")+
          theme_bw() + xlab("") + ylab("") 
      )
      
    } ## end server
    )
}

