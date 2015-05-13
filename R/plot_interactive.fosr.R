#' Interactive Plotting for Functional-on-Scalar Regressions
#' 
#' Produces an interactive plot illustrating a function-on-scalar 
#' regression analysis. 
#' 
#' @param fosr.obj fosr object to be plotted. 
#' @param xlab x axis label
#' @param ylab y axis label
#' @param title plot title
#' 
#' @author Jeff Goldsmith \email{jeff.goldsmith@@columbia.edu}, 
#' Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#' 
#' @seealso \code{\link{plot.interactive}}
#' @import shiny
#' @import ggplot2
#' @importFrom reshape2 melt
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
#' DTI$status = factor(sample(c("RRMS", "SPMS", "PPMS"), dim(DTI)[1], replace = TRUE))
#'
#' fosr.dti = fosr_gls(cca ~ pasat + gender, data = DTI)
#' 
#' plot_interactive(fosr.dti)
#' 
plot_interactive.fosr = function(fosr.obj, xlab = "", ylab="", title = "") {
    
  ################################
  ## code for processing tabs
  ################################
  
  p = dim(fosr.obj$beta.hat)[1]
  D = dim(fosr.obj$beta.hat)[2]
  grid = 1:D
  
  ## Tab 1: fitted values
  
  pred.list = names(attributes(terms(fosr.obj$terms))$dataClasses)[-1]
  calls <- vector("list", length(pred.list))
  for(i in 1:length(pred.list)){
    
    calls[[i]] =  eval(createInputCall(pred.list[i], get(pred.list[i], fosr.obj$data) ))
    
  }  
  
  ## Tab 2: coefficient functions
  
  coef.list = colnames(model.matrix(fosr.obj$terms, fosr.obj$data[1,]))
  coefInputValues = 1:p
  names(coefInputValues) = coef.list
  
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
                                    h4("Predictor Values"),
                                    eval(calls),
                                    hr(),
                                    helpText("Use inputs to select predictor values; a fitted response curve for a subject with those values is
                                             plotted to the right.")
                             ),
                             column(9, h4("Fitted Value"), 
                                    plotOutput('FittedValPlot')
                             )
                    ),
                    tabPanel("Coefficient Functions",
                             column(3, selectInput("CoefChoice", label = h4("Select Coefficient"), choices = coefInputValues, selected = 1),
                                    hr(),
                                    helpText("Coefficient functions and confidence bounds for the selected predictor.")
                             ),
                             column(9, h4(""), 
                                    plotOutput('coef')
                            )     
                    ),
                    tabPanel("Residuals", icon = icon("medkit"),
                             column(3,
                                    hr(),
                                    helpText("Plot of residual curves.")
                             ),
                             column(9, h4(""), 
                                    plotOutput('resid')
                             )     
                    )
                    ),
    
    #################################
    ## Server
    #################################

    server = function(input, output){

      ## Reactive Code for Tab 1
      
      dataInput <- reactive({
        
        variables = sapply(pred.list, function(u) {input[[u]]})
        
        input.data = fosr.obj$data[1,]
        
        reassign = function(var, newdata){
          if(is.numeric(fosr.obj$data[,var])){ 
            var.value = as.numeric(newdata[var]) 
#          } else if(is.factor(fosr.obj$data[,var]) & length(levels(fosr.obj$data[,var])) <=2){ 
#            var.value = factor(levels(fosr.obj$data[,var])[newdata[var]+1], levels = levels(fosr.obj$data[,var])) 
          } else if(is.factor(fosr.obj$data[,var])){ 
            var.value = factor(newdata[var], levels = levels(fosr.obj$data[,var])) 
          }
          var.value
        }

        input.data[,pred.list] = lapply(pred.list, reassign, variables)
        
        X.design = t(matrix(model.matrix(fosr.obj$terms, input.data)))
        fit.vals = as.vector(X.design %*% fosr.obj$beta.hat)
        data.frame(grid = grid,
                   fit.vals = fit.vals)
      })
      
      ## Reactive Code for Tab 2
      
      dataInput3 <- reactive({
        CoefChoice = as.numeric(input$CoefChoice)
        data.frame(grid = grid,
                   coef = fosr.obj$beta.hat[CoefChoice,],
                   UB =  fosr.obj$beta.UB[CoefChoice,],
                   LB = fosr.obj$beta.LB[CoefChoice,])
      })      
      

      ## Code for Tab 3
      
      response = fosr.obj$data[,names(attributes(terms(fosr.obj$terms))$dataClasses)[1]]
      resid = response - fosr.obj$Yhat
      colnames(resid) = grid
      resid.m = melt(resid)
      colnames(resid.m) = c("subj", "grid", "residual")
      
      ## Tab 1 plot
      
      output$FittedValPlot <- renderPlot(
        ggplot(dataInput(), aes(x = grid, y = fit.vals)) + geom_line(lwd=1) + theme_bw() +
          xlab(xlab) + ylab(ylab) + ylim(range(fosr.obj$Yhat))
      )
            
      ## Tab 2 Plot
      output$coef <- renderPlot(
        ggplot(dataInput3(), aes(x=grid, y=coef))+geom_line(linetype=1, lwd=1.5, color="black")+
          geom_line(data = dataInput3(), aes(y=UB), color = "blue") +
          geom_line(data = dataInput3(), aes(y=LB), color = "blue")+
          theme_bw() + xlab("") + ylab("") 
      )
      
      ## Tab 3 Plot
      output$resid <- renderPlot(
        ggplot(resid.m, aes(x=grid, y=residual, group = subj)) + geom_line(alpha = .3, color="black") +
          theme_bw() + xlab("") + ylab("") 
      )
      
    } ## end server
    )
}

