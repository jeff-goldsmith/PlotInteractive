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
#' plot_interactive.fpca(fpca.cd4)
#' 


plot_interactive.fpca = function(fpca.obj, xlab = "", ylab="", title = "") {
  header_style <- h4(style = "padding: 0px 0px 10px 10px; color: #337ab7; opacity: 0.95; ", paste("Interactive Plot of FPCA"))
  
  ################
  # Tab 1
  ################
  varpercent = lapply(fpca.obj$evalues, function(i){100*round(i/sum(fpca.obj$evalues),3)}) # calculates percent variance explained
  calls <- as.list(rep(NA, fpca.obj$npc))
  PCs <- rep(NA, fpca.obj$npc)
  for(i in 1:fpca.obj$npc){
    
    PCnum = paste("PC", i, sep="")
    
    calls[[i]] =  eval(call("sliderInput", inputId= PCnum, label = paste(PCnum, ": ", varpercent[[i]],  "% Variance", sep=""),
                            min = -2, max = 2, step = .1, value = 0, format = "#.# SD"))
    
    PCs[i] = PCnum
  }  
  #################
  # Tab 2 
  #################
  scree <- as.data.frame(cbind(1:fpca.obj$npc,fpca.obj$evalues)); colnames(scree) <- c("k", "lambda")
  
  scree.cum = rep(NA, fpca.obj$npc) 
  for(i in 1:fpca.obj$npc){scree.cum[i] = sum(fpca.obj$evalues[1:i])/sum(fpca.obj$evalues)}
  scree.cum <- as.data.frame(cbind(1:fpca.obj$npc,scree.cum)); colnames(scree.cum) <- c("k", "lambda")
  
  #################
  # Tab 4 
  #################
  rows = 1:dim(fpca.obj$score)[1]
  scoreIDs = cbind(rows, fpca.obj$score)
  
  #################
  shinyApp(
    ui = navbarPage(title = strong(style = "color: #6500FF; ", "PlotInteractive"), windowTitle = "PlotInteractive", 
                    collapsible = FALSE, id = "nav",
                    inverse = TRUE, header = header_style,
                    tabPanel("score plot", 
                             column(3,
                                    h3("FPC Score Values"),
                                    "[Sliders indicate FPC score values in SDs of the score distribution.]",
                                    hr(),
                                    eval(calls)
                             ),
                             column(9,
                                    h4("Mean and Fitted Curve Given Scores"), 
                                    plotOutput('fpca_plot')
                             )
                    ), ## end Tab 1
                    tabPanel("scree plot", 
                             fluidPage(
                              column(6,  h4("Scree Plot", align = "center"),
                                      plotOutput('scree1')
                                      
                                      ),
                              column(6, h4("Scree Plot: Cumulative Variance", align = "center"), plotOutput('scree2')
                                    ) ,
                              
                              fluidRow( 
                                column(4, br(), hr(),
                                  h3("Scree Plots"),  
                                  helpText("Scree plots are displayed in the panels above. These plots are used to visualize
                                             which principal components explain the most variability in the data.")
                                      ),
                                column(8, br(), h3("put download button here for downloading scree plots")
                                       )
                                        ) ## end fluidRow
                                      ) ## end fluidPage
                             ), ## end Tab 2
                    tabPanel("PC subtracted from mu plot",
                             column(3, h3("Raisin 3"),
                                    selectInput("PCchoice", label = h3("Select PC"), choices = 1:fpca.obj$npc, selected = 1),
                                    hr(),
                                    helpText("Solid black line indicates population mean. Blue and red lines indicate addition
                                             and subtraction, respectively, of selected principal component from population mean.")
                                    ),
                             column(9, h4("Raisin is a Mountain Poodle"),
                                    plotOutput('muPCplot')
                             )
                             ), ## end Tab 3
                    tabPanel("score extrema",
                             column(3, h3("Titley-title"),
                                    selectInput("PCchoice2", label = h3("Select PC"), choices = 1:fpca.obj$npc, selected = 1),
                                    hr(),
                                    helpText("Yhat values for two individuals with the smallest scores for the selected principal
                                             component are represented by the red curves. Yhat values for two individuals
                                             with the largest scores for the selected PC are represented by the blue curves.")
                                    ),
                             column(9, h4("Pizza tastes better than Soylent"),
                                    h5("Most extreme individuals for given PC", align = "center"), plotOutput("extrema1"),
                                    h5("Second most extreme individuals for given PC", align = "center"), plotOutput("extrema2")
                             )
                                    )
                    ), ## end Tab 4
    server = function(input, output){
      
      mu = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu))
      efunctions = fpca.obj$efunctions; sqrt.evalues = diag(sqrt(fpca.obj$evalues))      
      scaled_efunctions = efunctions %*% sqrt.evalues
      
      ############################
      # Reactive code for Tab 1
      ############################
      dataInput <- reactive({
        PCweights = rep(NA, length(PCs))
        for(i in 1:length(PCs)){PCweights[i] = input[[PCs[i]]]}
        
        as.data.frame(cbind(1:length(fpca.obj$mu), as.matrix(fpca.obj$mu)+efunctions %*% sqrt.evalues %*% PCweights ))
      })
      
      #############################
      # Reactive Code for Tab 3
      #############################         
      dataInput2 <- reactive({
        PCchoice = as.numeric(input$PCchoice)
        scaled_efunctions[,PCchoice]
      })
      
      ##################################
      # Reactive Code for Tab 4
      #################################
      dataInput3 <- reactive({
        PCchoice2 = as.numeric(input$PCchoice2)
        minIDs = scoreIDs[order(scoreIDs[,(PCchoice2+1)]),][1:2,1]; maxIDs = scoreIDs[order(-scoreIDs[,(PCchoice2+1)]),][1:2,1]
        
        as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$Yhat[minIDs[1],], fpca.obj$Yhat[minIDs[2],],
                            fpca.obj$Yhat[maxIDs[1],], fpca.obj$Yhat[maxIDs[2],]))
      })      
      
      
      ## Tab 1 Plot
      output$fpca_plot <- renderPlot(       
        ############################################################
        ggplot(mu, aes(x=V1, y=V2))+geom_line(lwd=2, aes( color= "mu"))+theme_bw()+
          scale_x_continuous(breaks=seq(0,length(fpca.obj$mu)-1, length=6), labels = paste0(c(0,0.2,0.4,0.6,0.8,1)))+
          geom_line(data=dataInput(),lwd = 1.5, aes(color = "subject"))+xlab(xlab)+ylab(ylab)+ggtitle(title)+
          scale_color_manual("Line Legend", values = c(mu = "cornflowerblue", subject = "lightcoral"),
                             labels = c("Mean", "Subject"))+ 
          theme(legend.key = element_blank())+ylim(c(range(fpca.obj$Yhat)[1],range(fpca.obj$Yhat)[2]))
        ################################################### 
      )
      
      
      ## Tab 2 Plots (Scree Plots)
      output$scree1 <- renderPlot(
        ggplot(scree, aes(x=k, y=lambda))+geom_line(linetype=1, lwd=1.5, color="black")+
          geom_point(size = 4, color = "black")+ theme_bw()+ xlab("kth Principal Component")+ylab("Eigenvalue")
      )
      output$scree2 <- renderPlot(
        ggplot(scree.cum, aes(x=k, y=lambda))+geom_line(linetype=1, lwd=1.5, color="black")+
          geom_point(size = 4, color = "black")+theme_bw()+xlab("kth Principal Component")+ylab("proportion of variance explained")
      )
      
      ## Tab 3 plot
      output$muPCplot <- renderPlot(
        ###############################################################           
        ggplot(mu, aes(x=V1, y=V2))+geom_line(lwd=2)+theme_bw()+
          geom_point(data=as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu+2*dataInput2())),color = "blue", size = 8, shape = '+')+
          geom_point(data=as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu-2*dataInput2())), color = "red", size = 8, shape = "-")+
          scale_x_continuous(breaks=seq(0,length(fpca.obj$mu)-1, length=6), labels = paste0(c(0,0.2,0.4,0.6,0.8,1)))+
          xlab(xlab)+ylab(ylab)+ylim(c(range(fpca.obj$Yhat)[1],range(fpca.obj$Yhat)[2]))+
          ggtitle(bquote(psi[.(input$PCchoice)]~(t) ~ "," 
                         ~.(100*round(fpca.obj$evalues[as.numeric(input$PCchoice)]/sum(fpca.obj$evalues),3)) ~ "% Variance"))
        ###############################################################      
      )
      
      ## Tab 4 plots
      output$extrema1 <- renderPlot(
        ######################################################################################################
        ggplot(data=dataInput3(), aes(x=V1,y=V2))+theme_bw()+
          scale_x_continuous(breaks=seq(0,length(fpca.obj$mu)-1, length=6), labels = paste0(c(0,0.2,0.4,0.6,0.8,1)))+
          geom_line(size=2, color = "red")+geom_line(data = dataInput3(), aes(x=V1, y=V4), size=2, color = "blue")+
          xlab(xlab)+ylab(ylab)+ ylim(c(range(fpca.obj$Yhat)[1],range(fpca.obj$Yhat)[2]))
        #####################################################################################################################
      )
      
      output$extrema2 <- renderPlot(
        ######################################################################################################
        ggplot(data=dataInput3(), aes(x=V1,y=V3))+theme_bw()+
          scale_x_continuous(breaks=seq(0,length(fpca.obj$mu)-1, length=6), labels = paste0(c(0,0.2,0.4,0.6,0.8,1)))+
          geom_line(size=2, color = "red")+geom_line(data = dataInput3(), aes(x=V1, y=V5), size=2, color = "blue")+
          xlab(xlab)+ylab(ylab)+ ylim(c(range(fpca.obj$Yhat)[1],range(fpca.obj$Yhat)[2]))
        #####################################################################################################################
      )
      
    } ## end server
    )
}

