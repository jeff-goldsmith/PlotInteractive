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
  #################
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
  ##################
  # Tab 2 
  #################
  scree <- as.data.frame(cbind(1:fpca.obj$npc,fpca.obj$evalues)); colnames(scree) <- c("k", "lambda")
  
  scree.cum = rep(NA, fpca.obj$npc) 
  for(i in 1:fpca.obj$npc){scree.cum[i] = sum(fpca.obj$evalues[1:i])/sum(fpca.obj$evalues)}
  scree.cum <- as.data.frame(cbind(1:fpca.obj$npc,scree.cum)); colnames(scree.cum) <- c("k", "lambda")
  
  ##################
  # Tab 3 
  #################
  #dat2 = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu+sqrt(fpca.obj$evalues[1])*fpca.obj$efunctions[,1]))
  #dat3 = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu-sqrt(fpca.obj$evalues[1])*fpca.obj$efunctions[,1]))
  
  
  #################
  shinyApp(
    ui = fluidPage(
      h3("Interactive plot of FPCA"),
      fluidRow(
        tabsetPanel(type = "tabs",
                    tabPanel("score plot",
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
                    ),
                    tabPanel("scree plot", 
                             column(3, h2("Scree Plots"), hr(),
                                    helpText("Scree plots are displayed in the right panel. These plots are used to visualize
                                             which principal components explain the most variability in the data.")
                                    ),
                             column(9,
                                    h4("Here are some scree plots"),
                                    plotOutput('scree1'), br(),
                                    plotOutput('scree2')
                             )                                                          
                             ),
                    tabPanel("PC subtracted from mu plot", 
                             column(3,
                                    selectInput("PCchoice", label = h3("Select PC"), choices = 1:fpca.obj$npc, selected = 1)
                             ),
                             column(9, h4("some stuff goes here"),
                                    plotOutput('muPCplot')
                             )
                    ),
                    tabPanel("score extrema", h4("Pizza tastes better than Soylent"))
      )
      )
      
    ),
    server = function(input, output) {
      
      ## use call statement outside of reactive to make list of input[[PC[i]]] thing? 
      mu = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu))
      efunctions = fpca.obj$efunctions; sqrt.evalues = diag(sqrt(fpca.obj$evalues))
      
      test = efunctions %*% sqrt.evalues
      
      #fpca.obj$efunctions %*% diag(sqrt(fpca.obj$evalues))
      
      ############################
      # Reactive code for Tab 1
      ############################
      dataInput <- reactive({
        ## use an apply function here to make the code cleaner
        PCweights = rep(NA, length(PCs))
        for(i in 1:length(PCs)){
          PCweights[i] = input[[PCs[i]]]
        }
        as.data.frame(cbind(1:length(fpca.obj$mu), as.matrix(fpca.obj$mu)+efunctions %*% sqrt.evalues %*% PCweights ))
      })
      
      #############################
      # Reactive Code for Tab 3
      #############################   
      
      dataInput2 <- reactive({
        PCchoice = as.numeric(input$PCchoice)
        #sqrt(fpca.obj$evalues[PCchoice])*fpca.obj$efunctions[,PCchoice])
        
        #sqrt.evalues[PCchoice]*efunctions[,PCchoice]
        test[,PCchoice]
        
        #dat2 = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu+dataInput2()))
        #as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu-test[,PCchoice]))
        
        
      
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
      
      output$scree1 <- renderPlot(
        ggplot(scree, aes(x=k, y=lambda))+geom_line(linetype=1, lwd=1.5, color="black")+
          geom_point(size = 4, color = "black")+ theme_bw()+
          labs(list(title = "Scree Plot", x = "kth Principal Component", y = "Eigenvalue"))          
      )
      
      output$scree2 <- renderPlot(
        ggplot(scree.cum, aes(x=k, y=lambda))+geom_line(linetype=1, lwd=1.5, color="black")+
          geom_point(size = 4, color = "black")+theme_bw()+
          labs(list(title = "Scree Plot: Cumulative Variance", x = "kth Principal Component", y = "proportion of variance explained"))          
      )
      
      output$muPCplot <- renderPlot(
        ###############################################################
        
        #dat2 = as.data.frame(cbind(d, fpca.obj$mu+sqrt(fpca.obj$evalues[input$PCchoice])*fpca.obj$efunctions[,input$PCchoice]))
        #dat3 = as.data.frame(cbind(d, fpca.obj$mu-sqrt(fpca.obj$evalues[input$PCchoice])*fpca.obj$efunctions[,input$PCchoice]))
        
        #dat2 = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu+dataInput2()))
        #dat3 = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu-dataInput2()))
        
        
        ggplot(mu, aes(x=V1, y=V2))+geom_line(lwd=2)+theme_bw()+
          geom_point(data=as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu+dataInput2())),color = "blue", size = 5, shape = '+')+
          #geom_point(data=dat3, color = "red", size = 5, shape = "-")+
          labs(list(title=bquote(psi[.(input$PCchoice)]~(t) ~ "," ~.(100*round(fpca.obj$evalues[PCnum]/sum(fpca.obj$evalues),3)) ~ "% Variance"), 
                    x = "Time", y = "Systolic Blood Pressure"))
        
        
        ###############################################################
        
      )
    }
  )
}
