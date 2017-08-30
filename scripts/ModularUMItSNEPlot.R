## require()  for required libraries for module
library(ggplot2)
library(plotly)
library(cellranger)
library(cellrangerRkit)

# MODULE UI
UMItSNEPlotUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  fillCol(
    column(width = 12,
      shiny::sliderInput(inputId = ns("plot_limits"), 
                       label = "Value Limits", 
                       min = 0, max = 10, 
                       value = c(0, 4), step = 0.5)
      ), # column
    column(width = 12,
        plotly::plotlyOutput(ns("plot1"))
        ), # column
    column(width = 12,
        shiny::verbatimTextOutput(ns("transform"))
        ) # column
    ) # fillCol

}



# MODULE Server
UMItSNEPlotServer <- function(input, output, session, outs, gene_symbols) {
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  
  # outputs plotly version of tSNE plot
  output$plot1 <- plotly::renderPlotly({
    
    # if not genes are provide, displays total counts
    if( is.null(gene_symbols()) ){
      
      cellrangerRkit::visualize_umi_counts(gbm = outs()[["gbm"]], 
                           projection = outs()[["tsne_proj"]][c("TSNE.1", "TSNE.2")],
                           limits = input$plot_limits)
      
    }else{
      
      # display plot by genes provided  
      cellrangerRkit::visualize_gene_markers(gbm = outs()[["gbm_log"]],
                             gene_probes = gene_symbols(),
                             projection = outs()[["tsne_proj"]][c("TSNE.1", "TSNE.2")],
                             limits = input$plot_limits)
      
    }
    
  })
  
  # printing out the number of non-zero results
  output$transform <- renderPrint({
    
    paste("After transformation, the gene-barcode matrix contains", 
          dim(outs()[["gbm_log"]])[1], "genes for",  
          dim(outs()[["gbm_log"]])[2], "cells")
    
  })
  

}