## require()  for required libraries for module
library(ggplot2)
library(plotly)
library(Seurat)

# MODULE UI
IdentifytSNEUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  tagList(
    
    div(
        plotly::plotlyOutput(ns("plot1"))
        ), # div
    
    div(
        shiny::verbatimTextOutput(ns("transform"))
        ) # div
    
    ) # tagList

}



# MODULE Server
IdentifytSNEServer <- function(input, output, session, obj) {
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  
  # outputs plotly version of tSNE plot
  output$plot1 <- plotly::renderPlotly({
   obj() <- RunTSNE(object = obj(), dims.use = 1:10, do.fast = TRUE)
   select.cells <- TSNEPlot(object = obj(), do.identify = TRUE)
   # select.cells <- FeaturePlot(object = obj(), do.identify = TRUE)    
  })
  
  # printing out the number of non-zero results
  output$transform <- renderPrint({
    
   head(select.cells)
    
  })
  
  
  # # when input directory is selected updates gene symbols name for selection
  # observeEvent(!is.null(outs()), {
  #   
  #   updateSelectizeInput(session = session, 
  #                        inputId = "gene_symbol",
  #                        label = "Select Gene Symbols",
  #                        choices = fData(outs()[["gbm_log"]])$symbol)
  #   
  # })
  

}