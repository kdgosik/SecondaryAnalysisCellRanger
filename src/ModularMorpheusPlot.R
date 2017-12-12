## require()  for required libraries for module
library(morpheus)

# MODULE UI
MorpheusUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  tagList(
    
    div(
      shiny::sliderInput(inputId = ns("plot_limits"), 
                       label = "Value Limits", 
                       min = 0, max = 10, 
                       value = c(0, 4), step = 0.5)
      ), # div
    
    div(
      morpheus::morpheusOutput("plot1")
    )
    
    ) # tagList

}



# MODULE Server
MorpheusServer <- function(input, output, session, obj) {
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  
  # outputs morpheus plot
  output$plot1 <- morpheus::renderMorpheus({
    
    morpheus(x = obj@data, labCol = pp@meta.data)
    
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