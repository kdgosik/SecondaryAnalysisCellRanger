
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

  # if cellranger R kit is not installed then install it
if( !{"cellrangerRkit" %in% installed.packages()} ) {
  source("http://cf.10xgenomics.com/supp/cell-exp/rkit-install-2.0.0.R")
}

  # Load packages
library(shiny)
library(shinyFiles)
library(plotly)
library(cellranger)
library(cellrangerRkit)

shinyServer(function(input, output, session) {
  
    # defines root directory for the user
  shinyDirChoose(input, 'file_path', roots = c(root = '/'))
  
    # gets the path to the cellranger_pipestance_path
  outs <- reactive({
    
    # path to cell ranger output
    home <- normalizePath("/") # normalizes home path
    
      # gets cellranger path from the inputed directory
    cellranger_pipestance_path <- file.path(home, 
                                            paste(unlist(input$file_path$path[-1]), 
                                            collapse = .Platform$file.sep))

      # loads gene - barcode matrix
    gbm <- load_cellranger_matrix(cellranger_pipestance_path)
      # loads analysis results
    analysis_results <- load_cellranger_analysis_results(cellranger_pipestance_path)
      # tSNE projects from analysis results
    tsne_proj <- analysis_results$tsne
  
      # returns list of the outputs needed for plots
    return(list(gbm = gbm, tsne_proj = tsne_proj))

  })
  
    # when input directory is selected updates gene symbols name for selection
  observeEvent(input$file_path, {
    
    updateSelectizeInput(session = session, 
                         inputId = "gene_symbol",
                         label = "Select Gene Symbols",
                         choices = fData(outs()[["gbm"]])$symbol)
    
  })
  
    # outputs plotly version of tSNE plot
  output$genePlot <- renderPlotly({
    
      # if not genes are provide, displays total counts
    if( is.null(input$gene_symbol) ){
      
      visualize_umi_counts(gbm = outs()[["gbm"]], 
                           projection = outs()[["tsne_proj"]][c("TSNE.1", "TSNE.2")])
      
    }else{
      
        # display plot by genes provided  
      visualize_gene_markers(gbm = outs()[["gbm"]],
                             gene_probes = input$gene_symbol,
                             projection = outs()[["tsne_proj"]][c("TSNE.1", "TSNE.2")])
      
    }
    
  })
  
})
