## require()  for required libraries for module
library(ggplot2)
library(plotly)
library(cellranger)
library(cellrangerRkit)

# MODULE UI
UMItSNEPlotUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  plotly::plotlyOutput(ns("plot1")) 

}



# MODULE Server
UMItSNEPlotServer <- function(input, output, session, outs, gene_symbols) {
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  
  # outputs plotly version of tSNE plot
  output$plot1 <- plotly::renderPlotly({
    
    # if not genes are provide, displays total counts
    if( is.null(gene_symbols) ){
      
      cellrangerRkit::visualize_umi_counts(gbm = outs()[["gbm"]], 
                           projection = outs()[["tsne_proj"]][c("TSNE.1", "TSNE.2")])
      
    }else{
      
      # display plot by genes provided  
      cellrangerRkit::visualize_gene_markers(gbm = outs()[["gbm"]],
                             gene_probes = gene_symbol,
                             projection = outs()[["tsne_proj"]][c("TSNE.1", "TSNE.2")])
      
    }
    
  })

}