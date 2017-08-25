
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
    
    if( input$input_data != "Example" ){
        # path to cell ranger output
      home <- normalizePath("/") # normalizes home path
    
        # gets cellranger path from the inputed directory
      cellranger_pipestance_path <- file.path(home, 
                                              paste(unlist(input$file_path$path[-1]), 
                                              collapse = .Platform$file.sep))

    }else{
      cellranger_pipestance_path <- "data"
    }
    
      # loads gene - barcode matrix
    gbm <- load_cellranger_matrix(cellranger_pipestance_path)
    
      # normalize nonzero genes
    use_genes <- get_nonzero_genes(gbm)
    gbm_bcnorm <- normalize_barcode_sums_to_median(gbm[use_genes, ])
    gbm_log <- log_gene_bc_matrix(gbm_bcnorm, base = 10)
    
      # loads analysis results
    analysis_results <- load_cellranger_analysis_results(cellranger_pipestance_path)
      # tSNE projects from analysis results
    tsne_proj <- analysis_results$tsne
  
      # returns list of the outputs needed for plots
    return(list(gbm = gbm, 
                gbm_log = gbm_log,
                tsne_proj = tsne_proj))

  })
  
    # when input directory is selected updates gene symbols name for selection
  observeEvent(!is.null(outs) | !is.null(input$file_path), {
    
    updateSelectizeInput(session = session, 
                         inputId = "gene_symbol",
                         label = "Select Gene Symbols",
                         choices = fData(outs()[["gbm"]])$symbol)
    
  })
  
    # output plotly version of tSNE plot
  output$genePlot <- renderPlotly({
    
      # if not genes are provide, displays total counts
    if( is.null(input$gene_symbol) ){
      
      visualize_umi_counts(gbm = outs()[["gbm"]], 
                           projection = outs()[["tsne_proj"]][c("TSNE.1", "TSNE.2")],
                           limits = input$plot_limits)
      
    }else{
      
        # display plot by genes provided  
      visualize_gene_markers(gbm = outs()[["gbm_log"]],
                             gene_probes = input$gene_symbol,
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
  
  output$heatmap <- renderPlot({
    
    example_K <- 5 # number of clusters (use "Set3" for brewer.pal below if example_K > 8)
    example_col <- rev(brewer.pal(example_K,"Set2")) # customize plotting colors
    cluster_result <- analysis_results$clustering[[paste("kmeans", example_K,"clusters",sep="_")]]
    # sort the cells by the cluster labels
    cells_to_plot <- order_cell_by_clusters(gbm, cluster_result$Cluster)
    # order the genes from most up-regulated to most down-regulated in each cluster
    prioritized_genes <- prioritize_top_genes(gbm, cluster_result$Cluster, "sseq", min_mean=0.5)
    
    # create values and axis annotations for pheatmap
    gbm_pheatmap(log_gene_bc_matrix(gbm), prioritized_genes, cells_to_plot,
                 n_genes=3, colour=example_col, limits=c(-1,2))
    
  })
})
