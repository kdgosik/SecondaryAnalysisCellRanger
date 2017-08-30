
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
source("scripts/ModularUMItSNEPlot.R")
source("scripts/ModularClusterExplore10x.R")

shinyServer(function(input, output, session) {
  
    # defines root directory for the user
  shinyDirChoose(input, 'file_path', roots = c(root = '/'))
  
    # gets the path to the cellranger_pipestance_path
  outs <- reactive({
    
    cellranger_pipestance_path <- "data"
    
    if( input$input_data != "Example" ){
        # path to cell ranger output
      home <- normalizePath("/") # normalizes home path
    
        # gets cellranger path from the inputed directory
      cellranger_pipestance_path <- file.path(home, 
                                              paste(unlist(input$file_path$path[-1]), 
                                              collapse = .Platform$file.sep))

    }
    
      # loads gene - barcode matrix
    gbm <- load_cellranger_matrix(cellranger_pipestance_path)
    
      # normalize nonzero genes
    use_genes <- get_nonzero_genes(gbm)
    gbm_bcnorm <- normalize_barcode_sums_to_median(gbm[use_genes, ])
    gbm_log <- log_gene_bc_matrix(gbm_bcnorm, base = 10)
    
      # loads analysis results
    analysis_results <- load_cellranger_analysis_results(cellranger_pipestance_path)
  
      # returns list of the outputs needed for plots
    return(list(gbm = gbm, 
                gbm_log = gbm_log,
                tsne_proj = analysis_results$tsne, # tSNE projects from analysis results
                clustering = analysis_results$clustering) # clustering from analysis results
           ) 

  })
  
    # when input directory is selected updates gene symbols name for selection
  observeEvent(!is.null(outs) | !is.null(input$file_path), {
    
    updateSelectizeInput(session = session, 
                         inputId = "gene_symbol",
                         label = "Select Gene Symbols",
                         choices = fData(outs()[["gbm_log"]])$symbol)
    
  })
  
    # calling ModularUMItSNEPlot.R functions
  callModule(module = UMItSNEPlotServer, 
            id = "tSNE", 
            outs = outs, 
            gene_symbols = reactive({input$gene_symbol}))
  
  callModule(module = ClusterExplore10xServer,
             id = "cluster_explore",
             outs = outs)
    
  output$heatmap <- renderPlot({
    
    example_K <- 3 # number of clusters (use "Set3" for brewer.pal below if example_K > 8)
    example_col <- rev(brewer.pal(example_K,"Set2")) # customize plotting colors
    cluster_result <- outs()[["clustering"]][[paste("kmeans", example_K,"clusters",sep="_")]]
    
    # sort the cells by the cluster labels
    cells_to_plot <- order_cell_by_clusters(outs()[["gbm"]], cluster_result$Cluster)
    
    # order the genes from most up-regulated to most down-regulated in each cluster
    prioritized_genes <- prioritize_top_genes(outs()[["gbm"]], cluster_result$Cluster, "sseq", min_mean=0.5)
    
    # create values and axis annotations for pheatmap
    gbm_pheatmap(gbm = log_gene_bc_matrix(outs()[["gbm"]]), 
                 genes_to_plot = prioritized_genes, 
                 cells_to_plot = cells_to_plot,
                 n_genes = 3, 
                 colour = example_col, 
                 limits = c(-1, 2))
    
  })
  
})
