
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
library(cellranger)
library(cellrangerRkit)
source("scripts/ModularUMItSNEPlot.R")
source("scripts/ModularClusterExplore10x.R")
# source("scripts/ModularReadCellRanger.R")

shinyServer(function(input, output, session) {
  
  # defines root directory for the user
  shinyDirChoose(input, 'file_path', roots = c(root = '/'))

  # gets the path to the cellranger_pipestance_path
  cellranger_pipestance_path <- reactive({

    path <- "data"

  if( input$input_data != "Example" ) {
    # path to cell ranger output
    home <- normalizePath("/") # normalizes home path

    # gets cellranger path from the inputed directory
    path <- file.path(home,
                      paste(unlist(input$file_path$path[-1]), collapse = .Platform$file.sep))

  }

    return(path)

  })

  outs <- eventReactive(input$read_data, {

    selected_path <- cellranger_pipestance_path()

    # loads gene - barcode matrix
    gbm <- load_cellranger_matrix( selected_path )

    # normalize nonzero genes
    use_genes <- get_nonzero_genes(gbm)
    gbm_bcnorm <- normalize_barcode_sums_to_median(gbm[use_genes, ])
    gbm_log <- log_gene_bc_matrix(gbm_bcnorm, base = 10)

    # loads analysis results
    analysis_results <- load_cellranger_analysis_results(cellranger_pipestance_path())

    # returns list of the outputs needed for plots
    return(list(gbm = gbm,
                gbm_log = gbm_log,
                tsne_proj = analysis_results$tsne, # tSNE projects from analysis results
                clustering = analysis_results$clustering) # clustering from analysis results
    )

  })

  # when input directory is selected updates gene symbols name for selection
  observeEvent(!is.null(outs()), {

    updateSelectizeInput(session = session,
                         inputId = "gene_symbol",
                         label = "Select Gene Symbols",
                         choices = fData(outs()[["gbm_log"]])$symbol)

  })
  
  # outs <- callModule(module = ReadCellRangerServer, id = "read_10x")
  
    # calling ModularUMItSNEPlot.R functions
  callModule(module = UMItSNEPlotServer, 
            id = "tSNE", 
            outs = outs)
  
  callModule(module = ClusterExplore10xServer,
             id = "cluster_explore",
             outs = outs)
  
})
