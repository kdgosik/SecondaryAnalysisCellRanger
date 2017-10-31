#' Need to write a description here
#'
#' @seealso shiny
#' @export
#' @param id The namespace for the module
#' @keywords shiny
#' @import ggplot2
#' @import plotly
#' @import cellranger
#' @import cellrangerRkit
#' @importFrom shiny div
#' @importFrom shiny NS
#' @importFrom shiny sliderInput
#' @importFrom shiny plotOutput
#' @improtFrom shiny numeriInput


## require()  for required libraries for module
# if cellranger R kit is not installed then install it
if( !{"cellrangerRkit" %in% installed.packages()} ) {
  source("http://cf.10xgenomics.com/supp/cell-exp/rkit-install-2.0.0.R")
}

# Load packages
library(shiny)
library(shinyFiles)
library(cellranger)
library(cellrangerRkit)

# MODULE UI
ReadCellRangerUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  tagList(
    div(
      radioButtons(ns("input_data"), "Select Input Source", choices = c("Example", "Select Directory"))
    ),
    ### CANNOT GET IT TO FIND THE FILE PATH ########
    div(
      # conditionalPanel(
      #   condition = "input.input_data == 'Select Directory'",
        shinyDirButton(id = ns("file_path"), 
                      label = "Cellranger Pipestance Path",
                      title = "Button")
        # ) # conditionalPanel
      ),
    
    div(
      actionButton(ns("read_data"), "Read Data")
      )
    
    ) # tagList

}



#' Write another really good description but this time about the server function.
#' 
#'  think about incorporating seurat objects
#'
#' @seealso shiny
#' @export
#' @param input List-like object that stores the current values of all of the widgets in your app.
#' @param output List-like object that stores instructions for building the R objects in your app.
#' @param session List-like object about the session to be passed to the UI.
#' @param outs a reactive dataframe
#' @param ... additional paramters to be passed...
#' @keywords shiny
#' @importFrom reactive
#' @importFrom validate
#' @importFrom observe



# MODULE Server
ReadCellRangerServer <- function(input, output, session) {
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  
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
  
  return(outs)

}