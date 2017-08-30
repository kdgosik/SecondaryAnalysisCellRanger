
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
source("scripts/ModularUMItSNEPlot.R")
source("scripts/ModularClusterExplore10x.R")
source("scripts/ModularReadCellRanger.R")

shinyServer(function(input, output, session) {
  
    # calling ModularReadCellRanger
  outs <- callModule(module = ReadCellRangerServer, id = "10x_path")
  
    # calling ModularUMItSNEPlot.R functions
  callModule(module = UMItSNEPlotServer, 
            id = "tSNE", 
            outs = outs)
  
  callModule(module = ClusterExplore10xServer,
             id = "cluster_explore",
             outs = outs)
  
})
