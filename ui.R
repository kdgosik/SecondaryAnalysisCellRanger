
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyFiles)
library(plotly)
library(cellranger)
library(cellrangerRkit)
source("scripts/ModularUMItSNEPlot.R")
source("scripts/ModularClusterExplore10x.R")
source("scripts/ModularReadCellRanger.R")

shinyUI(
  fluidPage(

  # Application title
  titlePanel("Gene Expression Explore"),
  
  # Sidebar with a selector for genes
  sidebarLayout(
    sidebarPanel(
      
      # ReadCellRangerUI("10x_path")
      
      radioButtons("input_data", "Select Input Source", choices = c("Example", "Select Directory")),
      
      conditionalPanel(
        condition = "input.input_data == 'Select Directory'",
        shinyDirButton(id = "file_path", 
                       label = "Cellranger Pipestance Path",
                       title = "Button")
      )
      
    ),
    
    # Show the t-SNE plot
    mainPanel(
      tabsetPanel(
        tabPanel(title = "tSNE",
          UMItSNEPlotUI("tSNE")
        ),
        tabPanel(title = "Cluster",
          ClusterExplore10xUI("cluster_explore")
                 ),
        tabPanel(title = "Heatmap",
          plotOutput("heatmap")
        )
      )
      
      ) # mainPanel
    
    ) # sidebarLayout
  
  ) # fluidPage
  
) # shinyUI
