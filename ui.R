
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("scripts/ModularUMItSNEPlot.R")
source("scripts/ModularClusterExplore10x.R")
# source("scripts/ModularReadCellRanger.R")

shinyUI(
  fluidPage(

  # Application title
  titlePanel("Gene Expression Explore"),
  
  # Sidebar with a selector for genes
  sidebarLayout(
    
    sidebarPanel(
      # ReadCellRangerUI("read_10x")
      
      radioButtons("input_data", "Select Input Source", choices = c("Example", "Select Directory")),
      conditionalPanel(
        condition = "input.input_data == 'Select Directory'",
        shinyDirButton(id = "file_path",
                       label = "Cellranger Pipestance Path",
                       title = "Button")
      ), # conditionalPanel
      actionButton("read_data", "Read Data")
      
    ), # sidebarPanel
    
    # Show the t-SNE plot
    mainPanel(
      tabsetPanel(
        
        tabPanel(title = "tSNE",
          UMItSNEPlotUI("tSNE")
          ), # tabPanel
        
        tabPanel(title = "Cluster",
          ClusterExplore10xUI("cluster_explore")
          ) # tabPanel
        
      ) # tabsetPanel
      
      ) # mainPanel
    
    ) # sidebarLayout
  
  ) # fluidPage
  
) # shinyUI
