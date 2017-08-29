
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

shinyUI(
  fluidPage(

  # Application title
  titlePanel("Gene Expression Explore"),
  
  # Sidebar with a selector for genes
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("input_data", "Select Input Source", choices = c("Example", "Select Directory")),
      
      conditionalPanel(
        condition = "input.input_data == 'Select Directory'",
        shinyDirButton(id = "file_path", 
                       label = "Cellranger Pipestance Path",
                       title = "Button")
      ),
      
      selectizeInput(inputId = "gene_symbol", 
                     label = "Select Gene Symbols", 
                     choices = "",
                     multiple = TRUE),
      
      sliderInput(inputId = "plot_limits",
                  label = "Value Limits",
                  min = 0,
                  max = 10,
                  value = c(0, 4),
                  step = 0.5)
    ),
    
    # Show the t-SNE plot
    mainPanel(
      plotlyOutput("genePlot"),
      verbatimTextOutput("transform"),
      # UMItSNEPlotUI("tSNE"),
      plotOutput("heatmap")
      ) # mainPanel
    
    ) # sidebarLayout
  
  ) # fluidPage
  
) # shinyUI
