
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

shinyUI(fluidPage(

  # Application title
  titlePanel("Gene Expression Explore"),
  
  # Sidebar with a selector for genes
  sidebarLayout(
    sidebarPanel(
      shinyDirButton(id = "file_path", 
                     label = "Cellranger Pipestance Path",
                     title = "Button"),
      
      selectizeInput(inputId = "gene_symbol", 
                     label = "Select Gene Symbols", 
                     choices = "",
                     multiple = TRUE)
    ),
    
    # Show the t-SNE plot
    mainPanel(
      plotlyOutput("genePlot")
    )
  )
))
