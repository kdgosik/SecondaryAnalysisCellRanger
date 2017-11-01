## require()  for required libraries for module
library(ggplot2)
library(plotly)
library(cellranger)
library(cellrangerRkit)
library(shinycssloaders)

# MODULE UI
ClusterExplore10xUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  tagList(
    
    div(
      shiny::sliderInput(inputId = ns("num_clusters"), 
                       label = "Number of Clusters", 
                       min = 2, max = 10, 
                       value = 5, step = 1)
      ), # div
    
    div(
      withSpinner(shiny::plotOutput(ns("cluster_plot")))
      ), # div
    
    div(
      shiny::numericInput(inputId = ns("n_genes"), 
                         label = "Number of Genes", 
                         value = 3,
                         min = 1, max = 20,
                         step = 1)
      ), # div
    
    div(
      shiny::sliderInput(inputId = ns("hm_limits"),
                         label = "Heatmap Limits",
                         min = -5, max = 5,
                         value = c(-1, 2), step = 0.5)
      ), # div
    
    div(
      shiny::plotOutput(ns("pheatmap"))
      ) # div
    
    ) # tagList

}



# MODULE Server
ClusterExplore10xServer <- function(input, output, session, outs) {
  
  
  cluster_result <- reactive({
  
    outs()[["clustering"]][[paste("kmeans", input$num_clusters, "clusters", sep = "_")]]
    
  })
  
  example_col <- reactive({
    rev(brewer.pal(input$num_clusters, ifelse(input$num_clusters < 9, "Set2", "Set3"))) # customize plotting colors
  })
  
  output$cluster_plot <- renderPlot({
    
    visualize_clusters(cluster_result = cluster_result()$Cluster,
                       projection = outs()[["tsne_proj"]][c("TSNE.1","TSNE.2")],
                       colour = example_col())

  })
  
  output$pheatmap <- renderPlot({
    
    # sort the cells by the cluster labels
    cells_to_plot <- order_cell_by_clusters(outs()[["gbm"]], cluster_result()$Cluster)
    # order the genes from most up-regulated to most down-regulated in each cluster
    prioritized_genes <- prioritize_top_genes(outs()[["gbm"]], cluster_result()$Cluster, "sseq", min_mean=0.5)
    
    gbm_pheatmap(log_gene_bc_matrix(outs()[["gbm"]]), 
                 genes_to_plot = prioritized_genes, 
                 cells_to_plot = cells_to_plot,
                 n_genes = input$n_genes, 
                 colour = example_col(), 
                 limits = input$hm_limits)
    
  })
    


  # 
  # ## Work into module
  # output_folder <-"/path_to_your_local_folder/pbmc_data_public/pbmc3k/gene_sets"
  # write_cluster_specific_genes(prioritized_genes, output_folder, n_genes=10)
  
  
  # create values and axis annotations for pheatmap
}