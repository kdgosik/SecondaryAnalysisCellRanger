## require()  for required libraries for module
library(ggplot2)
library(plotly)
library(cellranger)
library(cellrangerRkit)

# MODULE UI
ClusterExplore10xUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  fillCol(
    div(
      shiny::sliderInput(inputId = ns("num_clusters"), 
                       label = "Number of Clusters", 
                       min = 2, max = 10, 
                       value = 5, step = 1)
      ), # div
      div(
        plotOutput(ns("cluster_plot"))
        ), # div
      div(
        sliderInput(inputId = "n_genes", 
                    label = "Number of Genes", 
                    min = 1, max = 20, 
                    value = 3, step = 1),
        sliderInput(inputId = "hm_limits",
                    label = "Heatmap Limits",
                    min = -5, max = 5,
                    value = c(-1, 2), step = 0.5),
        plotOutput(ns("pheatmap"))
        ) # div
    ) # fillCol

}



# MODULE Server
ClusterExplore10xServer <- function(input, output, session, outs) {
  
  
  cluster_result <- reactive({
  
    outs()[["clustering"]][[paste("kmeans", input$num_cluster, "clusters", sep = "_")]]
    
  })
  
  output$cluster_plot <- renderPlot({
    
    example_col <- rev(brewer.pal(input$num_cluster, ifelse(example_K < 9, "Set2", "Set3"))) # customize plotting colors
    visualize_clusters(cluster_result()$Cluster,tsne_proj[c("TSNE.1","TSNE.2")],
                       colour = example_col)

  })
  
  output$pheatmap <- renderPlot({
    
    example_col <- rev(brewer.pal(input$num_cluster, ifelse(example_K < 9, "Set2", "Set3"))) # customize plotting colors
    # sort the cells by the cluster labels
    cells_to_plot <- order_cell_by_clusters(outs()[["gbm"]], cluster_result()$Cluster)
    # order the genes from most up-regulated to most down-regulated in each cluster
    prioritized_genes <- prioritize_top_genes(outs()[["gbm"]], cluster_result()$Cluster, "sseq", min_mean=0.5)
    
    gbm_pheatmap(log_gene_bc_matrix(outs()[["gbm"]]), 
                 genes_to_plot = prioritized_genes, 
                 cells_to_plot = cells_to_plot,
                 n_genes = input$n_genes, 
                 colour = example_col, 
                 limits = c(-1,2))
    
  })
    


  # 
  # ## Work into module
  # output_folder <-"/path_to_your_local_folder/pbmc_data_public/pbmc3k/gene_sets"
  # write_cluster_specific_genes(prioritized_genes, output_folder, n_genes=10)
  
  
  # create values and axis annotations for pheatmap
}