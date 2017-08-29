{
# library(Seurat)
# library(tidyverse)
# library(magrittr)
# rm(list=ls())
# 
# ## Creating DotPlot with Marginal plots of counts
# 
#   # Original DotPlot function from Seurat package
# 
# function (object, genes.plot, cols.use = c("lightgrey", "blue"), 
#           col.min = -2.5, col.max = 2.5, dot.min = 0, dot.scale = 6, 
#           group.by, plot.legend = FALSE, do.return = FALSE, x.lab.rot = FALSE) 
# {
#   if (!missing(x = group.by)) {
#     object <- SetAllIdent(object = object, id = group.by)
#   }
#   data.to.plot <- data.frame(FetchData(object = object, vars.all = genes.plot))
#   data.to.plot$cell <- rownames(x = data.to.plot)
#   data.to.plot$id <- object@ident
#   data.to.plot <- data.to.plot %>% gather(key = genes.plot, 
#                                           value = expression, -c(cell, id))
#   data.to.plot <- data.to.plot %>% group_by(id, genes.plot) %>% 
#     summarize(avg.exp = mean(expm1(x = expression)), pct.exp = PercentAbove(x = expression, 
#                                                                             threshold = 0))
#   data.to.plot <- data.to.plot %>% ungroup() %>% group_by(genes.plot) %>% 
#     mutate(avg.exp.scale = scale(x = avg.exp)) %>% mutate(avg.exp.scale = MinMax(data = avg.exp.scale, 
#                                                                                  max = col.max, min = col.min))
#   data.to.plot$genes.plot <- factor(x = data.to.plot$genes.plot, 
#                                     levels = rev(x = sub(pattern = "-", replacement = ".", 
#                                                          x = genes.plot)))
#   data.to.plot$pct.exp[data.to.plot$pct.exp < dot.min] <- NA
#   p <- ggplot(data = data.to.plot, mapping = aes(x = genes.plot, 
#                                                  y = id)) + geom_point(mapping = aes(size = pct.exp, color = avg.exp.scale)) + 
#     scale_radius(range = c(0, dot.scale)) + scale_color_gradient(low = cols.use[1], 
#                                                                  high = cols.use[2]) + theme(axis.title.x = element_blank(), 
#                                                                                              axis.title.y = element_blank())
#   if (!plot.legend) {
#     p <- p + theme(legend.position = "none")
#   }
#   if (x.lab.rot) {
#     p <- p + theme(axis.text.x = element_text(angle = 90, 
#                                               vjust = 0.5))
#   }
#   suppressWarnings(print(p))
#   if (do.return) {
#     return(p)
#   }
# }
#   
#   ## Exploring function parts
#   
# # function params
# col.min = -2.5; col.max = 2.5; dot.min = 0; dot.scale = 6, 
# 
# genes.plot <- cd_genes <- c("CD247", "CD3E", "CD9")
#   
#   data.to.plot <- data.frame(FetchData(object = pbmc_small, vars.all = cd_genes))
#   data.to.plot$cell <- rownames(x = data.to.plot)
#   data.to.plot$id <- pbmc_small@ident
#   data.to.plot <- data.to.plot %>% gather(key = genes.plot, 
#                                           value = expression, -c(cell, id))
#   data.to.plot <- data.to.plot %>% 
#     group_by(id, genes.plot) %>% 
#     mutate(avg.exp = mean(expm1(x = expression)), 
#               pct.exp = Seurat:::PercentAbove(x = expression, threshold = 0)) %>%
#     filter(expression > 0)
#   
#   data.to.plot <- data.to.plot %>% 
#     ungroup() %>% 
#     group_by(genes.plot) %>% 
#     mutate(avg.exp.scale = scale(x = avg.exp)) %>% 
#     mutate(avg.exp.scale = Seurat:::MinMax(data = avg.exp.scale, max = col.max, min = col.min))
#   
#   data.to.plot$genes.plot <- factor(x = data.to.plot$genes.plot, 
#                                     levels = rev(x = sub(pattern = "-", 
#                                                          replacement = ".", 
#                                                          x = genes.plot)))
#   
#   data.to.plot$pct.exp[data.to.plot$pct.exp < dot.min] <- NA
# 
#   library(ggplot2)
#   library(gridExtra)
#   pMain <- ggplot(data.to.plot, aes(x = genes.plot, 
#                                     y = id, size = 
#                                     pct.exp, 
#                                     color = avg.exp.scale)) + 
#     geom_point() + theme(legend.position = "bottom")
#   
#   # hacky, using grid extra.  Doesn't line up properly
#   pTop <- ggplot(data.to.plot, aes(x = genes.plot)) + geom_histogram(stat = "count")
#   pRight <- ggplot(data.to.plot, aes(x = id)) + geom_histogram(stat="count") + coord_flip()
#   pEmpty <- ggplot(data.to.plot, aes(x = genes.plot, y = id)) +
#     geom_blank() +
#     theme(axis.text = element_blank(),
#           axis.title = element_blank(),
#           line = element_blank(),
#           panel.background = element_blank())
#   
#   grid.arrange(pTop, pEmpty, pMain, pRight,
#                ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 2.5))  
#   
#   
#   ## correct formulation
#   ggMarginal(pMain, type = "histogram", margins = "both", xparams = list(stat = "count"),
#              yparams = list(stat = "count"))
  
} 
## Writing New Function for Seurat ################
require(Seurat)
require(tidyverse)
require(ggExtra)
  
DotPlot_MarginalCount <- function (object, genes.plot, cols.use = c("lightgrey", "blue"), 
            col.min = -2.5, col.max = 2.5, dot.min = 0, dot.scale = 6, 
            group.by, plot.legend = FALSE, do.return = FALSE, x.lab.rot = FALSE) 
  {
    if (!missing(x = group.by)) {
      object <- SetAllIdent(object = object, id = group.by)
    }
    
    data.to.plot <- data.frame(FetchData(object = object, vars.all = genes.plot))
    data.to.plot$cell <- rownames(x = data.to.plot)
    data.to.plot$id <- object@ident
    data.to.plot <- data.to.plot %>% gather(key = genes.plot, 
                                            value = expression, -c(cell, id))
    data.to.plot <- data.to.plot %>% 
      group_by(id, genes.plot) %>% 
      mutate(avg.exp = mean(expm1(x = expression)), 
             pct.exp = Seurat:::PercentAbove(x = expression, threshold = 0)) %>%
      filter(expression > 0)
    
    data.to.plot <- data.to.plot %>% 
      ungroup() %>% 
      group_by(genes.plot) %>% 
      mutate(avg.exp.scale = scale(x = avg.exp)) %>% 
      mutate(avg.exp.scale = Seurat:::MinMax(data = avg.exp.scale, max = col.max, min = col.min))
    
    data.to.plot$genes.plot <- factor(x = data.to.plot$genes.plot, 
                                      levels = rev(x = sub(pattern = "-", 
                                                           replacement = ".", 
                                                           x = genes.plot)))
    
    data.to.plot$pct.exp[data.to.plot$pct.exp < dot.min] <- NA
    
    p <- ggplot(data.to.plot, aes(x = genes.plot, 
                                      y = id, size = 
                                        pct.exp, 
                                      color = avg.exp.scale)) + 
      geom_point() + 
      theme(legend.position = "bottom") +
      scale_radius(range = c(0, dot.scale)) + 
      scale_color_gradient(low = cols.use[1], high = cols.use[2]) + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())

    p <- ggMarginal(p = p, 
                    type = "histogram", 
                    margins = "both", 
                    xparams = list(stat = "count"),
                    yparams = list(stat = "count"))
    
    if (!plot.legend) {
      p <- p + theme(legend.position = "none")
    }
    if (x.lab.rot) {
      p <- p + theme(axis.text.x = element_text(angle = 90, 
                                                vjust = 0.5))
    }
    suppressWarnings(print(p))
    if (do.return) {
      return(p)
    }
}
  