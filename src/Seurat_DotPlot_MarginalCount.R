## Writing Updated Function for Seurat Objects ################
pks <- c("Seurat", "tidyverse", "ggplot2", "gtable", "grid", "scales")
pks <- pks[!{pks %in% installed.packages()}]
if( length(pks) > 0 ) lapply(pks, install.packages)

require(Seurat)
require(tidyverse)
require(ggplot2)
require(gtable)
require(grid)
require(scales)

DotPlot_MarginalCount <- function (object, genes.plot, cols.use = c("lightgrey", "blue"), 
                                   col.min = -2.5, col.max = 2.5, dot.min = 0, dot.scale = 6, 
                                   group.by, do.return = FALSE, x.lab.rot = FALSE) 
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
  
  
  gg_main <- ggplot(data.to.plot, aes(x = genes.plot, y = id, 
                                      size = pct.exp, color = avg.exp.scale)) + 
    geom_point() + 
    theme(legend.position = "bottom") +
    scale_radius(range = c(0, dot.scale)) + 
    scale_color_gradient(low = cols.use[1], high = cols.use[2]) + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          plot.margin = unit(c(3,3,3,3), "mm"))
  
  gg_rows <- ggplot(data.to.plot, aes(x = genes.plot)) + 
    geom_bar() +
    theme(plot.margin = unit(c(3,3,3,3), "mm"))
  
  
  gg_cols <- ggplot(data.to.plot, aes(x = id))+ 
    geom_bar() + 
    coord_flip() +
    theme(plot.margin = unit(c(3,3,3,3), "mm"))
  
  ## extract legend from DotPlot
  g <- ggplotGrob(gg_main)$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  
  ## plot DotPlot without legend
  g <- ggplotGrob(gg_main + theme(legend.position="none"))
  
  ## add column and put column barplot within
  g <- gtable_add_cols(g, unit(5,"cm"))
  g <- gtable_add_grob(g, ggplotGrob(gg_cols), t = 1, l=ncol(g), b=nrow(g), r=ncol(g))
  
  ## add row and put legend within
  g <- gtable_add_rows(g, unit(1,"cm"))
  g <- gtable_add_grob(g, legend, t = nrow(g), l=1, b=nrow(g), r=ncol(g)-1)
  
  ## add row on top and put row barplot within
  g <- gtable_add_rows(g, unit(5,"cm"), 0)
  g <- gtable_add_grob(g, ggplotGrob(gg_rows), t = 1, l=1, b=1, r=5) 
  
  grid.newpage()
  grid.draw(g)
}

