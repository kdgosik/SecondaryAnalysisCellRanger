library(tidyverse)
library(tidytext)
library(cellranger)
library(cellrangerRkit)
library(rstan)

gbm <- load_cellranger_matrix( "data" )
use_genes <- get_nonzero_genes(gbm)
e <- exprs(gbm[use_genes, ])

# writing as a tibble
e_tbl <- tibble(cell = e@Dimnames[[2]][e@j + 1],
                gene = e@Dimnames[[1]][e@i + 1],
                n = e@x) %>%
  group_by(cell) %>%
  mutate(total_genes = sum(n))


## tf_idf
e_tbl <- e_tbl %>%
  bind_tf_idf(gene, cell, n)
e_tbl




## add columns for 
# tissue, cell-state, cell-type, subject, experiment



## rstan for hierarchicl mixed modeling for posterior probabilities

[Reference](https://github.com/stan-dev/example-models/wiki/ARM-Models-Sorted-by-Type)
# Multilevel models using tf_idf as response
  ## probably treat it as gaussian response first

## think about priors for each group 


# pbmc_subset <- e_tbl %>%
#   sample_n(size = 500)

## using lme4 package first

library(lme4)

fit_lmer <- lmer(tf_idf ~ 1 + (1 | cell) + (1 | gene), 
                 data = e_tbl, 
                 subset = sample(NROW(e_tbl), 50000))

## or vary genes within cells, not enough observations

fit2_lmer <- lmer(tf_idf ~ 1 + (1 | cell) + (1 | cell:gene), 
                 data = e_tbl, 
                 subset = sample(NROW(e_tbl), 50000))


library(rstan)
library(ggplot2)

## following along with https://github.com/stan-dev/example-models/blob/master/ARM/Ch.13/13.5_Non-NestedModels.R

pbmc_subset <- e_tbl[sample(NROW(e_tbl), 100), ]
first_cells <- unique(e_tbl$cell)
first_genes <- unique(e_tbl$gene)
pbmc_subset <- e_tbl %>%
  filter(cell %in% first_cells[1:5] &
           gene %in% first_genes[1:50])

# check combos
pbmc_subset %>%
  select(cell, gene, tf_idf) %>%
  spread(cell, tf_idf, fill = 0)

select_genes <- c("ENSG00000065978", "ENSG00000077549", "ENSG00000142676", "ENSG00000142937", "ENSG00000177954")
pbmc_subset <- pbmc_subset %>%
  filter(gene %in% select_genes)

y <- as.numeric(scale(pbmc_subset$tf_idf))
n.cell <- length(unique(pbmc_subset$cell))
n.gene <- length(unique(pbmc_subset$gene))
cell.id <- as.numeric(factor(pbmc_subset$cell))
gene.id <- as.numeric(factor(pbmc_subset$gene))

## Model fit
## M1 <- lmer (y ~ 1 + (1 | group.id) + (1 | scenario.id))
dataList.1 <- list(N = length(y),
                   y = y,
                   n_cells = n.cell, 
                   n_genes = n.gene,
                   cell_id = cell.id,
                   gene_id = gene.id)

pbmc.sf1 <- stan(file='src/pbmc.stan', data = dataList.1, iter = 20000, chains = 4)
print(pbmc.sf1, pars = c("gamma","delta", "mu", "sigma_y", "lp__"))


# check combos
y.mat.new <- pbmc_subset %>%
  select(cell, gene, tf_idf) %>%
  spread(cell, tf_idf, fill = 0)

rnames <- y.mat.new$gene
y.mat.new <- y.mat.new %>%
  select(-gene) %>%
  as.matrix()

rownames(y.mat.new) <- rnames
n.cell <- 5
n.gene <- 5

image (y.mat.new, col = gray((1:11)/12), xaxt = "n", yaxt = "n")
axis (2, seq(0,1, length = n.cell), colnames(y.mat.new), tck=0, cex.axis=1.2)
axis (3, seq(0,1,length=n.gene), rnames, tck=0, cex.axis=1.2)
for (x in seq(.5,n.cell-.5,1)/(n.cell-1)) lines (c(-10,10),rep(x,2),col="white", lwd=.5)
for (x in seq(.5,n.gene-.5,1)/(n.scenario-1)) lines (rep(x,2),c(-10,10),col="white", lwd=.5)

