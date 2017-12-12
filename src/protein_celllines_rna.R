rm(list = ls()); gc(reset = TRUE)
library(data.table)
library(tidyverse)
library(igraph)
library(visNetwork)
library(d3heatmap)
library(jsonlite)
library(magrittr)

data_path <- file.path("~/Documents/data")

gene_locations <- fread(dir(data_path, pattern = "GeneName", full.names = TRUE))
chr18 <- unique(gene_locations[`Chromosome/scaffold name` == 18, `Gene stable ID`])

trna_celline <- fread(dir(data_path, pattern = "transcript_rna_celline.tsv", full.names = TRUE))

dat <- as.data.frame(trna_celline[ensgid %in% chr18, ])
k <- 1
out <- list()
for( g in chr18[1:10] ) {
  trs <- dat[dat$ensgid == g, "enstid"]
  other_trs <- dat[dat$ensgid != g, "enstid"]
  for( tr in trs ) {
    for( otr in other_trs ) {
      t1 <- unlist(dat[dat$enstid == tr, 3:117, drop = TRUE])
      t2 <- unlist(dat[dat$enstid == otr, 3:117, drop = TRUE])
      out[[k]] <- c(tr, otr, cor(t1,t2))
      k <- k + 1
    }
  }
}

protein_atlas <- fread(dir(data_path, "proteinatlas", full.names = TRUE))

protein_gene <- fread(dir(data_path, "nextprot_ensg", full.names = TRUE), header = FALSE)


## toJSON for flare graph

long <- trna_celline %>% gather(ensgid, enstid)
colnames(long)[3:4] <- c("cell_line", "TPM")
long <- long %>% 
  dplyr::filter( TPM > 0 ) %>%
  dplyr::mutate( cell_line = gsub("\\.[[:alpha:]][[:digit:]]{2,3}", "", cell_line) ) %>%
  dplyr::group_by( cell_line, enstid ) %>%
  dplyr::mutate( TPM = mean(TPM) ) %>%
  unique %>%
  dplyr::left_join({
    gene_locations %>% 
      dplyr::select(ensgid = `Gene stable ID`, 
                    chromosome = `Chromosome/scaffold name`, 
                    start = `Transcript start (bp)`,
                    end = `Transcript end (bp)`)
    }) %>%
  unique



