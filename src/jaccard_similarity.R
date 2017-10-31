library(cellranger)
library(cellrangerRkit)
library(proxy)

## jaccard similarity ######

library(text2vec)
library(magrittr)
library(Matrix)

jaccard_similarity <- function(m) {
  A <- tcrossprod(m)
  im <- which(A > 0, arr.ind = TRUE, useNames = FALSE)
  b <- rowSums(m)
  Aim <- A[im]
  sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
    dims = dim(A)
  )
}


gbm <- load_cellranger_matrix( "data" )


A <- crossprod(exprs(gbm)) # cross product; 2700 x 2700
At <- tcrossprod(exprs(gbm)) # transpose of cross product; 32738 x 32738

use_genes <- get_nonzero_genes(gbm)
e <- exprs(gbm[use_genes, ])

# taking a dgTMatrix and returning names of the genes of non-sparse elements
e@Dimnames[[1]][e@i[e@j == 1]]

  # each list element is a cell with the entries being the genes expressed in that cell
cell_by_gene_list <- lapply(1:2700, function(id) e@Dimnames[[1]][e@i[e@j + 1 == id]])
cell_similarity_jaccard <- SuperExactTest::jaccard(cell_by_gene_list)
colnames(cell_similarity_jaccard) <- rownames(cell_similarity_jaccard) <- e@Dimnames[[2]]

library(igraph)

g <- graph_from_adjacency_matrix(cell_similarity_jaccard - diag(nrow(cell_similarity_jaccard)))
plot(g)

library(glasso)

a <- glasso(cell_similarity_jaccard, rho = 0.01, thr = 1e-3, maxit = 100, approx = TRUE)
aa <- glasso(cell_similarity_jaccard, rho = 0.02, w.init = a$w, wi.init = a$wi)

## 'Bag of Words Approach' for gene barcode matrix #############


#[tidytext example](https://cran.r-project.org/web/packages/tidytext/vignettes/tf_idf.html)
  # translate for single cell
  ## book would be a cell
  ## word would be a gene



library(dplyr)
library(janeaustenr)
library(tidytext)
library(tidyverse)
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>% group_by(book) %>% summarize(total = sum(n))
book_words <- left_join(book_words, total_words)
book_words


  ## count by word
library(ggplot2)
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

  ## tf_idf
book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

  ## arrange by tf_idf
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

## for specific book
book_words %>%
  filter(book == "Pride & Prejudice") %>%
  select(-total) %>%
  arrange(desc(tf_idf))


## translating for gene - barcode matrix ############

  # writing as a tibble
e_tbl <- tibble(cell = e@Dimnames[[2]][e@j + 1],
                gene = e@Dimnames[[1]][e@i + 1],
                n = e@x) %>%
  group_by(cell) %>%
  mutate(total_genes = sum(n))


## count by word
# library(ggplot2)
# ggplot(e_tbl, aes(n/total_genes, fill = cell)) +
#   geom_histogram(show.legend = FALSE) +
#   xlim(NA, 0.0009) +
#   facet_wrap(~cell, ncol = 2, scales = "free_y")

## tf_idf
e_tbl <- e_tbl %>%
  bind_tf_idf(gene, cell, n)
e_tbl

## arrange by tf_idf
e_tbl %>%
  select(-total_genes) %>%
  arrange(desc(tf_idf))

## for specific cell
e_tbl %>%
  filter(cell == "GACGCTCTCTCTCG-1") %>%
  select(-total_genes) %>%
  arrange(desc(tf_idf))



e_tf_idf <- e_tbl %>%
  select(cell, gene, tf_idf) %>%
  mutate(tf_idf = round(tf_idf, 3)) %>%
  spread(cell, tf_idf, fill = 0)

e_tfidf_cor <- cor(e_tf_idf[,-1])
e_tfidf_cor_round <- round(e_tfidf_cor, 0)

library(igraph)
g <- graph_from_adjacency_matrix(e_tfidf_cor_round, 
                                 mode = "undirected", 
                                 weighted = TRUE,
                                 diag = FALSE)
plot(g, l = layout_randomly)

g_ego <- make_ego_graph(g)


library(visNetwork)
# cliq <- cliques(g)
cliq <- cliques(g, min = 10)

visNetwork(dat$nodes, dat$edges)
