params <- list()
params$path <- "data/outs/filtered_gene_bc_matrices/hg19"
params$cells <- 3
params$genes <- 200
params$project <- "Title"
params$max_mt <- 0.05

rmarkdown::render('my_markdown_report.Rmd',
                  output_file = paste('report.', Sys.Date(), 
                                      '.pdf', sep=''))

library(googleVis) ## Version >= 0.3.2 required
library(knitr)
library(markdown)
library(RCurl)
## URL to the Markdown example file on github:gist
gist <- "https://raw.github.com/gist/3968910/4633a98fdc5193eb7da156059d182e61ccbfd4a8/MarkdownExampleWithGoogleVis.Rmd"
knitrRmd <- paste(readLines(textConnection(getURL(gist))), collapse="\n")

## Write the content of knitrRmd into a Rmd-file, knit it and convert it
## into a html output. Finally show the file with the R-help http
## server, this will ensure that also the motion chart is visible.
wd <- getwd()
setwd(tempdir())
fn=tempfile()
fn.Rmd <- paste(fn, ".Rmd", sep="")
fn.md <- paste(fn, ".md", sep="")
fn.html <- paste(fn, "-out.html", sep="") 
## Write R Markdown into a file
cat(knitrRmd, file=fn.Rmd)
render_markdown()
knit(fn.Rmd, fn.md)
knit2html(fn.md)
## Open HTML file in browser
plot.gvis(fn.html)