#!/usr/bin/env Rscript
require(docopt)
'Usage:
utils.R [-n <project name>]

Options:
-n Project Name [default: name]

]' -> doc

opts <- docopt(doc)
setwd(file.path("~", "Documents", opts$n))

require(utils)
require(knitr)
require(rmarkdown)

create_these <- c("src", "references", "docs", "results", 
                  "src/images", "src/html", "src/chunks")
for( d in create_these ) dir.create(d, showWarnings = FALSE)

try({
## statistics image backgroud found on this blog
download.file(url = "https://blog.sqlauthority.com/i/a/directory_main_statistics.jpg", 
              destfile = "src/images/statistics.jpg")
})

## writing a simple html button file that references my github page
html_lines <- c("<!DOCTYPE html>",
                  "<html>",
                  '<a href="https://github.com/kdgosik"><button>My Github</button></a>',
                  "</html>")
writeLines(html_lines, con = "src/html/github_button.html")

## custom css included in the Notebook file to set backgroud of downloaded image above
customcss_lines <- c("body{",
                     " background-image: url('images/statistics.jpg');",
                     "min-height: 500px;",
                     " /* Set background image to fixed (don't scroll along with the page) */",
                     " background-attachment: fixed;",
                     "background-position: right top;",
                     "/* Set the background image to no repeat */",
                     "background-repeat: no-repeat;",
                     "/* Scale the background image to be as large as possible */",
                     "background-size: cover;",
                     "}")
writeLines(customcss_lines, "src/custom.css")

  # writes the site yaml file
site_lines <- c('name: "Reproducible Workflows"',
                'output_dir: "../docs"',
                'navbar:',
                '  title: "Reproducible Workflows"',
                '  left:',
                '    - text: "Home"',
                '      href: index.html',
                '    - text: "Notebook"',
                '      href: Notebook.html')
writeLines(site_lines, "src/_site.yml")

## Function from stack overflow to run the purl function on each indivdual chunk of an 
## Rmarkdown file and return as separate .R files.
## (https://stackoverflow.com/questions/35855837/with-knitr-preserve-chunk-options-when-purling-chunks-into-separate-files)

purl_chunks <- function(input_file, input_path = "src/chunks"){
  purled <- knitr::purl(input_file)    # purl original file; save name to variable
  lines <- readLines(purled)    # read purled file into a character vector of lines
  starts <- grep('^## ----.*-+', lines)    # use grep to find header row indices
  stops <- c(starts[-1] - 1L, length(lines))   # end row indices
  # extract chunk names from headers
  names <- sub('^## ----([^-]([^,=]*[^,=-])*)[,-][^=].*', '\\1', lines[starts])
  names <- ifelse(names == lines[starts], '', paste0(names)) # clean if no chunk name
  # make nice file names with chunk number and name (if exists)
  file_names <- file.path(input_path,
                          paste0(names, '.R'))
  for(chunk in seq_along(starts)){    # loop over header rows
    # save the lines in the chunk to a file
    writeLines(append("#!/usr/bin/env Rscript", 
                      lines[starts[chunk]:stops[chunk]]), 
               con = file_names[chunk])
  }
  unlink(purled)    # delete purled file of entire document
}


  ## check if an index file exists for the website, if not create one
if( !{file.exists("src/index.Rmd")} ) {
  index_lines <- c('---', 
                   'title: "Index"', 
                   'output:', 
                   '  html_document:', 
                   '    toc: true',
                   '    toc_float: true', 
                   '---')
  index_lines <- paste0("#'", index_lines)
  writeLines(index_lines, con = "src/index.R")
  spin("src/index.R", knit = FALSE, format = "Rmd")
}


  ## check if a Notebook exists for website, if not creat one
if( !{file.exists("src/Notebook.Rmd")} ) {
  template_lines <- c('---',
                     'title: "Notebook"',
                     'output:',
                     '  html_document:',
                     '    toc: true',
                     '    toc_float: true',
                     '    css: custom.css',
                     '    includes:',
                     '      in_header: html/github_button.html',
                     '---',
                     '',
                     '## Comments',
                     'I need to add markdown comments here',
                     '')

  template_lines <- paste0("#'", template_lines)
  code_lines <- c('```{r NotebookSetup, include=FALSE}',
                 'knitr::opts_chunk$set(echo = TRUE)')
  template_lines <- c(template_lines, code_lines)
  writeLines(template_lines, con = "src/Notebook.R")
  spin("src/Notebook.R", knit = FALSE, format = "Rmd")
}

  ## output a purl version of the notebook at each documentation level
lapply(0:2, function(i){
  new_file <- paste0("src/Notebook_DocLevel",i,".Rmd")
  file.copy("src/Notebook.Rmd", new_file, overwrite = TRUE)
  purl(new_file, documentation = i)
  r_file <- paste0("Notebook_DocLevel", i, ".R")
  file.rename(r_file, paste0("src/", r_file))
})

  ## copy and rename the documentation 2 level R file to the Final Notebook version
file.copy("src/Notebook_DocLevel2.R", "src/FinalNotebook.R", overwrite = TRUE)

  ## spin Final Notebook version R file into and .Rmd file
spin("src/FinalNotebook.R", knit = FALSE, format = "Rmd")

  ## purl individual chunks of the final notbook into separate R files
purl_chunks("src/FinalNotebook.Rmd")

  ## render all markdown files in the src folder into html files for the website
rmarkdown::render_site(input = "src")

