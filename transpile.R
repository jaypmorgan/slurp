source("ast.R")
options(warn = -1)

args <- commandArgs(trailingOnly = TRUE)

source_code <- paste(readLines(args[[1]]), sep = "\n", collapse = "")
source_code <- stringi::stri_replace_all(source_code, regex = "\\(.*?(\\r\\n|\\r|\\n)", " ")
print(source_code)

ast(source_code)
