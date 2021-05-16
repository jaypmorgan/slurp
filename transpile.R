source("ast.R")
options(warn = -1)

args <- commandArgs(trailingOnly = TRUE)

source_code <- readLines(args[1])

ast(source_code)
