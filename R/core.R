getScriptPath <- function(){
    cmd.args <- commandArgs()
    m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
    script.dir <- dirname(regmatches(cmd.args, m))
    if(length(script.dir) == 0) stop("can't determine script dir: please call the script with Rscript")
    if(length(script.dir) > 1) stop("can't determine script dir: more than one '--file' argument detected")
    return(script.dir)
}

d <- getScriptPath()
source(file.path(d, "R/functions.R"))
source(file.path(d, "R/listprocessor.R"))


keywords_to_parameter <- function(args) {
  stringr::str_replace_all(args, ":([\\w\\d_]+),", "\\1=")
}

strip_comments <- function(input) {
  stringr::str_remove_all(input, ";;.*")
}

progn <- function(args) {
  paste(args, collapse = "\n")
}

defparam <- function(args) {
  assignments <- c()
  for (i in seq(1, length(args), 2)) {
    assignments <- c(assignments, paste(args[[i]], "<-", args[[i+1]]))
  }
  out <- paste0(assignments, collapse = "\n")
  return(out)
}

lambda <- function(args) {
  paste0("function(",
         keywords_to_parameter(paste(args[[1]], collapse = ", ")), ") {\n",
         paste(args[2:length(args)], collapse = "\n"),
         "\n}")
}

defun <- function(args) {
  has_docstring <- stringr::str_detect(args[[3]], "^\".*\"")
  if (has_docstring) {
    docstring <- stringr::str_remove_all(args[[3]], "\"")
    body <- paste("#'", docstring, "\n", args[4:length(args)])
  } else {
    body <- args[3:length(args)]
  }
  out <- paste(args[[1]], "<- function(", paste(args[[2]], collapse = ", "), ") {\n")
  body <- stringr::str_replace_all(body, "`", "expr")
  out <- paste(out, paste0(body, collapse = "\n"), "\n}")
  out <- keywords_to_parameter(out)
  return(out)
}

if_c <- function(args) {
  expr <- args[[1]]
  body1 <- args[[2]]
  body2 <- ""
  if (length(args) > 2) {
    body2 <- args[[3]]
  }
  out <- paste0("if (", expr, ") {\n  ", body1, "\n}")
  if (body2 != "") {
    out <- paste0(out, " else {\n  ", body2, "\n}")
  }
  return(out)
}

when_c <- function(args) {
  expr <- first(args)
  body <- rest(args)
  paste0("if (", expr, ") {\n  ", paste0(body, collapse = "\n  "), "\n}")
}

unless_c <- function(args) {
  expr <- first(args)
  body <- rest(args)
  paste0("if (!", expr, ") {\n  ", paste0(body, collapse = "\n  "), "\n}")
}

cond_c <- function(args) {
  ## already compiled just return
  return(args)
}
