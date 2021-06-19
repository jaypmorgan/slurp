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
  if (typeof(args) == "list") {
    return(paste0(args, collapse = "\n"))
  }
  ## already compiled just return
  return(args)
}

##' Import a SluRp script file
##'
##' Import a SluRp script file
##' @title source_slurp
##' @param source_code String representation of the source
##' @return NULL
##' @author Jay Morgan
##' @export
source_slurp <- function(source_code) {
  source_code <- readLines(source_code)
  source_code <- paste(source_code, collapse = "\n")
  source_code <- strip_comments(source_code)
  source_code <- stringi::stri_replace_all(source_code, regex = "\\s\\s\\s", " ")
  source_code <- stringr::str_match_all(source_code, "(\\(.*\\))")[[1]][,1]
  for (statement in source_code) {
    slurp_evaluate_ast(slurp_ast(statement))
  }
}
##' Interpret a string as SluRp
##'
##' @title SluRp
##' @param source_code
##' @return
##' @author Jay Morgan
##' @export
slurp <- function(source_code, envir = rlang::caller_env()) {
  source_code <- paste(source_code, collapse = "\n")
  source_code <- strip_comments(source_code)
  source_code <- stringi::stri_replace_all(source_code, regex = "\\s\\s\\s", " ")
  source_code <- stringr::str_match_all(source_code, "(\\(.*\\))")[[1]][,1]
  for (statement in source_code) {
    print(slurp_evaluate_ast(slurp_ast(statement), envir = envir))
  }
}

getin <- function(args) {
  arr <- args[[1]]

  if (is(eval(parse(text=arr)), "NamedMatrix")) {
    return(paste0("getin.namedmatrix(", arr, ", ", args[[2]], ")"))
  }

  out <- c(arr, "[")
  for (i in 2:length(args)) {
    idx <- args[[i]]
    if (idx == ":") idx <- ","
    out <- c(out, idx)
  }
  out <- c(out, "]")
  return(paste0(out, collapse = ""))
}

setin <- function(args) {
  arr <- args[[1]]

  if (is(eval(parse(text=arr)), "NamedMatrix")) {
    return(paste0("setin.namedmatrix(", arr, ", ", args[[2]], ", ", args[[3]], ")"))
  }

  out <- c(arr, "[")
  for (i in 2:length(args)) {
    idx <- args[[i]]
    if (idx == ":") idx <- ","
    out <- c(out, idx)
  }
  out <- c(out, "] <- ", args[[3]])
  return(paste0(out, collapse = ""))
}
