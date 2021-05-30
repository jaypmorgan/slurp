library(base)
library(rlang)

getScriptPath <- function(){
    cmd.args <- commandArgs()
    m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
    script.dir <- dirname(regmatches(cmd.args, m))
    if(length(script.dir) == 0) stop("can't determine script dir: please call the script with Rscript")
    if(length(script.dir) > 1) stop("can't determine script dir: more than one '--file' argument detected")
    return(script.dir)
}

d <- getScriptPath()

source(file.path(d, "R/listprocessor.R"))
source(file.path(d, "R/functions.R"))

##' Evaluate the AST form
##'
##' Using the result of ast(...), evaluate and compile
##' the expresions
##' @title evaluate_ast
##' @param ast_list the results of ast(...)
##' @return the compiled R expression
##' @author Jay Morgan
##' @export
evaluate_ast <- function(ast_list) {

  defun <- function(name, args, body) {
    out <- paste(name, "<- function(", paste(args, collapse = ", "), ") {\n")
    body <- stringr::str_replace_all(body, "`", "expr")
    out <- paste(out, body, "\n}")
    return(out)
  }

  if_control <- function(expr, body1, body2) {
    out <- paste0("if (", expr, ") {\n  ", body1, "\n}")
    if (body2 != "") {
      out <- paste0(out, " else {\n  ", body2, "\n}")
    }
    return(out)
  }

  is_infix <- function(fun) {
    infix_ops <- c("+", "-", "*", "/", "%%", "^", ">", "<", ">=", "<=", "==")
    if (fun %in% infix_ops) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  evaluate_directly <- function(statement) {
    out <- tryCatch({
      # interpret the statement from the string representation
      out <- eval(parse(text = statement))
      return(out)
    },
    error=function(cond) {
      # take the statement literally
      out <- statement
      return(out)
    })
    return(out)
  }

  evaluate_function <- function(func, args) {
    out <- do.call(func, args, envir = rlang::env_parents()[[2]])
    return(out)
  }

  standardise_name <- function(func) {
    func <- stringr::str_replace_all(func, "->", "_to_")
    func <- stringr::str_replace_all(func, "-", "_")
    func <- stringr::str_replace_all(func, "\\?", "_p")
  }

  keywords_to_parameter <- function(args) {
    clean_args <- list()
    for (i in 1:length(args)) {
      clean_args[[i]] <- stringr::str_replace_all(args[[i]], ":([\\w\\d_]+),", "\\1=")
    }
    return(clean_args)
  }

  compile_function <- function(func, passed_args) {
    func <- standardise_name(func)
    args <- list()
    for (i in seq_len(length(passed_args))) {
      args[[i]] <- standardise_name(passed_args[[i]])
    }

    if (is_infix(func)) {
      out <- paste(args, collapse = func)
    } else if (func == "defparam") {
      out <- paste(args[[1]], "<-", args[[2]])
    } else if (func == "lambda") {
      out <- paste0("function(", paste(args[[1]], collapse = ", "), ") {\n",
                    paste(args[[2:length(args)]], collapse = "\n"),
                    "\n}")
    } else if (func == "defun") {
      has_docstring <- stringr::str_detect(args[[3]], "^\".*\"")
      if (has_docstring) {
        docstring <- stringr::str_remove_all(args[[3]], "\"")
        body <- paste("#'", docstring, "\n", args[[4]])
      } else {
        body <- args[[3]]
      }
      out <- defun(args[[1]], args[[2]], body)
    } else if (func == "if") {
      expr <- args[[1]]
      body1 <- args[[2]]
      body2 <- ""
      if (length(args) > 2) {
        body2 <- args[[3]]
      }
      out <- if_control(expr, body1, body2)
      print(out)
    } else {
      out <- paste0(func, "(", keywords_to_parameter(paste(args, collapse=",")), ")")
    }
    return(out)
  }

  run_evaluation <- function(ast_list) {
    func <- first(ast_list)
    args <- rest(ast_list)
    evaluated_args <- list()
    counter <- 0
    n_func <- FALSE

    if (func %in% c("defun")) {
      evaluated_args[[counter <- counter + 1]] <- args[[1]]
      evaluated_args[[counter <- counter + 1]] <- args[[2]]
      args <- args[c(-1, -2)]
    }

    if (func %in% c("lambda")) {
      evaluated_args[[counter <- counter + 1]] <- args[[1]]
      args <- args[c(-1)]
    }

    for (item in args) {
      # TODO: there should be a better way of saying this list is referencing a function call
      if (is_list(item) && (item[[1]] != "TRUE" && item[[1]] != "FALSE")) {
        evaluated_args[[counter <- counter + 1]] <- run_evaluation(item)
      } else {
        evaluated_args[[counter <- counter + 1]] <- item # evaluate_directly(item)
      }
    }

    output <- compile_function(func, evaluated_args)
    return(output)
  }

  var <- standardise_name(ast_list[[1]])

  if (length(ast_list) > 1 || (ast_list[[1]] != "" && !(var %in% ls(rlang::env_parents()[[1]])))) {
    func <- run_evaluation(ast_list)
    output <- eval(parse(text = func), envir = rlang::env_parents()[[1]])
  } else {
    output <- eval(parse(text = var), envir = rlang::env_parents()[[1]])
  }
  return(output)
}
