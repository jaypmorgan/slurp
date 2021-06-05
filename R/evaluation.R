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

  builtin_keywords <- c("progn", "defparam", "lambda", "defun", "if", "while", "unless")
  builtin_mappings <- c(progn, defparam, lambda, defun, if_c, while_c, unless)

  compile_function <- function(func, passed_args) {
    func <- standardise_name(func)
    args <- list()
    for (i in seq_len(length(passed_args))) {
      args[[i]] <- standardise_name(passed_args[[i]])
    }

    if (is_infix(func)) {
      out <- paste(args, collapse = func)
    } else if (func %in% builtin_keywords) {
      fn_idx <- which(func == builtin_keywords)
      out <- builtin_mappings[[fn_idx]](args)
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
