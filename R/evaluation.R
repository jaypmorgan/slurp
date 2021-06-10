library(base)
library(rlang)

##' Evaluate the AST form
##'
##' Using the result of ast(...), evaluate and compile
##' the expresions
##' @title evaluate_ast
##' @param ast_list the results of ast(...)
##' @return the compiled R expression
##' @author Jay Morgan
##' @export
slurp_evaluate_ast <- function(ast_list) {

  is_infix <- function(fun) {
    infix_ops <- c("+", "-", "*", "/", "%%", "^", ">", "<", ">=", "<=", "==", "%>%")
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
    if (length(func) == 0) { return(func) }
    if (func == "<-") { return(func) }
    if (grepl("\"", func)) { return(func) }
    func <- stringr::str_replace_all(func, "->", "_to_")
    func <- stringr::str_replace_all(func, "(?<!<)-", "_")
    func <- stringr::str_replace_all(func, "\\?", "_p")
  }

  builtin_keywords <- c("progn", "defparam", "lambda", "defun", "if", "when", "unless", "cond")
  builtin_mappings <- c(progn, defparam, lambda, defun, if_c, when_c, unless_c, cond_c)

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

    if (func %in% c("cond")) {
      for (idx in 1:length(args)) {
        item <- args[[idx]]
        args[[idx]] <- c("if", item[1], item[2])
      }
    }

    for (item in args) {
      # TODO: there should be a better way of saying this list is referencing a function call
      if (rlang::is_list(item) && (item[[1]] != "TRUE" && item[[1]] != "FALSE")) {
        evaluated_args[[counter <- counter + 1]] <- run_evaluation(item)
      } else {
        evaluated_args[[counter <- counter + 1]] <- item
      }
    }

    output <- compile_function(func, evaluated_args)
    return(output)
  }

  var <- standardise_name(ast_list[[1]])
  slurp_env <- rlang::env_parents()[[1]]

  if (length(ast_list) > 1
      || (ast_list[[1]] != "" && !(var %in% ls(slurp_env)))
      || sum(grepl(paste0("^", var), utils::lsf.str(envir = slurp_env)))) {
    func <- run_evaluation(ast_list)
    output <- eval(parse(text = func), envir = slurp_env)
  } else {
    output <- eval(parse(text = var), envir = slurp_env)
  }
  return(output)
}
