source("listprocessor.R")

evaluate_ast <- function(ast_list) {

  defun <- function(name, args, body) {
    out <- paste(name, "<- function(", paste(args, collapse = ", "), ") {\n")
    body <- stringr::str_replace_all(body, "`", "expr")
    out <- paste(out, compile_function(body[[1]], body[2:length(body)]), "\n}")
    return(out)
  }

  is_infix <- function(fun) {
    infix_ops <- c("+", "-", "*", "/", "%", "^")
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
      clean_args[[i]] <- stringr::str_replace_all(args[[i]], ":([\\w\\d_]+)", "\\1=")
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
      statements <- c()
      for (i in 2:length(args)) {
        statements <- c(statements, paste0("  ", run_evaluation(args[[i]])))
      }
      out <- paste0("function(", paste(args[[1]], collapse = ", "), ") {\n",
                    paste(statements, collapse = "\n"),
                    "\n}")
    } else if (func == "defun") {
      out <- defun(args[[1]], args[[2]], args[[3]])
    } else {
      out <- paste0(func, "(", paste(keywords_to_parameter(args), collapse=","), ")")
    }
    return(out)
  }

  run_evaluation <- function(ast_list) {
    func <- first(ast_list)
    args <- rest(ast_list)
    evaluated_args <- list()
    counter <- 0
    for (item in args) {
      if (typeof(item) == "list" && !(func %in% c("lambda", "defun"))) {
        evaluated_args[[counter <- counter + 1]] <- run_evaluation(item)
      } else {
        evaluated_args[[counter <- counter + 1]] <- item # evaluate_directly(item)
      }
    }

    output <- compile_function(func, evaluated_args)
    return(output)
  }

  var <- standardise_name(ast_list[[1]])

  if (length(ast_list) > 1 || !(var %in% ls(rlang::env_parents()[[1]]))) {
    func <- run_evaluation(ast_list)
    output <- eval(parse(text = func), envir = rlang::env_parents()[[1]])
  } else {
    output <- eval(parse(text = var), envir = rlang::env_parents()[[1]])
  }
  return(output)
}
