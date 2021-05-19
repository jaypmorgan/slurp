library(rlang)
source("listprocessor.R")

evaluate_ast <- function(ast_list) {

  is_infix <- function(fun) {
    infix_ops <- c("+", "-", "*", "/", "%")
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
    out <- do.call(func, args, envir = env_parents()[[2]])
    return(out)
  }

  compile_function <- function(func, args) {
    if (is_infix(func)) {
      out <- paste(args[[1]], func, args[[2]])
    } else if (func == "defparam") {
      out <- paste(args[[1]], "<-", args[[2]])
    } else {
      out <- paste0(func, "(", paste(args, collapse=","), ")")
    }
    return(out)
  }

  run_evaluation <- function(ast_list) {
    func <- first(ast_list)
    args <- rest(ast_list)
    evaluated_args <- list()
    counter <- 0
    for (item in args) {
      if (typeof(item) == "list") {
        evaluated_args[[counter <- counter + 1]] <- run_evaluation(item)
      } else {
        evaluated_args[[counter <- counter + 1]] <- item # evaluate_directly(item)
      }
    }

    output <- compile_function(func, evaluated_args)
    return(output)
  }

  if (length(ast_list) > 1) {
    output <- eval(parse(text = run_evaluation(ast_list)), envir = env_parents()[[1]])
  } else {
    output <- eval(parse(text = ast_list[[1]]), envir = env_parents()[[1]])
  }
  return(output)
}
