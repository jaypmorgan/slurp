library(rlang)
source("listprocessor.R")

ast <- function(ui) {

  prepare_args <- function(...) {
    args <- unlist(list(...))
    return(args)
  }

  function_map <- list()
  function_map[["+"]] <- function(a, b) { paste("(", a, "+", b, ")") }
  function_map[["-"]] <- function(a, b) { paste("(", a, "-", b, ")") }
  function_map[["*"]] <- function(a, b) { paste("(", a, "*", b, ")") }
  function_map[["/"]] <- function(a, b) { paste("(", a, "/", b, ")") }
  function_map[["^"]] <- function(a, b) { paste("(", a, "^", b, ")") }
  function_map[["defparam"]] <- function(...) {
    a <- prepare_args(...);
    e <- env_parents()[[2]]
    assign(a[[1]], str2lang(a[[2]]), envir = e)
    return(a[[2]])
  }


  is_infix <- function(fun) {
    infix_ops <- c("+", "-", "*", "/", "%")
    if (fun %in% infix_ops) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  tokenize <- function(ui) {
    str_regex <- "(?:(\\(|\\))|([\\[\\]#{}])|(\".*?\")|([*+\\w\\d\\/.-]+))"
    tokens <- stringr::str_match_all(ui, str_regex)[[1]][,1]
    return(tokens)
  }

  to_vector <- function(tokens) {
    v <- c("c(")
    start <- FALSE
    for (token in tokens) {
      if (token == "[") { start <- TRUE; next }
      if (token != "]" && start == TRUE) {
        v <- c(v, token, ",")
      }
      if (token == "]" && start == TRUE) {
        break
      }
    }
    v <- v[1:(length(v)-1)]
    v <- c(v, ")")
    v <- paste0(v, collapse="")
    return(v)
  }

  general_fun <- function(...) {
    args <- list(...)
    func <- args[[1]]
    if (!is.na(args[[2]])[[1]]) {
      args <- args[2:length(args)][[1]]
      fun <- paste0(func, "(", paste0(args, collapse = ", "), ")")
    } else {
      fun <- paste0(func, "()")
    }
    return(fun)
  }

  compile_function <- function(inputs) {
    func <- inputs[[1]]
    args <- inputs[2:length(inputs)]

    if ("[" %in% args) {
      # find the start and end point of the vector
      start_idx <- which("[" == args)
      end_idx <- which("]" == args)
      v <- to_vector(args[start_idx:end_idx])
      new_args <- c()

      # resize arguments concatenating middle vector
      if (start_idx != 1) {
        new_args <- c(new_args, args[1:(start_idx-1)])
      }
      new_args <- c(new_args, v)
      if (end_idx != length(args)) {
        new_args <- c(new_args, args[1:(end_idx)])
      }
      args <- new_args
    }

    if (func %in% names(function_map)) {
      if (!is_infix(func)) {
        fun <- function_map[[func]](args)
      } else {
        fun <- function_map[[func]](args[[1]], args[[2]])
      }
    } else {
      fun <- general_fun(func, args)
    }
    return(fun)
  }

  eval_function <- function(fun) {
    print(fun)
    tryCatch({
        out <- eval(parse(text=fun))
        return(out)
    },
    error=function(cond) {
      cat(paste(cond, "\n"))
      return(NULL)
    })
  }

  eval_variable <- function(v) {
    tryCatch({
        out <- eval(str2lang(v))
        return(out)
    },
    error=function(cond) {
      cat(paste(cond, "\n"))
      return(NULL)
    })
  }

  tokens <- tokenize(ui)
  root <- NULL


  add_tokens <- function(tokens) {
    out <- preallocate(n = 10)
    i <- 1
    while (i <= length(tokens)) {
      token <- tokens[[i]]

      if (token == "(" && !is.null(first(out))) {
        end_token <- 0
        for (e in 1:length(tokens)) {
          t <- tokens[[e]]
          if (t == ")") {
            end_token <- e
            break
          }
        }
        out <- add_element(out, add_tokens(tokens[i:end_token]))
        i <- end_token
      } else if (token != ")" && token != "(") {
        out <- add_element(out, token)
      }
      i <- i + 1
    }
    return(out)
  }

  run_ast <- function(lst) {
    fun <- lst[[1]]
    args <- c()

    for (el in 2:find_last(lst)) {
      item <- lst[[el]]
      if (typeof(item) == "list") {
        item <- run_ast(item)
      }
      args <- c(args, item)
    }
    o <- compile_function(c(fun, args))
    ## o <- eval_function(o)
    return(o)
  }

  if (tokens[[1]] == "(") {
    lst <- add_tokens(tokens)
    output <- run_ast(lst)
    output <- eval_function(output)
  } else {
    ## is a variable
    output <- eval_variable(tokens)
  }
  return(output)
}
