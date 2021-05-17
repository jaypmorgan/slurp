ast <- function(ui) {
  is_infix <- function(fun) {
    infix_ops <- c("+", "-", "*", "/", "%")
    if (fun %in% infix_ops) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  tokenize <- function(ui) {
    str_regex <- "(?:(\\(|\\))|([\\[\\]#{}])|([\"*+\\w\\d\\/-]+))"
    tokens <- stringr::str_match_all(ui, str_regex)[[1]][,1]
    return(tokens)
  }

  arithmetic_fun <- function(op, arg1, arg2) {
    return(paste0(arg1, op, arg2))
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
    args <- args[2:length(args)][[1]]
    fun <- paste0(func, "(", paste0(args, collapse = ", "), ")")
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

    if (is_infix(func)) {
      fun <- arithmetic_fun(func, args[[1]], args[[2]])
    } else {
      fun <- general_fun(func, args)
    }
    return(fun)
  }

  eval_function <- function(fun) {
    tryCatch({
        out <- eval(parse(text=fun))
    },
    error=function(cond) {
      message(paste0(cond, "\n"))
      return(NA)
    })
  }

  tokens <- tokenize(ui)
  end_point <- -1
  start_point <- -1
  output <- ""

  for (i in length(tokens):1) {
    t <- tokens[[i]]
    if (t == ")") {
      end_point <- i
    }
    if (t == "(") {
      start_point <- i
    }

    if (start_point != -1 && end_point != -1) {
      fun <-compile_function(tokens[(start_point+1):(end_point-1)])
      output <- eval_function(fun)
      start_point <- -1
      end_point <- -1
    }
  }
  return(output)
}
