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
    tokens <- stringr::str_replace_all(ui, "\\(", "( ")
    tokens <- stringr::str_replace_all(tokens, "\\)", " )")
    tokens <- strsplit(tokens, " ")[[1]]
    return(tokens)
  }

  arithmetic_fun <- function(op, arg1, arg2) {
    return(paste0(arg1, op, arg2))
  }

  general_fun <- function(...) {
    args <- list(...)
    func <- args[[1]]
    args <- args[2:length(args)][[1]]
    ## for (i in 1:length(args)) {
    ##   args[[i]] <- str2lang(args[[i]])
    ## }
    fun <- paste0(func, "(", paste0(args, collapse = ", "), ")")
    return(fun)
  }

  compile_function <- function(inputs) {
    func <- inputs[[1]]
    args <- inputs[2:length(inputs)]
    if (is_infix(func)) {
      fun <- arithmetic_fun(func, args[[1]], args[[2]])
    } else {
      fun <- general_fun(func, args)
    }
    return(fun)
  }

  eval_function <- function(fun) {
    out <- eval(parse(text=fun))
  }

  tokens <- tokenize(ui)
  end_point <- -1
  start_point <- -1

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
