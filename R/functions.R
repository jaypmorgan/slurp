progn <- function(args) {
  out <- paste(args, collapse = "\n")
  return(out)
}

falsey <- function(arg) {
  if (is.null(arg)
      || is.na(arg)
      || arg == FALSE
      || arg == 0L
      || arg == 0.0
      || (typeof(arg) == "list" && length(arg) == 0)
      || arg == "") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

and <- function(...) {
  args <- list(...)
  for (arg in args) {
    if (falsey(arg) || eval(arg) == FALSE) {
      return(FALSE)
    }
  }
  return(args[[length(args)]])
}

or <- function(...) {
  args <- list(...)
  for (arg in args) {
    if (falsey(arg) || eval(arg) == FALSE) {
      return(arg)
    }
  }
  return(args[[length(args)]])
}

##' invert a bool value, i.e. TRUE => FALSE, FALSE => TRUE
##'
##' Instead of the traditional inversion of boolean values using
##' !(bool), here we provide a more lispy style inversion function called 'not'.
##' This can be applied to boolean functions as a simple function call.
##' @title not
##' @param bool The bool to invert `TRUE/FALSE`
##' @return
##' @author Jay Morgan
##' @examples
##' (not TRUE)
##' (not FALSE)
##' (not (== 1 1))
##' @export
not <- function(bool) {
  return(!(bool))
}

thread_first <- function(...) {
  args <- list(...)
  x <- args[[1]]
  for (i in 2:length(args)) {
    lst <- args[[i]]
    lst[[length(lst)+1]] <- x
  }
  return(x)
}
