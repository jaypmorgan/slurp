defun <- function(name, args, body) {
  out <- paste(name, "<- function(", paste(args, collapse = ", "), ") {\n")
  body <- stringr::str_replace_all(body, "`", "expr")
  out <- paste(out, compile_function(body[[1]], body[2:length(body)]), "\n}")
  return(out)
}

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

not <- function(arg) {
  return(!(arg))
}
