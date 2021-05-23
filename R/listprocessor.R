first <- function(lst) {
  return(lst[[1]])
}

rest <- function(lst) {
  return(lst[c(-1)])
}

nth <- function(lst, n) {
  return(lst[[n]])
}

find_last <- function(lst) {
  i <- 1
  for (index in 1:length(lst)) {
    if (!is.null(lst[[index]])) {
      i <- i + 1
    }
  }
  return(i)
}

last <- function(lst) {
  return(lst[[find_last(lst)]])
}

preallocate <- function(n = 10000) {
  return(vector("list", n))
}

add_element <- function(lst, element) {
  index <- find_last(lst)
  lst[[index]] <- element
  return(lst)
}

display_elements <- function(lst) {
  v <- c("(")
  for (i in 1:length(lst)) {
    if (is.null(lst[[i]])) {
      break
    }
    item <- lst[[i]]
    if (typeof(item) == "list") {
      v <- c(v, display_elements(item))
    } else {
      v <- c(v, item)
    }
    v <- c(v, " ")
  }
  v <- v[1:(length(v)-1)]
  v <- c(v, ")")
  return(paste0(v, collapse=""))
}

## car and cdr function derivatives
car    <- function(lst) { first(lst) }
cdr    <- function(lst) { rest(lst) }
cadr   <- function(lst) { first(rest(lst)) }
caddr  <- function(lst) { first(rest(rest(lst))) }
cadddr <- function(lst) { first(rest(rest(rest(lst)))) }
caadr  <- function(lst) { first(first(rest(lst))) }
caaadr <- function(lst) { first(first(first(rest(lst)))) }
