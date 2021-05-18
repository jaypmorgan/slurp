first <- function(lst) {
  return(lst[[1]])
}

rest <- function(lst) {
  return(lst[2:length(lst)])
}

nth <- function(lst, n) {
  return(lst[[n]])
}

preallocate <- function(n = 10000) {
  return(vector("list", n))
}

## car and cdr function derivatives
car    <- function(lst) { first(lst) }
cdr    <- function(lst) { rest(lst) }
cadr   <- function(lst) { first(rest(lst)) }
caddr  <- function(lst) { first(rest(rest(lst))) }
cadddr <- function(lst) { first(rest(rest(rest(lst)))) }
caadr  <- function(lst) { first(first(rest(lst))) }
caaadr <- function(lst) { first(first(first(rest(lst)))) }
