##' Retrieve the first element from list
##'
##' Retrieve and return the first element from a list. This lispy
##' style function uses indexing under the hood but exposes the
##' process as a function call.
##' @title first
##' @param lst the list to which the first element will be returned
##' @return the first element
##' @examples
##' (first {1 2 3}) => 1
##' (first {{1 2} {3 4}}) => list(1, 2)
##' @author Jay Morgan
##' @export
first <- function(lst) {
  return(lst[[1]])
}

##' Retrieve the rest of a list
##'
##' Retrieve the rest of the elements from a list type data structure.
##' @title rest
##' @param lst the list to get everything but the first index
##' @return the list excluding the first index
##' @examples
##' (rest {1 2 3 4}) => list(2, 3, 4)
##' (rest {{1 2} {3 4}}) => list(3, 4)
##' @author Jay Morgan
##' @export
rest <- function(lst) {
  return(lst[c(-1)])
}

##' Return the nth element from a list
##'
##' Retrieve the nth element of the list via a function call instead of indexing
##' @title nth
##' @param lst The list to which the nth element will be returned
##' @param n the index of the element to retrieve
##' @return the nth element
##' @examples
##' (nth {1 2 3} 1) => 1
##' (nth {{1 2} {3 4}} 2) => list(3, 4)
##' @author Jay Morgan
nth <- function(lst, n) {
  return(lst[[n]])
}

##' Cut or slice a list data type from start index to end index
##'
##' Retrieve elements from start index to end index. If start is not supplied, it will be assumed to be the 1st index. If end is not supplied then, all elements up to the end will be returned.
##' @title cut
##' @param lst the list from which the elements are returned
##' @param start the start index (default: 1st index)
##' @param end the end index (default: length of list)
##' @return all elements within the specified range
##' @examples
##' (cut {1 2 3 4} 1 2) => list(1, 2)
##' (cut {1 2 3 4} :start 2) => list(2, 3, 4)
##' (cut {1 2 3 4} :end 3) => list(1, 2, 3)
##' @author Jay Morgan
cut <- function(lst, start = NULL, end = NULL) {
  if (is.null(start)) {
    start <- 1
  }
  if (is.null(end)) {
    end <- length(lst)
  }
  return(lst[start:end])
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
