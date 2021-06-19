library(R6)

NamedMatrix <- R6Class("NamedMatrix", list(
  data = NULL,
  dims = NULL,
  names = NULL,
  initialize = function(dims, data) {
    self$data <- array(data, dims)
    self$dims <- length(dim(self$data))
    self$names <- names(dims)
  },
  print = function() { print(self$data) },
  getdim = function(idx, do_eval = TRUE) {
    calls <- c("[")
    if (typeof(idx) == "list") {
      indexes <- vector(mode = "list", length = self$dims)
      for (i in seq(1, length(idx))) {
        k <- names(idx)[[i]]
        v <- idx[[i]]
        indexes[[which(self$names == k)]] <- paste0("c(", paste(v, collapse=", "), ")")
      }
      for (i in seq(1, self$dims)) {
        if (is.null(indexes[[i]])) {
          indexes[[i]] <- ""
        }
      }
      indexes <- paste(indexes, collapse = ", ")
      t <- paste0("self$data[", indexes, "]")
      if (do_eval) {
        return(eval(parse(text = t)))
      } else {
        return(t)
      }
    } else {
      if (do_eval) {
        return(self$data[idx])
      } else {
        return(paste0("self$data[", idx, "]"))
      }
    }
  },
  set = function(idx, val) {
    arr <- self$getdim(idx, do_eval = FALSE)
    ass <- paste0(arr, " <- ", val)
    return(eval(parse(text=ass)))
  },
  get = function(idx) {
    return(self$getdim(idx))
  })
)

make_array <- function(dims, data) NamedMatrix$new(dims, data)
getin.namedmatrix <- function(x, idx) x$get(idx)
setin.namedmatrix <- function(x, idx, val) x$set(idx, val)
