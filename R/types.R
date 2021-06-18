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
  getdim = function(idx) {
    calls <- c("[")
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
    eval(parse(text = paste0("self$data[", indexes, "]")))
  })
)
