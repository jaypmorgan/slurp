library(rlang)
source("listprocessor.R")

ast <- function(input) {
  fun_re <- "^\\("
  fun_con <- "(?:(\\(|\\))|([\\[\\]#{}])|(\".*?\")|([*+<>\\w\\d\\/.-]+))"

  is_fun <- function(input) {
    return(stringr::str_detect(input, fun_re))
  }

  find_first_stop <- function(lst, stop_token = ")") {
    t <- 1
    for (item in lst) {
      if (item == stop_token) {
        break
      } else {
        t <- t + 1
      }
    }
    return(t)
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

  tokenize_function <- function(input, depth=0) {
    boundaries <- c("[", "]", "(", ")", "{", "}")
    contents <- list()
    counter <- 0
    if (typeof(input) == "list") {
      tokens <- input[[1]]
    } else {
      tokens <- stringr::str_match_all(input, fun_con)[[1]][,1]
    }
    skip_tokens <- FALSE
    end_token <- 0
    for (i in 1:length(tokens)) {

      if (skip_tokens) {
        if (i < end_token) {
          # move onto the next token
          next
        } else {
          # done skipping
          end_token <- 0
          skip_tokens <- FALSE
        }
      }

      token <- tokens[[i]]
      if (token == "[") {
        end_token <- find_first_stop(tokens, "]")
        contents[[counter <- counter + 1]] <- to_vector(list(tokens[i:end_token])[[1]])
        skip_tokens <- TRUE
      } else if (!(token %in% boundaries)) {
        contents[[counter <- counter + 1]] <- token
      } else if (token == "(" && length(contents) >= 1) {
        end_token <- find_first_stop(tokens)
        contents[[counter <- counter + 1]] <- tokenize_function(list(tokens[(i+1):end_token]), depth=2)
        skip_tokens <- TRUE
      }
    }
    return(contents)
  }

  tokenize <- function(input) {
    if (is_fun(input)) {
      return(tokenize_function(input))
    } else {
      # try to evaluate directly
      return(input)
    }
  }
  return(tokenize(input))
}


display.ast <- function(ast_list, depth = 0) {
  for (item in ast_list) {
    if (typeof(item) == "list") {
      display.ast(item, depth = depth + 1)
    } else {
      indentation <- paste(replicate("--", n = depth), collapse = "")
      print(paste(indentation, item, collapse = ""))
    }
  }
}
