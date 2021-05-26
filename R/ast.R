getScriptPath <- function(){
    cmd.args <- commandArgs()
    m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
    script.dir <- dirname(regmatches(cmd.args, m))
    if(length(script.dir) == 0) stop("can't determine script dir: please call the script with Rscript")
    if(length(script.dir) > 1) stop("can't determine script dir: more than one '--file' argument detected")
    return(script.dir)
}

d <- getScriptPath()

library(rlang)
source(file.path(d, "R/listprocessor.R"))

##' Tokenise the user input
##'
##' From the lisp syntax, tokenise and provide a AST representation using
##' list of lists.
##' @title ast
##' @param input as a lisp-style string
##' @return List of lists for each token
##' @author Jay Morgan
##' @export
ast <- function(input) {

  filter_comments <- function(input) {
    stringr::str_remove_all(input, "(;;.*$)")
  }

  fun_re <- "^\\("
  fun_con <- "(?:(\\(|\\))|([\\[\\]#{}])|(\".*?\")|([*+<>^:?=&$||\\w\\d\\/.-]+))"

  is_fun <- function(input) {
    return(stringr::str_detect(input, fun_re))
  }

  find_first_token <- function(lst, stop_token = ")") {
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

  find_matching_token <- function(lst, start_token = "(", end_token = ")") {
    n   <- 0
    end <- 0
    start <- find_first_token(lst, start_token)
    for (t in start:length(lst)) {
      if (lst[[t]] == end_token && n == 1) {
        end <- t
      } else if (lst[[t]] == start_token) {
        n <- n + 1
      } else if (lst[[t]] == end_token) {
        n <- n - 1
      }
    }
    return(list(start = start, end = end))
  }

  to_data_types <- function(input) {
    input <- stringr::str_replace_all(input, "\\[", "(c ")
    input <- stringr::str_replace_all(input, "\\{", "(list ")
    input <- stringr::str_replace_all(input, "[\\]\\}]", ")")
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

  find_ends <- function(lst, start_token = "(", end_token = ")") {
    n <- 0
    end <- -1
    for (i in seq_len(length(lst))) {
      token <- lst[[i]]
      if (token == end_token && n == 0) {
        return(i)
      } else if (token == start_token && i != 1) {
        n <- n + 1
      } else if (token == end_token) {
        n <- n - 1
      }
    }
    if (end == -1) {
      stop("Unbalanced paranthesis")
    }
    return(end)
  }

  find_start_ends <- function(lst, find_token = "(", end_token = ")") {
    starts <- c()
    ends <- c()
    for (index in seq_len(length(lst))) {
      token <- lst[[index]]
      if (token == find_token) {
        starts <- c(starts, index)
        end <- find_ends(lst[index:length(lst)], start_token = find_token, end_token = end_token)
        end <- end + (index-1)
        ends <- c(ends, end)
      }
    }
    return(list(starts = starts, ends = ends))
  }

  tokenize_function <- function(tokens, start_token = "(", end_token = ")") {
    contents <- list()
    counter <- 0
    i <- 2

    while (i < length(tokens)) {
      token <- tokens[[i]]

      if (token == end_token) { break }
      if (token == start_token) {
        out <- tokenize_function(tokens[i:length(tokens)])
        i <- out$end + (i - 1)  # skip past the already processed sublist
        token <- out$contents
      }

      ## if (typeof(token) != "list" && token == "[") {
      ##   out <- tokenize_function(tokens[i:length(tokens)], start_token = "[", end_token = "]")
      ##   i <- out$end + (i - 1)
      ##   token <- out$contents
      ##   token <- paste0("c(", paste(token, collapse=", "), ")")
      ## }

      contents[[counter <- counter + 1]] <- token
      i <- i + 1
    }

    return(list(contents = contents, end = i))
  }

  tokenize <- function(input) {
    input <- filter_comments(input)
    input <- to_data_types(input)
    if (is_fun(input)) {
      tokens <- stringr::str_match_all(input, fun_con)[[1]][,1]
      return(tokenize_function(tokens)$contents)
    } else {
      # try to evaluate directly
      return(input)
    }
  }
  return(tokenize(input))
}


display_ast <- function(ast_list, depth = 0) {
  for (item in ast_list) {
    if (typeof(item) == "list") {
      display.ast(item, depth = depth + 1)
    } else {
      indentation <- paste(replicate("--", n = depth), collapse = "")
      print(paste(indentation, item, collapse = ""))
    }
  }
}
