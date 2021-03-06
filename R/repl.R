slurp_repl <- function() {
  prompt <- "SluRp> "
  history_file <- ".slurp.history"

  lread <- function() {
    is_complete <- function(input) {
      input <- paste0(input, collapse = "")
      n_starts <- stringr::str_count(input, "\\(")
      n_ends <- stringr::str_count(input, "\\)")
      return(n_starts == n_ends)
    }

    cat(prompt)
    lines <- c()
    while (TRUE) {
      line <- readLines("stdin", n = 1)
      line <- strip_comments(line)
      lines <- c(lines, line)
      if (is_complete(lines)) {
        break
      }
    }
    answer <- paste0(lines, collapse = " ")
    return(answer)
  }

  leval <- function(ui) {
    if (length(ui) == 0 || ui == "(exit)") {
      cat("\n")
      q("no")
    } else if (ui != "") {
      out <- tryCatch({
        lst <- slurp_ast(ui)
        out <- slurp_evaluate_ast(lst)
        return(out)
      },
      error = function(cond) {
        message(paste0(cond, "\n"))
        return(NULL)
      })
    } else {
      out <- ""
    }
    return(out)
  }

  lprint <- function(output) {
    ## if (!is.na(output) && !is.null(output) && output != "") {
    ##   print(output)
    ## }
    print(output)
  }

  lrep <- function() {}

  write_history <- function(ui) {
    write(ui, history_file, append = TRUE)
  }


  lmain <- function() {
    while (TRUE) {
      user_input <- lread()
      output <- leval(user_input)
      write_history(user_input)
      lprint(output)
    }
  }

  lmain()
}
