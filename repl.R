source("ast.R")
source("evaluation.R")

repl <- function() {
  prompt <- "SluRp> "
  history_file <- ".slurp.history"

  lread <- function() {
    cat(prompt)
    answer <- readLines("stdin", n = 1)
    return(answer)
  }

  leval <- function(ui) {
    if (ui == "(exit)") {
      q("no")
    } else if (ui != "") {
      out <- tryCatch({
        lst <- ast(ui)
        out <- evaluate_ast(lst)
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
    ## if (!is.null(output) && output != "") {
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

repl()
