source("ast.R")

repl <- function() {
  prompt <- "SluRp> "
  history_file <- ".slurp.history"

  lread <- function() {
    cat(prompt)
    answer <- readLines("stdin", n = 1)
    return(answer)
  }

  leval <- function(ui) {
    ## if (stringi::stri_enc_toascii(ui)) {
    ##   q("no")
    ## }
    if (ui == "(exit)") {
      q("no")
    } else if (ui != "") {
      out <- ast(ui)
    } else {
      out <- ""
    }
    return(out)
  }

  lprint <- function(output) {
    if (!is.null(output) && output != "") {
      print(output)
    }
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
