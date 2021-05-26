getScriptPath <- function(){
    cmd.args <- commandArgs()
    m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
    script.dir <- dirname(regmatches(cmd.args, m))
    if(length(script.dir) == 0) stop("can't determine script dir: please call the script with Rscript")
    if(length(script.dir) > 1) stop("can't determine script dir: more than one '--file' argument detected")
    return(script.dir)
}

d <- getScriptPath()

source(file.path(d, "R/ast.R"))
source(file.path(d, "R/evaluation.R"))

repl <- function() {
  prompt <- "SluRp> "
  history_file <- ".slurp.history"

  lread <- function() {
    cat(prompt)
    answer <- readLines("stdin", n = 1)
    return(answer)
  }

  leval <- function(ui) {
    if (length(ui) == 0 || ui == "(exit)") {
      cat("\n")
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
