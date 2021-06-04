strip_comments <- function(input) {
  stringr::str_remove_all(input, ";;.*")
}
