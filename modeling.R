library(data.tree)

gen_path_string <- function (x) {
  gsub(" ", "/", x$word)
}