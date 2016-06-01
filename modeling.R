library(data.tree)
library(dplyr)

gen_path_string <- function (x) {
  gsub(" ", "/", x$word)
}