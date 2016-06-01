rm(list=ls())
setwd('/Users/telvis/work/datasciencecoursera/10_capstone')

reinit_environment <- function() {
  tmp = system.time({
    
    
    datums = list()
    # All datums
    # datums$df_ngram_2 <- readRDS("data/term_doc_matrix_2_ngram_df.rds")
    # datums$df_ngram_3 <- readRDS("data/term_doc_matrix_3_ngram_df.rds")
    # datums$df_ngram_4 <- readRDS("data/term_doc_matrix_4_ngram_df.rds")
    
    # # Datums after pruning
    # datums$df_ngram_2 <- readRDS("data/pruned_50p_term_doc_matrix_2_ngram_df.rds")
    # datums$df_ngram_2 <- mutate(datums$df_ngram_2, 
    #                             word=as.character(word))
    # datums$df_ngram_3 <- readRDS("data/pruned_50p_term_doc_matrix_3_ngram_df.rds")
    # datums$df_ngram_3 <- mutate(datums$df_ngram_3, 
    #                             word=as.character(word))
    # datums$df_ngram_4 <- readRDS("data/pruned_50p_term_doc_matrix_4_ngram_df.rds")
    # datums$df_ngram_4 <- mutate(datums$df_ngram_4, 
    #                             word=as.character(word))
    # datums$all_df <- c(datums$df_ngram_2,
    #                    datums$df_ngram_3,
    #                    datums$df_ngram_4)

    datums$df_ngram_all <- readRDS("data/pruned_50p_term_doc_matrix_all_ngram_df.rds")
    
  })
  print(tmp)
  datums
}

# from http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}


datums <- reinit_environment()
source("analysis.R")
source("explore.R")
print(lsos())