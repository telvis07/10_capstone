rm(list=ls())
setwd('/Users/telvis/work/datasciencecoursera/10_capstone')

reinit_environment <- function() {
  tmp = system.time({
    # datums = list()
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
    # datums$all_df <- rbind(datums$df_ngram_2,
    #                    datums$df_ngram_3,
    #                    datums$df_ngram_4)

    # datums$df_ngram_all <- readRDS("data/pruned_50p_term_doc_matrix_all_ngram_df.rds")
    # min_frequency=100
    # datums$df_ngram_all <- filter(datums$df_ngram_all, freq > min_frequency)
    
    # datums$df_ngram_all <- readRDS("data/pruned_50p_term_doc_matrix_all_ngram_minfreq_100_df.rds")
    # datums$ngram_tree <- readRDS("data/ngram_all_minfreq_175_tree.rds")
    # ngram_df_list <- readRDS("data/ngram_df_list.5.percent.rds")
  })
  print(tmp)
  # ngram_df_list
}




ngram_df_list <- reinit_environment()
# source("analysis.R")
# source("explore.R")
# source("sample_data.R")
# source("search_with_dataframes.R")