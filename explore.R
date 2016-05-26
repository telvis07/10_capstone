library(ggplot2)

save_plot <- function(obj, filename="plots/default.png") {
  png(filename, width = 960, height = 960, units = "px")
  print(obj)
  dev.off()
  print(sprintf("Saved %s", filename))
}

explore_ngram_data <- function(df=NULL, ngram_length=2) {
  if (is.null(df)) {
    save_file_df="data/term_doc_matrix_%s_ngram_df.rds"
    save_file_df <- sprintf(save_file_df, ngram_length)
    print(sprintf("Reading %s", save_file_df))
    df <- readRDS(save_file_df)
  }
  
  # top 10 words 
  print(head(df))
  
  # plot top 10
  save_plot_filename <- sprintf("plots/top_words_%s_ngram.png", ngram_length)
  top_df <- df[1:10,]
  p <- ggplot(top_df, aes(word, freq))    
  p <- p + geom_bar(stat="identity")   
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
  save_plot(p, save_plot_filename)
  
  # get frequencies
  tbl <- table(df$freq)
  frequency_counts <- as.data.frame(tbl)
  
  # frequency plot
  save_plot_filename <- sprintf("plots/log_frequencies_%s_ngram.png", ngram_length)
  print("Hist plotting the term frequencies")
  # obj <- qplot(log(df$freq))
  obj <- qplot(log(frequency_counts$Freq), frequency_counts$Var1)

  # plot the distribution of the frequencies
  save_plot(obj, save_plot_filename)
  
  # top 6 frequencies
  # print(head(table(df$freq)))
  # print(head(prop.table(table(df$freq))))
  print(head(tbl))
  print(head(prop.table(tbl)))
  
  # 2-ngram
  # 1        2        3        4        5        6 
  # 15393042  2344343   900319   473764   290214   195183 
  # 
  # 1           2           3           4           5           6 
  # 0.750366883 0.114280033 0.043887983 0.023094643 0.014147105 0.009514614 
  
  # 3-ngram
  #           1           2           3           4           5           6 
  # 0.939112054 0.039699598 0.009700753 0.004048329 0.002122727 0.001276024 
}

cover_percentage <- function(df) {
  # 3. How many unique words do you need in a frequency sorted dictionary 
  # to cover 50% of all word instances in the language? 90%?
  sums <- cumsum(df$freq)
  cover_50 <- which(sums > sum(df$freq) * .50)[1]
  print(sprintf("%s of %s (%s%%) cover 50%% of word instances", cover_50, nrow(df), cover_50/nrow(df)*100))
  cover_90 <- which(sums > sum(df$freq) * .90)[1]
  print(sprintf("%s of %s (%s%%) cover 90%% of word instances", cover_90, nrow(df), cover_90/nrow(df)*100))
}