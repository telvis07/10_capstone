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
  
  # > cover_percentage(datums$df_ngram_2)
  # [1] "1137868 of 20514021 (5.54678188152386%) cover 50% of word instances"
  # [1] "15313787 of 20514021 (74.6503428070001%) cover 90% of word instances"
  #                        Type       Size    PrettySize Rows Columns
  # datums                 list 3071531248  [1] "2.9 Gb"    1      NA
  
  
  # > cover_percentage(datums$df_ngram_3)
  # [1] "17793995 of 41719440 (42.6515672310079%) cover 50% of word instances"
  # [1] "36934351 of 41719440 (88.5303134462016%) cover 90% of word instances"
  # datums                    list 6988597936  [1] "6.5 Gb"    1      NA
  
  # > cover_percentage(datums$df_ngram_4)
  # [1] "20568337 of 42556490 (48.3318455069955%) cover 50% of word instances"
  # [1] "38158860 of 42556490 (89.6663705112898%) cover 90% of word instances"
  #                           Type       Size    PrettySize Rows Columns
  # datums                    list 7545601208    [1] "7 Gb"    1      NA
}


plot_word_frequencies <- function(dtm) {
  print("using colsums/head")
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
  head(freq, 14) 
  wf <- data.frame(word=names(freq), freq=freq)   
  print(head(wf)) 
  
  p <- ggplot(subset(wf, freq>15), aes(word, freq))    
  p <- p + geom_bar(stat="identity")   
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
  p
}

plot_wordcloud <- function(dtm) {
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
  set.seed(142)   
  wordcloud(names(freq), freq, min.freq=10, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))  
}

plot_wordcloud_top_n <- function(dtm, max.words=15) {
  freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
  set.seed(142)   
  dark2 <- brewer.pal(6, "Dark2")   
  wordcloud(names(freq), freq, max.words=max.words, rot.per=0.2, colors=dark2)  
}

hierarchical_cluster <- function(dtm) {
  d <- dist(t(dtm), method="euclidian")   
  fit <- hclust(d=d, method="ward")   
  fit
  
  plot(fit, hang=-1)   
}

kmeans_plot <- function(dtm) {
  d <- dist(t(dtm), method="euclidian")   
  kfit <- kmeans(d, 2)   
  clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
}