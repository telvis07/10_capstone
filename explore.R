library(ggplot2)
library(gridExtra)
source("analysis.R")
source("sample_data.R")
source("modeling.R")

get_sample_datums_vector_corpus <- function() {
  sample_vector_corpus <- list() 
  # twitter data
  sample_vector_corpus$twitter <- newline_text_file_to_corpus(filename="./data/final/en_US/sample/en_US.twitter.txt")
  sample_vector_corpus$blogs <- newline_text_file_to_corpus(filename="./data/final/en_US/sample/en_US.blogs.txt")
  sample_vector_corpus$news <- newline_text_file_to_corpus(filename="./data/final/en_US/sample/en_US.news.txt")
  sample_vector_corpus$all_corpus <- load_sample_dircorpus()
  
  sample_vector_corpus$twitter <- preprocess_entries(sample_vector_corpus$twitter)
  sample_vector_corpus$blogs <- preprocess_entries(sample_vector_corpus$blogs)
  sample_vector_corpus$news <- preprocess_entries(sample_vector_corpus$news)
  sample_vector_corpus$all_corpus <- preprocess_entries(sample_vector_corpus$all_corpus)

  # 
  sample_vector_corpus
}

do_explore_per_data_source <- function(sample_vector_corpus) {
  twitter <- sample_vector_corpus$twitter
  blogs <- sample_vector_corpus$blogs
  news <- sample_vector_corpus$news
  all_corpus <- sample_vector_corpus$all_corpus
  
  twitter_grams <- get_docterm_matrix(twitter, 1)
  blogs_grams <- get_docterm_matrix(blogs, 1)
  news_grams <- get_docterm_matrix(news, 1)
  all_corpus_ngrams <- get_docterm_matrix(all_corpus, 1)
  
  content_stats_df <- data.frame(
    source = c("twitter", "blogs", "news"),
    num_lines = c(
      length(twitter),
      length(blogs),
      length(news)
    ),
    num_unique_words = c (
      nrow(twitter_grams$wf),
      nrow(blogs_grams$wf),
      nrow(news_grams$wf)
    ),
    median_word_freq = c(
      median(twitter_grams$wf$freq),
      median(blogs_grams$wf$freq),
      median(news_grams$wf$freq)
    ),
    mean_word_freq = c(
      round(mean(twitter_grams$wf$freq), 0),
      round(mean(blogs_grams$wf$freq), 0),
      round(mean(news_grams$wf$freq), 0)
    )
  )
  content_stats_df
  
  # get frequencies
  # twitter_word_plot(twitter_grams = twitter_grams)
  
  # ngrams per source
}

ngrams_per_source_plot <- function(sample_vector_corpus,num_gram=2) {
  twitter <- sample_vector_corpus$twitter
  blogs <- sample_vector_corpus$blogs
  news <- sample_vector_corpus$news
  
  twitter_grams <- get_docterm_matrix(twitter, num_gram)
  blogs_grams <- get_docterm_matrix(blogs, num_gram)
  news_grams <- get_docterm_matrix(news, num_gram)
  title <- sprintf("Top %s-Grams by Source", num_gram)

  # # ngram top words
  p2 <- generate_word_frequency_plot(twitter_grams$wf, "twitter")
  p3 <- generate_word_frequency_plot(blogs_grams$wf, "blogs")
  p4 <- generate_word_frequency_plot(news_grams$wf, "news")
  p <- grid.arrange(p2, p3, p4, ncol=3, top=title)
  p
}

twitter_word_plot <- function(sample_vector_corpus) {
  twitter <- sample_vector_corpus$twitter
  twitter_grams <- get_docterm_matrix(twitter, 1)
  
  # get frequencies
  tbl <- table(twitter_grams$wf$freq)
  frequency_counts <- as.data.frame(tbl)
  frequency_counts$Var1 <- as.numeric(frequency_counts$Var1)
  
  # frequency plot
  obj <- ggplot(frequency_counts, aes(Var1, Freq)) +
    geom_bar(stat="identity") +
    labs(x="Number of occurences in corpus (frequency)") +
    labs(y="Number of words with identical frequency") +
    labs(title="Twitter 1% Sample: Word Frequency vs. Number of Words at each Frequency") +
    scale_x_continuous(breaks=seq(0,max(frequency_counts$Var1),25))
  
  obj
}

all_docs_word_plot <- function(sample_vector_corpus) {
  all_corpus <- sample_vector_corpus$all_corpus
  all_corpus_grams <- get_docterm_matrix(all_corpus, 1)
  
  # get frequencies
  tbl <- table(all_corpus_grams$wf$freq)
  frequency_counts <- as.data.frame(tbl)
  frequency_counts$Var1 <- as.numeric(frequency_counts$Var1)
  
  # frequency plot
  obj <- ggplot(frequency_counts, aes(Var1, Freq)) +
    geom_bar(stat="identity") +
    labs(x="Number of occurences in corpus (frequency)") +
    labs(y="Number of words with identical frequency") +
    labs(title="1% Sample: Word Frequency vs. Number of Words at each Frequency") +
    scale_x_continuous(breaks=seq(0,max(frequency_counts$Var1),25))
  
  obj
}

ngram_language_modeling_full <- function(docs=NULL) {
  # How these probabilities are estimated is a matter of great interest in the area of
  # language modeling. The most straightforward way is take a word history and count
  # the different words which follow that word history. As language models are predictive
  # models, one wants to model future possible word sequences given what was seen
  # in training. This suggests a simple relative frequency as a probability estimate of a
  # sequence of words, which is better known as the maximum likelihood estimator (see
  #                                                                               [Manning and Sch¨utze, 1999]):
  
  
  # generate_sample_files()
  if (is.null(docs)) {
    docs <- load_sample_dircorpus()
    docs <- preprocess_entries(docs)
  }
  
  ngram_2 <- get_docterm_matrix(docs, 2)
  ngram_3 <- get_docterm_matrix(docs, 3)
  ngram_4 <- get_docterm_matrix(docs, 4)
  
  # Combine all the word frequency data.frames
  ngram_all_df <- rbind(ngram_2$wf,
                        ngram_3$wf,
                        ngram_4$wf)
  
  # https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
  ngram_tree <- build_tree(ngram_all_df)
  
  ngram_tree
  
  # 
  # plot_tree_for_report(ngram_tree)
  
  #
  # predict_with_test_tree(ngram_tree)
}

ngram_language_modeling <- function(docs=NULL) {
  # How these probabilities are estimated is a matter of great interest in the area of
  # language modeling. The most straightforward way is take a word history and count
  # the different words which follow that word history. As language models are predictive
  # models, one wants to model future possible word sequences given what was seen
  # in training. This suggests a simple relative frequency as a probability estimate of a
  # sequence of words, which is better known as the maximum likelihood estimator (see
  #                                                                               [Manning and Sch¨utze, 1999]):
  
  
  # generate_sample_files()
  if (is.null(docs)) {
    docs <- load_sample_dircorpus()
    docs <- preprocess_entries(docs)
  }
  
  ngram_2 <- get_docterm_matrix(docs, 2)
  ngram_3 <- get_docterm_matrix(docs, 3)
  ngram_4 <- get_docterm_matrix(docs, 4)
  
  # Combine all the word frequency data.frames
  ngram_all_df <- rbind(ngram_2$wf,
                        ngram_3$wf,
                        ngram_4$wf)
  
  # now let's filter for ngrams that start with "data"
  starts_with_data_ngram_df <- filter(ngram_all_df, grepl("^data ", word) 
                                      & freq > 3)
  # https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
  ngram_tree <- build_tree(starts_with_data_ngram_df)
  
  ngram_tree
  
  # 
  # plot_tree_for_report(ngram_tree)
  
  #
  # predict_with_test_tree(ngram_tree)
}

plot_tree_for_report <- function(ngram_tree, 
                                 highlight_child=FALSE, 
                                 title="Tree Lookup for: data") {
  SetGraphStyle(ngram_tree, rankdir = "TB",  label = title, fontsize=40 )
  SetEdgeStyle(ngram_tree, arrowhead = "vee", color = "grey35", penwidth = 2)
  SetNodeStyle(ngram_tree, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", 
               fontname = "helvetica", tooltip = GetDefaultTooltip)
  if (highlight_child) {
    SetNodeStyle(ngram_tree$data$entry, fillcolor = "LightBlue", penwidth = "5px")
  }
  plot(ngram_tree )
}

predict_with_test_tree <- function(ngram_tree) {
  results <- perform_search(ngram_tree, c("data"))
  print("Predict next word after typing: 'data'")
  print(results)
  
  print("Predict next word after typing: 'data entry'")
  results <- perform_search(ngram_tree, c("data", "entry"))
  print(results)
}

do_explore_ngrams <- function(docs=NULL) {
  # generate_sample_files()
  if (is.null(docs)) {
    docs <- load_sample_dircorpus()
    docs <- preprocess_entries(docs)
  }

  # ngram top words
  ngram_2 <- get_docterm_matrix(docs, 2)
  p2 <- generate_word_frequency_plot(ngram_2$wf, 2)
  ngram_3 <- get_docterm_matrix(docs, 3)
  p3 <- generate_word_frequency_plot(ngram_3$wf, 3)
  ngram_4 <- get_docterm_matrix(docs, 4)
  p4 <- generate_word_frequency_plot(ngram_4$wf, 4)
  p <- grid.arrange(p2, p3, p4, ncol=3, top="Top Words by Ngram Length")
  p
}

get_sample_stats <- function() {
  tmp <- system.time({
    print("reading twitter")
    tweets <- newline_text_file_to_corpus(filename="./data/final/en_US/sample/en_US.twitter.txt")
    
    print("reading blogs")
    blogs <- newline_text_file_to_corpus(filename="./data/final/en_US/sample/en_US.blogs.txt")
    print("reading news")
    news <- newline_text_file_to_corpus(filename="./data/final/en_US/sample/en_US.news.txt")
    print("Joining data")
    docs <- c(tweets, blogs, news)
    print(sprintf("Saving vector corpus to %s", save_file))
    saveRDS(docs, save_file)
  })
  print(tmp)
  docs
}

ngram_frequency_stats <- function(df, ngram_length=2) {
  
  # 3. How many unique words do you need in a frequency sorted dictionary 
  # to cover 50% of all word instances in the language?
  sums <- cumsum(df$freq)
  cover_50 <- which(sums > sum(df$freq) * .50)[1]
  
  stats_df <- data.frame(
    min=min(df$freq),
    max=max(df$freq),
    mean=mean(df$freq),
    count=nrow(df),
    cover_50=cover_50/nrow(df)*100 
  )
  
  stats_df
  
}

generate_word_frequency_plot <- function(df, title="") {
  # convert 'word' to a factor variable that is sorted by the frequency.
  # so the plot will be in decreasing order by frequency
  top_df <- df[1:20,]
  top_df$word <- factor(top_df$word, levels=top_df[order(top_df$freq), "word"])
  p <- ggplot(top_df, aes(x=word, y=freq))  + 
    geom_bar(stat="identity") +
    labs(x="word") +
    labs(y="frequency") +
    labs(title=title) +
    coord_flip()  
}

save_plot <- function(obj, filename="plots/default.png") {
  png(filename, width = 960, height = 960, units = "px")
  print(obj)
  dev.off()
  print(sprintf("Saved %s", filename))
}

explore_ngram_data <- function(df, ngram_length=2) {
  # top 10 words 
  print(head(df))
  
  # plot top 10
  save_plot_filename <- sprintf("plots/top_words_%s_ngram.png", ngram_length)
  top_df <- df[1:20,]
  # convert 'word' to a factor variable that is sorted by the frequency.
  # so the plot will be in decreasing order by frequency
  top_df$word <- factor(top_df$word, levels=top_df[order(top_df$freq), "word"])
  p <- ggplot(top_df, aes(x=word, y=freq))  + 
    geom_bar(stat="identity") +
    labs(x="Ngram") +
    labs(y="Sampled Count") +
    labs(title="Top Words by Ngram Length") +
    # theme(text = element_text(size=8)) +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    coord_flip()  
  save_plot(p, save_plot_filename)
  
  # get frequencies
  tbl <- table(df$freq)
  frequency_counts <- as.data.frame(tbl)
  
  # frequency plot
  save_plot_filename <- sprintf("plots/log_frequencies_%s_ngram.png", ngram_length)
  print("Hist plotting the term frequencies")
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

cover_percentage_50_90 <- function(df) {
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

cover_percentage <- function(df, instance_percent=0.50) {
  # 3. How many unique words do you need in a frequency sorted dictionary 
  # to cover 50% of all word instances in the language? 90%?
  sums <- cumsum(df$freq)
  cover_n <- which(sums > sum(df$freq) * instance_percent)[1]
  ret = list(num_words=cover_n,
             total_words=nrow(df),
             percent_of_total_words=round(cover_n/nrow(df)*100, 1))
  ret
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

plot_word_frequencies_for_df <- function(df) {
  tbl <- table(df$freq)
  frequency_counts <- as.data.frame(tbl)
  obj <- qplot(log(frequency_counts$Freq), frequency_counts$Var1)
  obj
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