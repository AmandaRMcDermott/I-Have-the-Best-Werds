# BTM Functions

# Load libraries
require(topicmodels)
require(tidyverse)
require(stringr)
require(tidytext)
require(readr)
require(forcats)
require(textmineR)
require(tm)
require(igraph)
require(LDAvis)
require(lda)
require(maptpx)
require(BTM)
require(udpipe)
require(SnowballC)
require(mgsub)
require(tidytext)

# (1) Clean text -
# Word Substitutions, word removals, and stemming
clean_text2 <- function(df, text_column, stem = T) {
  
  print("Replacing and removing words...")
  # word substitutions and removals
  df <- df[,..text_column][, id := 1:nrow(df)][
    , eval(text_column) := str_replace_all(get(text_column), quadgrams)][
      , eval(text_column) := str_replace_all(get(text_column), trigrams)][
        , eval(text_column) := str_replace_all(get(text_column), bigrams)][
          , eval(text_column) := str_replace_all(get(text_column), char_to_num)][
            , eval(text_column) := str_replace_all(get(text_column), replace)]
  print()
  df <- unnest_tokens(df, word, !!text_column, token = "ngrams", n = 1)
  
  df<-setDT(df)[, word := gsub("\\'s", "", word)]
  df <- df[!setDT(data.frame(tidytext::stop_words)),on="word"]
  df<-df[!setDT(data.frame(word=tm::stopwords())), on = "word"]
  df<-df[!setDT(stopwords1), on = "word"]
  df<-df[!setDT(custom_stopwords), on = "word"]
  
  if(stem == T) {
    print("Stemming words...")
    df[, word := wordStem(word, language = "porter")]
  }
  
  print("Cleaning up dataframe...")
  df <- distinct(df[,text := str_c(word, collapse = " "), by = id][,word:=NULL],text,id, .keep_all = T)
  
  print("Last pass through replacing and removing words...")
  df[,text := str_replace_all(text, quadgrams)][
    , text := str_replace_all(text, trigrams)][
      , text := str_replace_all(text, bigrams)][
        , text := str_replace_all(text, char_to_num)][
          , text := str_replace_all(text, replace)]
  
  return(df[])
  
}


# (2) Gibbs Sampler for BTM
calc_mdl <- function(df, K, iter = 200, biterms, window){
  
  # Building the model
  set.seed(18)
  
  # For the trace:
  trace <- iter/10
  
  if (missing(biterms)) {
    
    btm_mdl1 <- BTM::BTM(df, 
                         k = K, # determine optimal K value
                         #beta = 0.01,
                         beta = 0.01,
                         iter = iter,
                         trace = trace,
                         detailed = T, 
                         background = T,
                         window = window)
    
  } else {
    
    btm_mdl1 <- BTM::BTM(df, 
                         k = K, # determine optimal K value
                         #beta = 0.01,
                         beta = 0.01,
                         iter = iter,
                         trace = trace,
                         biterms = biterms,
                         detailed = T, 
                         background = T,
                         window = window)
    
  }
  
  topicsterms1 <- terms(btm_mdl1, top_n = 10)
  
  dtm1 <- suppressWarnings(
    df %>% count(doc_id, word) %>% cast_dtm(term = word, document = doc_id, value = n)
  )
  
  calc_topic_coh <- function(K){
    
    top_words <- as.vector(topicsterms1[[K]]$token)
    
    out <- topic_coherence(top_words = top_words,
                           document_term_matrix = as.matrix(dtm1),
                           vocabulary = btm_mdl1$vocabulary$token,
                           K = 10) # top 10 words
    df <- tibble(topic_coh = out)
    df
  }
  
  topic_coh <- map_df(1:K, calc_topic_coh)
  
  topic_coh <- topic_coh %>% 
    mutate(topic = 
             as_factor(paste("Topic", seq(from = 1, to = K, by = 1))),
           num_topics = as_factor(paste(K, "Topics"))) %>% 
    dplyr::select(topic, topic_coh)
  
  # Average Coherence Score - 1/k*sum of coherence scores across all k
  add_row <- tibble(topic_coh = (1/K) * sum(topic_coh$topic_coh), topic = 
                      paste("Average Coherence for", K, "Topics"))
  
  topic_coh <- rbind(topic_coh, add_row)
  
  btm_mdl1$topic_coh <- topic_coh %>% arrange(desc(topic_coh))
  
  print(paste0("Model with ", K, " topics completed."))
  
  btm_mdl1
} 

# () Average Topic Coherence by K
best_coh_mdl <- function(mdls, min_k, max_k, override = F, mnl_k) {
  
  # min and max k values used
  min = min_k
  max = max_k
  
  avg_scores <- function(x){
    
    # arrange topics by best average coherence score
    df <- mdls[[x]][["topic_coh"]] %>% filter(str_detect(topic, "Average"))
    
    # return data
    df
  }
  
  # make tibble of average coherence score by K
  temp_df <- suppressWarnings(
    map_df(1:length(mdls), avg_scores) %>% arrange(desc(topic_coh))
  )
  
  if (override == T) {
    opt_k <<- mnl_k
    
    # Topic Coherence Score for the Model with the highest coherence score
    coh_tbl_topics <- suppressMessages(mdls[[(opt_k-min)+1]][["topic_coh"]])
    
    df <- list()
    df$avg_topic_coh <- temp_df
    df$best_coh <- coh_tbl_topics
    df$best_mdl <- mdls[[(opt_k-min)+1]]
    
  } else if( override == F) {
   
    # topic with highest coherence score
    opt_k <<- suppressMessages(as.double(str_extract(temp_df$topic[1], "\\d")))
    
    print(paste("Optimal Topic Number:", opt_k))
    
    # Topic Coherence Score for the Model with the highest coherence score
    coh_tbl_topics <- suppressMessages(mdls[[(opt_k-min)+1]][["topic_coh"]])
    
    df <- list()
    df$avg_topic_coh <- temp_df
    df$best_coh <- coh_tbl_topics
    df$best_mdl <- mdls[[(opt_k-min)+1]]
     
  }
  
  return(df)
}

# (3) Topic Coherence
topic_coherence <- function(top_words,
                            document_term_matrix,
                            vocabulary = NULL,
                            numeric_top_words = FALSE,
                            K = length(top_words)){
  
  # make sure the data is the right format
  vocabulary <- as.character(vocabulary)
  
  # perform some basic checks and throw errors if we see something weird.
  if(is.null(vocabulary) & !numeric_top_words){
    stop("You must provide a vocabulary vector!")
  }
  if(K > length(top_words)){
    K <- length(top_words)
    warning(paste("You must select a value for K that is less than length(top_words). K has automatically been set to :",K,sep = " "))
  }
  if(length(vocabulary) != ncol(document_term_matrix)){
    stop("The vocaublary vector must have the same number of entries as the number of columns in the document_term_matrix, and the word indicated by entries in the i'th column of document_term_matrix must correspond to the i'th entry in vocabulary.")
  }
  
  #if we are only using the K top words then reduce our top words vector
  top_words <- top_words[1:K]
  
  # binarize the document term matrix
  document_term_matrix <- matrix(as.numeric(document_term_matrix > 0),
                                 nrow = nrow(document_term_matrix),
                                 ncol = ncol(document_term_matrix))
  coherence_score <- 0
  for(i in 2:length(top_words)){
    for(j in 1:(i-1)){
      # we can either look up against vocab or just use indexes
      if(numeric_top_words){
        jindex <- top_words[j]
        iindex <- top_words[i]
      }else{
        jindex <- which(vocabulary == top_words[j])
        iindex <- which(vocabulary == top_words[i])
      }
      
      document_frequency <- sum(document_term_matrix[,jindex])
      j_positive <- which(document_term_matrix[,jindex] > 0)
      i_positive <- which(document_term_matrix[,iindex] > 0)
      co_document_frequency <- sum(i_positive %in% j_positive)
      
      coherence_score <- coherence_score + log((co_document_frequency + 1)/document_frequency)
      
    }
  }
  if(is.infinite(coherence_score)){
    coherence_score <- NA
    warning("The coherence score was not finite. Make sure that all words in your vocabulary appear atleast once.")
  }
  return(coherence_score)
}

# (4) Coherence score for one topic
calc_topic_coh <- function(topic){
  top_words <- as.vector(topicsterms1[[topic]]$token)
  out <- topic_coherence(top_words = top_words,
                         document_term_matrix = as.matrix(dtm1),
                         vocabulary = btm_mdl1$vocabulary$token,
                         K = 10) # top 10 words
  df <- tibble(topic_coh = out)
  df
}

# (5) Custom DTM
custom.dtm  = function(x1,scheme){
  
  # x1 = text corpus
  # scheme = tf or tfidf
  
  tdm = TermDocumentMatrix(x1)
  
  a1 = apply(tdm, 1, sum)
  a2 = ((a1 >= 2))
  tdm.new = tdm[a2,]
  
  # remove blank documents (i.e. columns with zero sums)
  a0 = NULL
  
  for (i1 in 1:ncol(tdm.new)) {
    if (sum(tdm.new[, i1]) == 0) {
      a0 = c(a0, i1)
    }
  }
  length(a0)    # no. of empty docs in the corpus
  if (length(a0) > 0) {
    tdm.new1 = tdm.new[,-a0]
  } else {
    tdm.new1 = tdm.new
  }
  
  
  dim(tdm.new1)    # reduced tdm
  if (scheme == "tfidf") {
    x2mat = t(weightTfIdf(tdm.new1))
  }
  else {
    x2mat = t((tdm.new1))
  }
  return(x2mat)
}

# (6) LDAvis for Customer and Employee Remarks
ldavis_topic_join <- function(mdls, x){
  # get phi
  phi1 <- t(mdls[[x]][["phi"]])
  
  # get doc size and thus length
  doc_size1 <- table(join_words$id)
  doc_length1 <- as.integer(doc_size1)
  
  # get theta
  scores1 <- predict(mdls[[x]], join_words)
  scores1 <- scores1[names(doc_size1),]
  
  # get vocab
  vocab1 <- mdls[[x]][["vocabulary"]][["token"]]
  vocab_temp1 <- mdls[[x]][["vocabulary"]]
  
  # get term freq
  term_freq1 <- mdls[[x]][["vocabulary"]][["freq"]]
  
  json1 <- createJSON(phi = phi1,
                      theta = scores1,
                      doc.length = doc_length1, 
                      vocab = vocab1, 
                      term.frequency = term_freq1)
  
  json1
}

# LDAvis for one dataframe
ldavis_topic <- function(mdls, df, x){
  # get phi
  phi1 <- t(mdls[[x]][["phi"]])
  
  # get doc size and thus length
  doc_size1 <- table(df$doc_id)
  doc_length1 <- as.integer(doc_size1)
  
  # get theta
  scores1 <- predict(mdls[[x]], df)
  scores1 <- scores1[names(doc_size1),]
  
  # get vocab
  vocab1 <- mdls[[x]][["vocabulary"]][["token"]]
  vocab_temp1 <- mdls[[x]][["vocabulary"]]
  
  # get term freq
  term_freq1 <- mdls[[x]][["vocabulary"]][["freq"]]
  
  json1 <- createJSON(phi = phi1,
                      theta = scores1,
                      doc.length = doc_length1, 
                      vocab = vocab1, 
                      term.frequency = term_freq1)
  
  json1
}

# (7) Add top words to STM model
add_word_pred <- function(df,top_words_df, K) {
  # Split top words into chunks
  x <- seq_along(top_words_df)
  word_chunks <- split(top_words_df,ceiling(x/K))
  
  # Add top words
  df$high_prob <- word_chunks[[1]]
  df$frex <- word_chunks[[2]]
  df$lift <- word_chunks[[3]]
  df$score <- word_chunks[[4]]
  
  # Get lda_viz_topic_n
  df <- df %>%
    arrange(desc(N)) %>% 
    mutate(lda_vis_topic_num = 1:K) %>% 
    setDT()
  
  return(df)
}

# (8) Create JSON for non-ordered Topics by Prop
createJSON_custom <- function (phi = matrix(), theta = matrix(), doc.length = integer(), 
          vocab = character(), term.frequency = integer(), R = 30, 
          lambda.step = 0.01, mds.method = jsPCA, cluster, plot.opts = list(xlab = "PC1", 
                                                                            ylab = "PC2"), 
          reorder.topics = TRUE) 
{
  dp <- dim(phi)
  dt <- dim(theta)
  N <- sum(doc.length)
  W <- length(vocab)
  D <- length(doc.length)
  K <- dt[2]
  if (dp[1] != K) 
    stop("Number of rows of phi does not match \n      number of columns of theta; both should be equal to the number of topics \n      in the model.")
  if (D != dt[1]) 
    stop("Length of doc.length not equal \n      to the number of rows in theta; both should be equal to the number of \n      documents in the data.")
  if (dp[2] != W) 
    stop("Number of terms in vocabulary does \n      not match the number of columns of phi (where each row of phi is a\n      probability distribution of terms for a given topic).")
  if (length(term.frequency) != W) 
    stop("Length of term.frequency \n      not equal to the number of terms in the vocabulary.")
  if (any(nchar(vocab) == 0)) 
    stop("One or more terms in the vocabulary\n      has zero characters -- all terms must have at least one character.")
  phi.test <- all.equal(rowSums(phi), rep(1, K), check.attributes = FALSE)
  theta.test <- all.equal(rowSums(theta), rep(1, dt[1]), check.attributes = FALSE)
  if (!isTRUE(phi.test)) 
    stop("Rows of phi don't all sum to 1.")
  if (!isTRUE(theta.test)) 
    stop("Rows of theta don't all sum to 1.")
  topic.frequency <- colSums(theta * doc.length)
  topic.proportion <- topic.frequency/sum(topic.frequency)
  
  if (reorder.topics == T){
    o <- order(topic.proportion, decreasing = TRUE) 
  } else if(reorder.topics == F) {
    o <- topic.proportion   
  }
  phi <- phi[o, ]
  theta <- theta[, o]
  topic.frequency <- topic.frequency[o]
  topic.proportion <- topic.proportion[o]
  mds.res <- mds.method(phi)###
  if (is.matrix(mds.res)) {
    colnames(mds.res) <- c("x", "y")
  }
  else if (is.data.frame(mds.res)) {
    names(mds.res) <- c("x", "y")
  }
  else {
    warning("Result of mds.method should be a matrix or data.frame.")
  }
  mds.df <- data.frame(mds.res, topics = seq_len(K), Freq = topic.proportion * 
                         100, cluster = 1, stringsAsFactors = FALSE)
  term.topic.frequency <- phi * topic.frequency
  term.frequency <- colSums(term.topic.frequency)
  stopifnot(all(term.frequency > 0))
  term.proportion <- term.frequency/sum(term.frequency)
  phi <- t(phi)
  topic.given.term <- phi/rowSums(phi)
  kernel <- topic.given.term * log(sweep(topic.given.term, 
                                         MARGIN = 2, topic.proportion, `/`))
  distinctiveness <- rowSums(kernel)
  saliency <- term.proportion * distinctiveness
  default.terms <- vocab[order(saliency, decreasing = TRUE)][1:R]
  counts <- as.integer(term.frequency[match(default.terms, 
                                            vocab)])
  Rs <- rev(seq_len(R))
  default <- data.frame(Term = default.terms, logprob = Rs, 
                        loglift = Rs, Freq = counts, Total = counts, Category = "Default", 
                        stringsAsFactors = FALSE)
  topic_seq <- rep(seq_len(K), each = R)
  category <- paste0("Topic", topic_seq)
  lift <- phi/term.proportion
  find_relevance <- function(i) {
    relevance <- i * log(phi) + (1 - i) * log(lift)
    idx <- apply(relevance, 2, function(x) order(x, decreasing = TRUE)[seq_len(R)])
    indices <- cbind(c(idx), topic_seq)
    data.frame(Term = vocab[idx], Category = category, logprob = round(log(phi[indices]), 
                                                                       4), loglift = round(log(lift[indices]), 4), stringsAsFactors = FALSE)
  }
  lambda.seq <- seq(0, 1, by = lambda.step)
  if (missing(cluster)) {
    tinfo <- lapply(as.list(lambda.seq), find_relevance)
  }
  else {
    tinfo <- parallel::parLapply(cluster, as.list(lambda.seq), 
                                 find_relevance)
  }
  tinfo <- unique(do.call("rbind", tinfo))
  tinfo$Total <- term.frequency[match(tinfo$Term, vocab)]
  rownames(term.topic.frequency) <- paste0("Topic", seq_len(K))
  colnames(term.topic.frequency) <- vocab
  tinfo$Freq <- term.topic.frequency[as.matrix(tinfo[c("Category", 
                                                       "Term")])]
  tinfo <- rbind(default, tinfo)
  ut <- sort(unique(tinfo$Term))
  m <- sort(match(ut, vocab))
  tmp <- term.topic.frequency[, m]
  r <- row(tmp)[tmp >= 0.5]
  c <- col(tmp)[tmp >= 0.5]
  dd <- data.frame(Term = vocab[m][c], Topic = r, Freq = round(tmp[cbind(r, 
                                                                         c)]), stringsAsFactors = FALSE)
  dd[, "Freq"] <- dd[, "Freq"]/term.frequency[match(dd[, "Term"], 
                                                    vocab)]
  token.table <- dd[order(dd[, 1], dd[, 2]), ]
  RJSONIO::toJSON(list(mdsDat = mds.df, tinfo = tinfo, token.table = token.table, 
                       R = R, lambda.step = lambda.step, plot.opts = plot.opts, 
                       topic.order = o))
}

# (9) 
toLDAvisJson_custom <- function (mod, docs, R = 30, plot.opts = list(xlab = "PC1", 
                                              ylab = "PC2"), lambda.step = 0.1, reorder.topics = TRUE) 
{
  if (!requireNamespace("LDAvis", quietly = TRUE)) 
    stop("Please install LDAvis package to use this function. You will also need servr.")
  theta <- mod$theta
  if (length(mod$beta$logbeta) > 1) 
    stop("This function does not yet allow content covariates.")
  phi <- exp(mod$beta$logbeta[[1]])
  if (any(phi == 0)) {
    phi <- phi + .Machine$double.eps
    phi <- phi/rowSums(phi)
  }
  vocab <- mod$vocab
  doc.length <- as.integer(unlist(lapply(docs, function(x) sum(x[2, 
                                                                 ]))))
  term.frequency <- mod$settings$dim$wcounts$x
  createJSON_custom(plot.opts = plot.opts, phi = phi, theta = theta, 
                     doc.length = doc.length, vocab = vocab, term.frequency = term.frequency, 
                     lambda.step = lambda.step, reorder.topics = reorder.topics)
}

# (10) Custom LDAvis function
toLDAvis_custom <- function (mod, docs, R = 30, plot.opts = list(xlab = "PC1", 
                                              ylab = "PC2"), lambda.step = 0.1, out.dir = tempfile(), 
          open.browser = interactive(), as.gist = FALSE, reorder.topics = TRUE) 
{
  f <- toLDAvisJson_custom(mod, docs, R = R, plot.opts = plot.opts, 
                    lambda.step = lambda.step, reorder.topics = reorder.topics)
  LDAvis::serVis(f, out.dir = out.dir, open.browser = open.browser, 
                 as.gist = as.gist, R = R)
}