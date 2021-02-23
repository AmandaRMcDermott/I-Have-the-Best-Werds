#%%%%%%%%%%%%%%%%%%%%%%%% Topic Model Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Date: 2/18/2021
#
# Functions for modeling and plotting topic model results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(data.table)
library(tm)
library(LDAvis)
library(lda)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(stm)
library(quanteda)

# LDA Modeling using K topics
topic_model <- function(x){
  print(paste0("Fitting with ", k_topics[x], " topics..."))
  mdl <-
    FitLdaModel(
      dtm = dtm,
      k = k_topics[x],
      iterations = 500,
      burnin = 300,
      alpha = 0.1,
      beta = 0.05,
      optimize_alpha = T,
      calc_likelihood = T,
      calc_coherence = T
    )
  mdl$k <- k_topics[x]
  mdl$coherence <- CalcProbCoherence(phi = mdl$phi, dtm = dtm, M = 5)  
  return(mdl)
}



# Best K Coherence Dataframe Output
best_coh <- function(model_list){
  coherence <<- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                           coherence = sapply(mdls, function(x) mean(x$coherence)))
  
  setorder(coherence,-coherence)
  #min_topics <- min(coherence$k)
  #max_coh <-  max(coherence$coherence)
  
  p<-coherence %>% 
    mutate(best = factor(ifelse(coherence == max(coherence),1,0))) %>% 
    ggplot(., aes(x = k, y = coherence)) +
    geom_line() +
    geom_point(aes(color = best),size=2) +
    scale_color_manual(values = c("black","red"),  labels = c("", "Best Score"))+
    scale_x_continuous(breaks = seq(1,20,1))+
    labs(y = "Coherence", x = "Number of Topics") +
    ggtitle(paste0("The optimal number of topics is ", coherence$k[1]))+
    theme_minimal()+
    theme(legend.position = "none")
  return(p)
}


# Outputs best model to global environment and displays dendrogram of topics
get_best_mdl <- function(mdls) {
  
  setorder(coherence,k)
  best_mdl <<- mdls[which.max(coherence$coherence)][[1]]
  
  best_mdl$top_terms <- GetTopTerms(best_mdl$phi, M = 20)
  as.data.frame(best_mdl$top_terms)
  
  best_mdl$topic_linguistic_dist <- CalcHellingerDist(best_mdl$phi)
  best_mdl$hclust <-
    hclust(as.dist(best_mdl$topic_linguistic_dist), "ward.D")
  best_mdl$hclust$labels <-
    paste(best_mdl$hclust$labels, best_mdl$labels[, 1])
  
  return(plot(best_mdl$hclust))
  
}