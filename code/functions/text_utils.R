### Text utilities

dither  <- function (x, s = 0.05) { x + s*sd(x, na.rm=T)*rnorm(length(x)) }

reset <- function(lessMargin=T) { 
  # bottom, left, top, right
  if (lessMargin) { mar <- c(2.5,2.5,1.5,0.5)+.1; mgp <- c(1.5,.5,0) }
  else            { mar <- c(5,4,4,2)+.1; mgp <- c(3,1,0) }	
  .old.par <<- par(mfrow=c(1,1), mar=mar ,mgp=mgp, 
                   fg="black",bg="white",col="black",	
                   col.main="black", col.sub="black", col.lab="black", col.axis="black"); 
  
  palette("default");  
}


words_from_string <- function (text) {
  return( unlist(strsplit(prepare_text(text),"\\s+")) )  # gobble up double space
}


zipf_plot <- function(freq, n.label=5, n.fit=200) {
  # Returns log-log regression
  freq <- sort(freq, decreasing=T)
  x<-1:length(freq); y<-freq
  df <- data.frame(list(x=x,y=y,lx=log(x),ly=log(y)))
  plot(y~x, xlab="Rank", ylab="Frequency", log="xy", data=df)
  common <- names(freq)[1:n.label]
  text(0.9*x[1:n.label],0.7*y[1:n.label],common,cex=0.75)
  regr<-lm(ly~lx, data=df[1:min(n.fit, nrow(df)),]); 
  lx <- log(x<-c(1,5000)); y <- exp(predict(regr, data.frame(lx=lx)))
  lines(x,y,col="red")
  return(regr)
}

### fix word names... must be better way to do with a table
rename_punctuation <- function(str) {
  require(stringr)
  # s <- gsub("([\\])","_bslash", str)			# yuck! do this one first
  s <- str_replace(str, "_,_",   "comma_")	# extra _ in case you have "comma"
  s <- str_replace(s  , "_;_",   "semi_")	
  s <- str_replace(s  , "_:_",   "colon_")	
  s <- str_replace(s  , "_-_",   "dash_")	
  s <- str_replace(s  , "_!_",   "exclam_")	
  s <- str_replace(s  , "_/_",   "slash_")	
  s <- str_replace(s  , "_§_",   "section_")	
  s <- str_replace(s  , "_&_",   "amp_")	
  s <- str_replace(s  , "_´_",   "bquote_")	
  s <- str_replace(s  , "_¨_",   "snake_")	
  s <- str_replace(s  , "_©_",   "cw_")	
  s <- str_replace(s  , "_±_",   "pm_")	
  s <- str_replace(s  , "_=_",   "equal_")	
  s <- str_replace(s  , "_%_",   "pct_")	
  s <- str_replace(s  , "_\\$_",  "dollar_")	
  s <- str_replace(s  , "_\\+_",  "plus_")	# note escape char
  s <- str_replace(s  , "_\\._", "period_")	
  s <- str_replace(s  , "_\\(_", "lparen_")	
  s <- str_replace(s  , "_\\)_", "rparen_")	
  s <- str_replace(s  , "_\\?_", "quest_")	
  s <- str_replace(s  , "_\\'_", "quote_")	
  s <- str_replace(s  , "_\"_",  "dquote_")	
  s <- str_replace(s  , "_\\[_", "lbrac_")	
  return (s)
}	

coef_cloud <- function(coefs) {
  require(wordcloud)
  v <- as.vector(coefs)[-1]				# no intercept
  nm <- (attributes(coefs)$Dimnames[[1]])[-1]
  par(mfrow=c(1,2))
  neg <- v<0
  wordcloud(nm[ neg], v[ neg]/max(v[ neg]) * 100, colors= 'red', random.color=F)  # brewer colors RdBu, Dark2
  wordcloud(nm[!neg], v[!neg]/max(v[!neg]) * 100, colors='blue', random.color=F)  
  # wordcloud(nm[-1], abs(v[-1])/max(abs(v[-1])) * 100, colors=brewer.pal(9,"RdBu"), random.color=F) 
  par(mfrow=c(1,1))
}

### Confusion matrix

confusion_summary <- function(truth, pred) { # assume inputs are logical
  sens   <- recall <- sum(pred & truth)/sum(truth)
  spec   <- sum(!pred & !truth)/sum(!truth)
  prec   <- sum(pred & truth)/ sum(pred)
  mclass <- sum(truth!=pred)/length(truth)
  z<-c(sens, spec, recall, prec, fm=2*prec*recall/(prec+recall), mclass)
  names(z) <- c("sens", "spec", "recall", "prec", "fm", "mclass")
  return(z)
}


### Spectral methods 

random_projection_svd <- function (X, d, power.iter=1, verbose=F) {  # matrix X, recover d dimensions
  n <- nrow(X); k <- ncol(X)
  omega <- matrix (rnorm (k*d), nrow=k, ncol=d)
  P <- X %*% omega
  if (power.iter >= 1) {
    if(verbose) cat("Starting power iterations...\n")
    for(i in 1:power.iter) {
      P <- X %*% (t(X) %*% P)
      if(verbose) cat("Completed power iteration...\n")
    }
  }
  Q <- qr.Q(qr(P))
  if (verbose) cat("Completed QR factorization...\n")
  udv <- svd(t(Q)%*%X)
  udv$u <- (Q %*% udv$u)
  return (udv)
}



plot_loadings <- function(v,j1,j2,threshold=0.1, cex=0.7) {	# labeled plots of loadings
  plot(v[,j1], v[,j2], col='gray',
       xlab=substitute("V"[j],list(j=j1)), 
       ylab=substitute("V"[j], list(j=j2)))
  label <- threshold < sqrt(v[,j1]^2 + v[,j2]^2)
  if(is.null(rownames(v))) cat("Add row labels to input matrix.\n")
  text(v[label,j1], v[label,j2], rownames(v)[label], cex=cex, srt=45)
}


### For simulating data with PCA patterns

make_hidden_pca_data <- function (n) {
  e  <- matrix(rnorm(n*25), nrow=n)
  u  <- rnorm(n)
  y  <- u^2 + rnorm(n)
  x  <- e + .35*u
  # plot(x[,3],y)
  # plot(u,y)
  return(list(x=x,y=y))
}

make_pca_data <- function(n, m, k=2, sd.signal=1) {
  e  <- matrix(rnorm(n*m), nrow=n)
  u  <- matrix(rnorm(n*k), nrow=n)
  vt <- matrix(rnorm(k*m), nrow=k)
  d  <- rnorm(k) * sd.signal
  return(e + u %*% (d * vt))
}

