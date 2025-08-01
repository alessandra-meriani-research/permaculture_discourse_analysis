
CoocStatisticsFunction <- function(coocTerm, mx.dtm, measure = "DICE") {
  
  # Ensure Matrix (SparseM} or matrix {base} format
  require(Matrix)
  
  # Ensure binary DTM
  if (any(mx.dtm > 1)) {
    mx.dtm[mx.dtm > 1] <- 1
  }
  
  # calculate cooccurrence counts
  coocCounts <- t(mx.dtm) %*% mx.dtm
  
  # retrieve numbers for statistic calculation
  k <- nrow(mx.dtm)
  ki <- sum(mx.dtm[, coocTerm])
  kj <- colSums(mx.dtm)
  names(kj) <- colnames(mx.dtm)
  kij <- coocCounts[coocTerm, ]
  
  # calculate statistics
  switch(measure, 
         DICE = {
           dicesig <- 2 * kij / (ki + kj)
           dicesig <- dicesig[order(dicesig, decreasing=TRUE)]
           sig <- dicesig
         },
         LOGLIK = {
           logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
                          + (k - ki - kj + kij) * log(k - ki - kj + kij) 
                          + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
                          - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
           logsig <- logsig[order(logsig, decreasing=T)]
           sig <- logsig    
         },
         MI = {
           mutualInformationSig <- log(k * kij / (ki * kj))
           mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]
           sig <- mutualInformationSig    
         },
         {
           sig <- sort(kij, decreasing = TRUE)
         }
  )
  sig <- sig[-match(coocTerm, names(sig))]
  return(sig)
}
