EmotionFunction <- function (textColumns, algorithm = "bayes", prior = 1, lexicon = NULL, verbose = FALSE,...) 
{
  #  print("creazione DocumentTermMatrix")
  #  cat("\n")
  require(svMisc)
  matrix <- create_matrix(textColumns, ...)
  if(is.null(lexicon)){
    lexicon <- read.csv("emotions_en.csv", header = FALSE)
  } else {
    lexicon <- read.csv(lexicon, header = FALSE)
  }
  ll <- table(lexicon[,2])
  lemo <- dimnames(ll)[[1]]
  counts <- list(length(which(lexicon[, 2] == lemo[1])), 
                 length(which(lexicon[, 2] == lemo[2])), 
                 length(which(lexicon[, 2] == lemo[3])), 
                 length(which(lexicon[, 2] == lemo[4])), 
                 length(which(lexicon[, 2] == lemo[5])), 
                 length(which(lexicon[, 2] == lemo[6])), 
                 nrow(lexicon))
  names(counts) <- c(lemo,"total")
  documents <- c()
  documenti <- data.frame(numeric(),numeric(),numeric(),numeric(),numeric(),numeric(),numeric(),character(),stringsAsFactors = F)
  colnames(documenti) <- c(lemo,"numParole","best_fit")
  parole <- data.frame(document=numeric(),word=character(),category=character(),score=numeric(),stringsAsFactors = F)
  for (i in 1:nrow(matrix)) {
    if(verbose == TRUE){
      progress(i)
    }
    scores <- list(0, 0, 0, 0, 0, 0)
    names(scores) <- lemo
    doc <- matrix[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    tro <- 0
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[, 2] == key),]
        index <- match(word, emotions[, 1], nomatch = 0)
        if (index > 0) {
          tro <- tro+1
          entry <- emotions[index, ]
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          score <- 1
          if (algorithm == "bayes") 
            score <- abs(log(score * prior/count))
          nr <- dim(parole)[1]+1
          parole[nr,1] <- i
          parole[nr,2] <- word
          parole[nr,3] <- category
          parole[nr,4] <- score
          
          scores[[category]] <- scores[[category]] + score
        }
      }
    }
    if (algorithm == "bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    }
    else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]] + 1e-06
      }
    }
    nscores <- unlist(scores)
    if(tro>0){
      mmx <- nscores[which.max(nscores)]
      if (algorithm == "bayes") {
        best_fit <- ifelse(abs((max(nscores[-which.max(nscores)])/mmx)-1)<0.001,"mix",names(scores)[which.max(unlist(scores))])
      } else {
        if(sum(nscores) < 0.0001){
          best_fit <- "NC" 
        } else {
          best_fit <- ifelse(abs((max(nscores[-which.max(nscores)])/mmx)-1)<0.001,"mix",names(scores)[which.max(unlist(scores))])
        }
      }
    } else {
      best_fit <- "NC"
    }
    documenti[i,1] <- scores[[1]]
    documenti[i,2] <- scores[[2]]
    documenti[i,3] <- scores[[3]]
    documenti[i,4] <- scores[[4]]
    documenti[i,5] <- scores[[5]]
    documenti[i,6] <- scores[[6]]
    documenti[i,7] <- tro
    documenti[i,8] <- best_fit
  }
  risult <- list(documents=documenti,words=parole)
  return(risult)
}
