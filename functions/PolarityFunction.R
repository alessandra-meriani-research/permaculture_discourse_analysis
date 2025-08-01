PolarityFunction <- function (textColumns, algorithm = "bayes", pstrong = 0.5, pweak = 1, 
                              prior = 1,  lexicon = NULL, verbose = FALSE, ...)   
{
  require(svMisc)
  matrix <- create_matrix(textColumns, ...)
  if(is.null(lexicon)){
    lexicon <- read.csv("subjectivity_en.csv", header = FALSE,stringsAsFactors = T)
  } else {
    lexicon <- read.csv(lexicon, header = FALSE,stringsAsFactors = T)
  }
  lexicon <- lexicon[lexicon$V3 %in% c("negativo","positivo","negative","positive"),]
  lexicon$V3 <- droplevels(lexicon$V3)
  ll <- table(lexicon[,3])
  lemo <- dimnames(ll)[[1]]
  lemo <- c(lemo[which(regexpr("pos",lemo)==1)], lemo[which(regexpr("neg",lemo)==1)])
  counts <- list(length(which(lexicon[, 3] == lemo[1])), 
                 length(which(lexicon[, 3] == lemo[2])), 
                 nrow(lexicon))
  names(counts) <- c(lemo,"total")
  documenti <- data.frame(POS=numeric(),NEG=numeric(),Ratio=numeric(),best_fit=character(),stringsAsFactors = F)
  esteso <- data.frame(document=numeric(),word=character(),category=character(),polarity=character(),score=numeric(),stringsAsFactors = F)
  for (i in 1:nrow(matrix)) {
    if(verbose == TRUE){
      progress(i)
    }
    scores <- list(0, 0)
    names(scores) <- lemo
    doc <- matrix[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    tro <- 0
    for (word in words) {
      index <- match(word, lexicon[, 1], nomatch = 0)
      if (index > 0) {
        tro <- 1
        entry <- lexicon[index, ]
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        if (algorithm == "bayes"){ 
          score <- pweak
          if (polarity == "strongsubj"){ 
            score <- pstrong}
          score <- abs(log(score * prior/count))
        } else {
          score <- 1
        }
        
        nr <- dim(esteso)[1]+1
        esteso[nr,1] <- i
        esteso[nr,2] <- word
        esteso[nr,3] <- category
        esteso[nr,4] <- polarity
        esteso[nr,5] <- score
        scores[[category]] <- scores[[category]] + score
      }
    }
    if (algorithm == "bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]] + 1e-06
      }
    }
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- round(abs(scores[[1]]/scores[[2]]),1)
    if(tro == 0) ratio <- 1
    if (ratio == 1) 
      best_fit <- "neutral"
    documenti[i,1] <- scores[[1]]
    documenti[i,2] <- scores[[2]]
    documenti[i,3] <- ratio # abs(scores[[1]]/scores[[2]])
    documenti[i,4] <- best_fit
  }
  risult <- list(documents=documenti,words=esteso)
  return(risult)
}
