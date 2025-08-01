SentimentFunction_udipipe <- function(x = NULL,
                                      dict = NULL,
                                      negators = polarityShifter,
                                      amplifiers = intensifier,
                                      deamplifiers = weakener){
  
  if(is.null(x) | is.null(dict)){
    message("mancano informazioni")
    return()
  }
  if(length(names(dict)) != length(dict)){
    message("mancano i nomi dei dizionari")
    return()
  }
  esclus <- c(negators,amplifiers,deamplifiers)
  for(i in 1:length(dict)){
    print(names(dict)[i])
    tmp <- txt_sentiment(x = x[,c(1:3,5,6,7,8)],
                         term = "lemma",
                         polarity_terms = dict[[i]] %>% filter(!term %in% esclus),
                         polarity_negators = negators,
                         polarity_amplifiers = amplifiers,
                         polarity_deamplifiers = deamplifiers,
                         constrain = T)
    tmp$data$dizionario <- names(dict)[i]
    tmp$overall$dizionario <- names(dict)[i]
    if(i == 1){
      dfoutD = tmp$data
      dfoutO = tmp$overall
    } else {
      dfoutD = rbind(dfoutD,tmp$data)
      dfoutO = rbind(dfoutO,tmp$overall)
    }
  }
  dfoutO$classPol <- ifelse(dfoutO$sentiment_polarity<0,"Negative",
                            ifelse(dfoutO$sentiment_polarity>0,"Positive","Neutral"))
  dfoutO <- as_tibble(dfoutO)
  dfoutD <- as_tibble(dfoutD)
  dfMediaPol <- dfoutO %>% 
    group_by(doc_id) %>% 
    summarise(mediaSent=mean(sentiment_polarity)) %>% 
    mutate(doc_id=as.numeric(doc_id))
  dfDizioPol <- dfoutO %>% 
    select(doc_id,dizionario,sentiment_polarity) %>%
    spread(dizionario,sentiment_polarity) %>% 
    mutate(doc_id=as.numeric(doc_id))
  out <- list(data=dfoutD,overall=dfoutO,DizioPol=dfDizioPol,MediaPol=dfMediaPol)
  return(out)
}
