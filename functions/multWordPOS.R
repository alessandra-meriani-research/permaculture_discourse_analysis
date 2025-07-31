multWordPOS <- function(x = NULL, model = "english-ewt", n = 100, save.xlsx = TRUE, xlsx.name = NULL){
  require(udpipe); require(dplyr); require(xlsx)
  if(is.null(x) | is.null(model)){stop("invalid argument")}
  if(class(x) != "character" | class(model) != "character"){stop("invalid argument")}
  out <- udpipe(x, object = model)
  AA2 <- bind_cols(tok=txt_nextgram(x=out$token, n = 2, sep = " "),
                   upo=txt_nextgram(x=out$upos, n = 2, sep = " ")) %>%
    filter(upo %in% c("NOUN NOUN","ADJ NOUN","PROPN PROPN")) %>%
    group_by(tok,upo) %>% summarise(nn=n(), .groups = 'drop') %>% arrange(-nn)
  AA3 <- bind_cols(tok=txt_nextgram(x=out$token, n = 3, sep = " "),
                   upo=txt_nextgram(x=out$upos, n = 3, sep = " ")) %>%
    filter(upo %in% c("NOUN NOUN NOUN","ADJ NOUN NOUN","ADJ ADJ NOUN","PROPN PROPN PROPN",
                      "NOUN ADP NOUN","PROPN ADP PROPN")) %>%
    group_by(tok,upo) %>% summarise(nn=n(), .groups = 'drop') %>% arrange(-nn)
  results <- bind_rows(AA2,AA3) %>% arrange(-nn) %>% slice_head(n=n)
  colnames(results) <- c("tokens","pos","freq")
  results$edit <- NA
  if(save.xlsx==TRUE){
    if(is.null(xlsx.name)==TRUE){xlsx.name <- "OutNgramPOS.xlsx"}
    write.xlsx(as.data.frame(results),file = xlsx.name,row.names = F,showNA = FALSE)
  }
  return(results)
}