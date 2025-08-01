SentimentFunction_syuzhet <- function (char_v, cl = NULL, language = "english", lexicon=NULL){
  require(syuzhet)
  if (!is.character(char_v)) 
    stop("Data must be a character vector.")
  if (!is.null(cl) && !inherits(cl, "cluster")) 
    stop("Invalid Cluster")
  if(is.null(lexicon)){
    lexicon <- dplyr::filter_(nrc, ~lang == language)
  }
  word_l <- strsplit(tolower(char_v), "[^A-Za-z']+")
  if (is.null(cl)) {
    nrc_data <- lapply(word_l, get_nrc_values, lexicon = lexicon)
  }
  else {
    nrc_data <- parallel::parLapply(cl = cl, word_l, lexicon = lexicon, 
                                    get_nrc_values)
  }
  result_df <- as.data.frame(do.call(rbind, nrc_data), stringsAsFactors = F)
  df <- data.frame(anger=numeric(),anticipation=numeric(),disgust=numeric(),fear=numeric(),joy=numeric(),
                   sadness=numeric(),surprise=numeric(),trust=numeric(),negative=numeric(),positive=numeric())
  # my_col_order <- c("anger", "anticipation", "disgust", 
  #                   "fear", "joy", "sadness", "surprise", 
  #                   "trust", "negative", "positive")
  # result_df[, my_col_order]
  df_result <- bind_rows(df,result_df)
  return(df_result)
}
