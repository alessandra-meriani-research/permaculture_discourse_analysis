# 02_text_preprocessing.R
# ------------------------------------------
#Text cleaning, lemmatization and structuring of the permaculture corpus

# --- libraries ---
library(dplyr)
library(stringr)
library(tm)
library(udpipe)
library(tibble)
library(readxl)

# --- corpus ---
corpus_df <- read_excel("data/corpus_blogs.xlsx")

########## (2) TEXT PREPROCESSING ##########

##### (2.1) CLEANING #####

# General text cleaning function
cleanText <- function(xtxt = NULL, hashtag = TRUE, mention = TRUE, numbers = TRUE,
                      punctuation = TRUE, lowercase = TRUE){
  
  if (is.null(xtxt) || !is.character(xtxt)) stop("Input must be a character vector.")
  
  htmlsymbols <- c("&copy;", "&reg;", "&trade;", "&ldquo;", "&lsquo;", "&rsquo;", "&bull;", "&middot;", "&sdot;",
                   "&ndash;", "&mdash;", "&cent;", "&pound;", "&euro;", "&ne;", "&frac12;", "&frac14;", "&frac34;",
                   "&deg;", "&larr;", "&rarr;", "&hellip;", "&nbsp;", "&lt;", "&gt;", "&amp;", "&quot;")
  
  xtxt <- gsub("(f|ht)(tp)(s?)(://)(\\S+)", " ", xtxt)                    # Remove URLs
  xtxt <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", xtxt)                  # Remove retweet mentions
  xtxt <- gsub(paste(htmlsymbols, collapse = "|"), " ", xtxt)              # Remove HTML symbols
  if (punctuation) xtxt <- gsub("[[:punct:]]", " ", xtxt)                  # Remove punctuation
  xtxt <- gsub('[[:cntrl:]]|[^[:graph:]]', ' ', xtxt)                      # Remove control chars
  if (hashtag) xtxt <- gsub("#\\S+", " ", xtxt)                            # Remove hashtags
  if (mention) xtxt <- gsub("@\\S+", " ", xtxt)                            # Remove mentions
  if (numbers) xtxt <- gsub("[[:digit:]]", "", xtxt)                       # Remove digits
  xtxt <- gsub("\\s+", " ", xtxt)                                          # Normalize whitespace
  xtxt <- trimws(xtxt)                                                     # Trim leading/trailing space
  if (lowercase) xtxt <- tolower(xtxt)                                     # Lowercase
  
  return(xtxt)
}

# Apply cleaning function
corpus_df$text_clean <- cleanText(corpus_df$text)

##### (2.2) NORMALIZATION: Tokenization + Lemmatization + POS #####

# Ensure encoding
corpus_df$text_utf <- iconv(corpus_df$text_clean, to = "UTF-8")

# Load UDPIPE model
udpipe_download_model(language = "english-ewt", model_dir = "./functions/")
ud_model <- udpipe_load_model(file = "./functions/english-ewt-ud-2.5-191206.udpipe")

# Annotate - 463130 obs
#corpus_tag <- udpipe(x = corpus_df$text_utf, object = ud_model, doc_id = 1:nrow(corpus_df))
corpus_tag <- corpus_tag %>% select(doc_id, token, lemma, upos)

# Identify stopwords
corpus_tag$stopwords <- tolower(corpus_tag$lemma) %in% stopwords("en") |
  tolower(corpus_tag$token) %in% stopwords("en")

# Filter out stopwords and undesired POS - 250251
corpus_tag_filtered <- corpus_tag %>%
  filter(!stopwords & !upos %in% c("PUNCT", "X", "PART", "SYM", "DET", "CCONJ", "ADP", "PRON", "SCONJ", "PROPN")) %>%
  filter(nchar(token) > 2)


##### (2.3) STRUCTURING: TERM-DOCUMENT MATRIX #####
#doc for each article putting together - 494 doc (articles) 
corpus_tag_filtered <- corpus_tag %>%
  filter(!stopwords & !upos %in% c("PUNCT", "X", "PART", "SYM", "DET", "CCONJ", "ADP", "PRON", "SCONJ", "PROPN")) %>%
  filter(nchar(token) > 2) %>%
  mutate(doc_id = as.factor(doc_id)) %>%
  group_by(doc_id) %>%
  summarise(text = paste(lemma, collapse = " ")) # text based on lemmatized tokens

#corpus with articles - bag of words
corpus_bow <- Corpus(VectorSource(corpus_tag_filtered$text))

# Generate Term-Document Matrix
tdm <- TermDocumentMatrix(corpus_bow, control = list(wordLengths = c(3, Inf)))

# Filter sparse terms (retain those present in >=5% of docs)
tdm_sparse <- removeSparseTerms(tdm, sparse = 0.95)
tdm_matrix <- as.matrix(tdm_sparse)

#export
write.csv(tdm_matrix, file = "data/tdm_matrix.csv", row.names = TRUE)

saveRDS(tdm_matrix, file = "data/tdm_matrix.rds")
