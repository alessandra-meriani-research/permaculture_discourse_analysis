# 03 text analysis_ 

#---libraries
library(tm)
library(udpipe)
library(dplyr)
library(ggplot2)
library(readxl)
library(wordcloud)
library(readr)
library(tibble)
library(tidyr)
library(stringr)
library(knitr)
library(openxlsx)
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(patchwork)



#----- TDM -------
tdm_matrix<-readRDS("data/tdm_matrix.rds")

#--- corpus
corpus_df <- read_excel("data/corpus_blogs.xlsx") 
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
corpus_df$text_clean <- cleanText(corpus_df$text)


######## 3.1 WORD-LEVEL ##########
# Word-level analysis of the permaculture blog corpus
# Includes frequency extraction, POS tagging, lemmatization, visualization, and n-gram analysis



# Compute word frequency from TDM
term_freq <- data.frame(
  words = rownames(tdm_matrix),
  freq = rowSums(tdm_matrix),
  stringsAsFactors = FALSE
)
term_freq$words <- iconv(term_freq$words, to = "UTF-8")

# POS tagging & Lemmatization on frequent words
ud_model <- udpipe_load_model("functions/english-ewt-ud-2.5-191206.udpipe")
fw_pos <- udpipe(x = term_freq$words, object = ud_model)
fw_pos <- fw_pos %>% select(token, lemma, upos)

# Merge frequency with POS-tagged lemmas
freq_lemmas <- merge(fw_pos, term_freq, by.x = "lemma", by.y = "words") %>% 
  select(lemma, freq) %>% 
  distinct(lemma, .keep_all = TRUE)

# Normalize frequency (per 10,000)
tot_tokens <- sum(freq_lemmas$freq, na.rm = TRUE)
freq_lemmas <- freq_lemmas %>% 
  mutate(
    rfreq = freq / tot_tokens,
    nrfreq = rfreq * 10000
  )

# Export lemma-level frequency table
write.csv(freq_lemmas, file = "outputs/freqLem_tdm_normalized.csv", row.names = FALSE)


###---permaculture term 
# Check frequency of 'permaculture' before exclusion
perma_freq <- freq_lemmas %>% filter(lemma == "permaculture") %>% pull(freq)
total_tokens <- sum(freq_lemmas$freq, na.rm = TRUE) + perma_freq
perma_pct <- perma_freq / total_tokens * 100
cat("Relative frequency (%):", round(perma_pct, 2), "\n")

#in how many documents the term "permaculture" appears
if ("permaculture" %in% rownames(tdm_matrix)) {
  perma_doc_count <- sum(tdm_matrix["permaculture", ] > 0)
  total_docs <- ncol(tdm_matrix)
  perma_doc_pct <- round((perma_doc_count / total_docs) * 100, 2)
  
  message("The term 'permaculture' appears in ", perma_doc_count, " out of ", total_docs, 
          " documents (", perma_doc_pct, "%).")
} else {
  warning("The term 'permaculture' is not found in the TDM.")
}
### The term 'permaculture' appears in 239 out of 494 documents (48.38%).

# Remove the lemma "permaculture" for visualization purposes
freq_lemmas <- freq_lemmas %>% filter(lemma != "permaculture")
freq_lemmas <- freq_lemmas %>% filter(lemma != "also")

# Wordcloud of top lemmas
set.seed(1515)
wordcloud(words = freq_lemmas$lemma,
          freq = freq_lemmas$freq,
          scale = c(4, 0.6),
          max.words = 300,
          min.freq = 3,
          random.order = FALSE,
          rot.per = 0.1,
          colors = c("#8FD047", "#498505", "#4A6927"),
          family = "Times New Roman")

# Barplot: top 20 frequent lemmas
plot_LemFreq <- freq_lemmas %>% 
  arrange(desc(nrfreq)) %>% 
  slice(1:20) %>%
  ggplot(aes(x = reorder(lemma, nrfreq), y = nrfreq)) +
  geom_bar(stat = "identity", fill = "#92AC59") +
  coord_flip() +
  labs(title = "Top 20 Lemmas by Normalized Frequency",
       x = "Lemma", y = "Frequency per 10,000") +
  theme_minimal(base_family = "Times New Roman")

#ggsave("outputs/top20_lemmas_barplot.png", plot = plot_LemFreq, width = 8, height = 5)

### --- N-GRAM ANALYSIS ---
#source("functions/multWordPOS.R")
#multWordPOS(x = corpus_df$text_clean, model = "english", n = Inf, save.xlsx = TRUE)
ngrams<-read_xlsx("data/OutNgramPOS.xlsx")
ngrams_plot <- ngrams %>%
  arrange(desc(freq)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = reorder(tokens, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#92AC59") +
  coord_flip() +
  labs(title = "Top 20 Frequent N-grams",
       x = "N-gram", y = "Frequency") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 11),  # <- più grande
    panel.grid.major.y = element_blank()    # <- opzionale per più pulizia
  )

ggsave("outputs/top20_ngrams_barplot.png", plot = ngrams_plot, width = 8, height = 5)

##### ----- COMBINED PLOT - graph patchwork 
combined_plot <- plot_LemFreq + ngrams_plot + 
  plot_annotation(
    title = "Most Frequent Lexical Units in the Corpus",
    theme = theme(
      plot.title = element_text(family = "Times New Roman", size = 14, face = "bold")
    )
  )

#ggsave("outputs/combined_wordlevel.png", plot = combined_plot, width = 12, height = 6, dpi = 300)


# --- BUBBLE PLOT: Word salience and dispersion ---
# Load normalized frequencies
freq_lemmas <- read.csv("outputs/freqLem_tdm_normalized.csv",
                        sep = ";",
                        quote = "",
                        fileEncoding = "UTF-8",
                        stringsAsFactors = FALSE)
#document frequency for each lemma
doc_freqs <- rowSums(tdm_matrix > 0)
doc_freq_table <- data.frame(
  lemma = names(doc_freqs),
  doc_freq = as.numeric(doc_freqs),
  stringsAsFactors = FALSE
)

# Merge with normalized frequency data
bubble_data <- merge(freq_lemmas, doc_freq_table, by = "lemma")
bubble_data <- bubble_data %>%
  mutate(doc_pct = doc_freq / ncol(tdm_matrix) * 100)

# Filter most relevant terms if needed
top_bubble <- bubble_data %>%
  arrange(desc(nrfreq)) %>%
  slice(1:30)  # or use other filter

top_bubble$label <- ifelse(rank(-top_bubble$freq) <= 30, top_bubble$lemma, "")

library(ggrepel)

ggplot(top_bubble, aes(x = nrfreq, y = doc_pct, size = freq, fill = doc_pct)) +
  geom_point(shape = 21, color = "#3C4E14", stroke = 0.4, alpha = 0.9) +
  geom_text_repel(
    aes(label = label),
    size = 4.5,             # Font più grande
    box.padding = 0.8,    # Distanza dal centro della bolla
    point.padding = 0.6,
    segment.size = 0.3,
    family = "Times New Roman",
    min.segment.length = 0,
    force = 1,
    max.overlaps = Inf
  ) +
  scale_fill_gradient(low = "#D8E8C0", high = "#3C4E14", name = "Doc. Frequency (%)") +
  scale_size_continuous(range = c(3.5, 12), guide = guide_legend(title = "Absolute Frequency")) +
  scale_x_continuous(name = "Normalized Frequency (per 10,000)", labels = scales::comma) +
  scale_y_continuous(name = "Document Occurrence (%)", labels = scales::comma) +
  ggtitle("Lexical Prominence and Dispersion of Key Lemmas") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )


# Save
ggsave("outputs/bubble_plot_wordlevel.png", width = 8, height = 6)




######## 3.2 TWO-WORDS LEVEL <- 