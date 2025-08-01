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
library(quanteda)
library(igraph)
library(topicmodels)
library(tidytext)
library(scales)


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

### 1) Document-Level Word Association (using Pearson)
# Association with "permaculture" in the Term-Document Matrix

tdm <- readRDS("data/tdm.rds")
perm_corr <- findAssocs(tdm, "permaculture", 0.55)[[1]]
perm_corr_df <- data.frame(word = names(perm_corr), corr = unlist(perm_corr))
str(perm_corr)
# Plot
ggplot(perm_corr_df, aes(x = reorder(word, corr), y = corr)) +
  geom_point(aes(color = corr), size = 3.5) +
  scale_color_gradient(low = "#C8D6A4", high = "#355E3B") +
  coord_flip() +
  labs(title = "Document-Level Associations with 'Permaculture'",
       x = "Associated Lemmas", y = "Pearson Correlation") +
  theme_minimal(base_family = "Times New Roman")  +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

ggsave("outputs/doc_level_pearson_permaculture.png", width = 8, height = 6)


  
  
### 2) Sentence-Level Co-Occurrences and Network (using Log-Likelihood)

# Reshape corpus into sentences
corpus_sent <- corpus(corpus_df$text, docnames = corpus_df$blog_name)
sentences <- corpus_reshape(corpus_sent, to = "sentences")
sentences_clean <- tolower(removePunctuation(as.character(sentences)))
sentences_clean <- removeWords(sentences_clean, c(stopwords("en"), "blog", "online", "related"))
write.csv(sentences_clean, file = "sentences_clean", row.names = TRUE)

# Create binary DTM
dtm <- DocumentTermMatrix(Corpus(VectorSource(sentences_clean)))
dtm <- removeSparseTerms(dtm, 0.995)
mx.dtm <- as.matrix(dtm)
mx.dtm[mx.dtm > 0] <- 1

# Log-Likelihood Co-occurrence
source("functions/CoocStatisticsFunction.R")
coocs <- CoocStatisticsFunction("permaculture", mx.dtm, measure="LOGLIK")
top_coocs <- sort(coocs, decreasing = TRUE)[1:15]

# Build graph
graph_data <- data.frame(from = rep("permaculture", 15),
                         to = names(top_coocs),
                         sig = top_coocs)

graphNetwork <- graph_from_data_frame(graph_data, directed = FALSE)
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == "permaculture", "#6C8B3C", "#C8D6A4")
V(graphNetwork)$size <- scales::rescale(log(degree(graphNetwork) + 1), to = c(5, 15))
E(graphNetwork)$width <- scales::rescale(E(graphNetwork)$sig, to = c(1, 8))
E(graphNetwork)$color <- "gray50"

# Plot
plot(graphNetwork,
     layout = layout_with_fr,
     vertex.label.family = "Times New Roman", vertex.label.cex = 0.8,
     vertex.label.color = "black", edge.curved = 0.2)




### (4.2) Two-Word Level – Sentence-based Co-occurrence Network ###
# --- Load libraries ---
library(spacyr)
library(quanteda)
library(tm)
library(Matrix)
library(igraph)
library(dplyr)
library(tibble)
library(scales)

# Installa spaCy (e Python environment se serve)
spacy_install()

# --- Start spaCy (solo alla prima volta devi correre spacy_initialize() con modello scaricato) ---
spacy_initialize(model = "en_core_web_sm")

# --- Load and clean corpus ---
texts_all <- tolower(corpus_df$text_clean)

# --- Sentence segmentation ---
parsed <- spacy_parse(texts_all, dependency = FALSE, lemma = TRUE, pos = TRUE, tag = FALSE)
sentences_df <- parsed %>%
  group_by(doc_id, sentence_id) %>%
  summarise(sentence = paste(lemma[!pos %in% c("PUNCT", "SPACE")], collapse = " "), .groups = "drop")

# --- Create quanteda corpus and DTM ---
qcorpus <- corpus(sentences_df$sentence)
qcorpus_tokens <- tokens(qcorpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
qcorpus_tokens <- tokens_remove(qcorpus_tokens, pattern = stopwords("en"))
dtm <- dfm(qcorpus_tokens)
dtm <- dfm_trim(dtm, min_termfreq = 2) # filtro parole rare

# --- Weighted term-term co-occurrence matrix ---
tcmatrix <- t(as.matrix(dtm)) %*% as.matrix(dtm)  # weighted, not binary

# --- Calculate log-likelihood association scores ---
source("functions/CoocStatisticsFunction.R")  

coocTerm <- "permaculture"
numberOfCoocs <- 10
coocs <- CoocStatisticsFunction(coocTerm, as.matrix(dtm), measure = "LOGLIK")
coocs <- head(coocs[!is.na(coocs)], numberOfCoocs)

# --- Build network edges ---
resultGraph <- data.frame(
  from = rep(coocTerm, numberOfCoocs),
  to = names(coocs),
  sig = as.numeric(coocs)
)

# --- Optional: Expand network with second-order co-occurrences ---
for (i in names(coocs)) {
  coocs2 <- CoocStatisticsFunction(i, as.matrix(dtm), measure = "LOGLIK")
  coocs2 <- head(coocs2[!is.na(coocs2)], numberOfCoocs)
  tmpGraph <- data.frame(
    from = rep(i, numberOfCoocs),
    to = names(coocs2),
    sig = as.numeric(coocs2)
  )
  resultGraph <- rbind(resultGraph, tmpGraph)
}

# --- Create and plot the network ---
graphNetwork <- graph_from_data_frame(resultGraph, directed = FALSE)

# Node aesthetics
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, "#6C8B3C", "#BFD19A")
V(graphNetwork)$size <- rescale(log(degree(graphNetwork)), to = c(4, 14))

# Edge aesthetics
E(graphNetwork)$color <- adjustcolor("#4A6927", alpha.f = 0.4)
E(graphNetwork)$width <- rescale(E(graphNetwork)$sig, to = c(1, 6))

# Final plot
png("outputs/permaculture_network.png", width = 2400, height = 2400, res = 300)
set.seed(1515)
plot(graphNetwork,
     layout = layout_with_fr,
     vertex.label = V(graphNetwork)$name,
     vertex.label.family = "Times",
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.shape = "circle",
     vertex.frame.color = adjustcolor("darkgray", alpha.f = .3))
dev.off()

### (4.2) Sentence-based Co-occurrence Network – Enhanced Version ###

# --- Load libraries ---
library(spacyr)
library(quanteda)
library(tm)
library(Matrix)
library(igraph)
library(dplyr)
library(tibble)
library(scales)
library(tidygraph)
library(ggraph)
library(ggrepel)

# --- Start spaCy ---
spacy_initialize(model = "en_core_web_sm")

# --- Load and clean corpus ---
texts_all <- tolower(corpus_df$text_clean)

# --- Sentence segmentation and lemmatization ---
parsed <- spacy_parse(texts_all, dependency = FALSE, lemma = TRUE, pos = TRUE, tag = FALSE)
sentences_df <- parsed %>%
  group_by(doc_id, sentence_id) %>%
  summarise(sentence = paste(lemma[!pos %in% c("PUNCT", "SPACE")], collapse = " "), .groups = "drop")

# --- Create sentence-level DTM ---
qcorpus <- corpus(sentences_df$sentence)
tokens_sent <- tokens(qcorpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
tokens_sent <- tokens_remove(tokens_sent, pattern = stopwords("en"))
dtm <- dfm(tokens_sent)
dtm <- dfm_trim(dtm, min_termfreq = 2)

# --- Co-occurrence matrix (term-term) ---
dtm_matrix <- as.matrix(dtm)
tcmatrix <- t(dtm_matrix) %*% dtm_matrix

# --- Load function for association statistics ---
source("functions/CoocStatisticsFunction.R")

# --- First-order co-occurrences ---
coocTerm <- "permaculture"
numberOfCoocs <- 10
coocs <- CoocStatisticsFunction(coocTerm, dtm_matrix, measure = "LOGLIK")
coocs <- head(coocs[!is.na(coocs)], numberOfCoocs)

# --- Build edge list ---
resultGraph <- data.frame(
  from = rep(coocTerm, numberOfCoocs),
  to = names(coocs),
  sig = as.numeric(coocs)
)

# --- Expand with second-order co-occurrences ---
for (i in names(coocs)) {
  coocs2 <- CoocStatisticsFunction(i, dtm_matrix, measure = "LOGLIK")
  coocs2 <- head(coocs2[!is.na(coocs2)], numberOfCoocs)
  tmpGraph <- data.frame(
    from = rep(i, numberOfCoocs),
    to = names(coocs2),
    sig = as.numeric(coocs2)
  )
  resultGraph <- rbind(resultGraph, tmpGraph)
}

# --- Build graph ---
graphNetwork <- graph.data.frame(resultGraph, directed = FALSE)

# --- Add centrality metrics ---
V(graphNetwork)$degree <- degree(graphNetwork)
V(graphNetwork)$betweenness <- betweenness(graphNetwork)
V(graphNetwork)$eigen <- eigen_centrality(graphNetwork)$vector

# --- Community detection (Louvain) ---
coms <- cluster_louvain(graphNetwork)
V(graphNetwork)$community <- coms$membership

# --- Node aesthetics ---
V(graphNetwork)$color <- scales::hue_pal()(max(coms$membership))[V(graphNetwork)$community]
V(graphNetwork)$size <- rescale(V(graphNetwork)$betweenness + 1, to = c(5, 16))  # +1 to avoid log(0)

# --- Edge aesthetics ---
E(graphNetwork)$color <- adjustcolor("#4A6927", alpha.f = 0.4)
E(graphNetwork)$width <- rescale(E(graphNetwork)$sig, to = c(1, 5))

# --- Converti igraph in tidygraph ---
graph_tbl <- as_tbl_graph(graphNetwork)

# --- Aggiungi centralità e community se non l'hai già fatto ---
graph_tbl <- graph_tbl %>%
  mutate(
    degree = centrality_degree(),
    betweenness = centrality_betweenness(),
    eigen = centrality_eigen(),
    community = as.factor(group_louvain())
  )

# --- Palette ---
colors_natura <- c(
  "#6C8B3C", "#A7C957", "#D4A373", "#F4E285", "#BFD19A",
  "#C6AC8F", "#EDE6C2", "#9FAF90", "#F1C27B"
)

cluster_count <- length(unique(V(graphNetwork)$community))
V(graphNetwork)$color <- colors_natura[V(graphNetwork)$community %% length(colors_natura) + 1]


# --- Etichette leggibili ---
label_cex <- 1
label_font <- "Times New Roman"

# --- Final plot ---
layout_fr <- layout_with_fr(graphNetwork)

# 

png("outputs/permaculture_network.png", width = 2400, height = 2400, res = 300)
set.seed(1515)
plot(graphNetwork,
     layout = layout_with_fr,
     vertex.label = V(graphNetwork)$name,
     vertex.label.family = label_font,
     vertex.label.cex = label_cex,
     vertex.label.color = "black",
     vertex.shape = "circle",
     vertex.frame.color = "gray40")
dev.off()

# --- Optional: View metrics table for interpretation in paper ---
centrality_table <- tibble(
  word = V(graphNetwork)$name,
  degree = V(graphNetwork)$degree,
  betweenness = V(graphNetwork)$betweenness,
  eigenvector = round(V(graphNetwork)$eigen, 4),
  cluster = V(graphNetwork)$community
) %>% arrange(desc(betweenness))




######## 3.3 CONTEXT LEVEL <-

### 1) LDA (isotopie)
#Rivelare le isotopie discorsive latenti nel corpus dei blog di permacultura, andando oltre le co-occorrenze e mappando le principali “narrazioni tematiche”. 

tokens_clean <- tokens(
  tolower(corpus_df$text_clean),
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE
)
tokens_clean <- tokens_remove(tokens_clean, pattern = stopwords("en"))

# --- Crea DFM ---
dfm_clean <- dfm(tokens_clean)
library(quanteda)
library(topicmodels)
library(tidytext)

# --- Crea tokens puliti ---
tokens_clean <- tokens(
  tolower(corpus_df$text_clean),
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE
)
tokens_clean <- tokens_remove(tokens_clean, pattern = stopwords("en"))
bad_tokens <- c("s", "tag", "related", "c", "t", "m", "ll", "ve", "re", "d", "also")  # puoi aggiungerne altri
tokens_clean <- tokens_remove(tokens_clean, pattern = c(stopwords("en"), bad_tokens))

# --- Crea DFM ---
dfm_clean <- dfm(tokens_clean)

# --- Rimuovi termini rari e documenti vuoti ---
dfm_clean <- dfm_trim(dfm_clean, min_termfreq = 3)
dfm_clean <- dfm_clean[rowSums(dfm_clean) > 0, ]

# --- Converti in DTM per LDA ---
dtm <- quanteda::convert(dfm_clean, to = "topicmodels")


# --- Run LDA model (k = number of topics) ---
k <- 7  #aumentato da 6 
lda_model <- LDA(dtm, k = k, control = list(seed = 2025))

# --- Tidy results ---
lda_topics <- tidy(lda_model, matrix = "beta")

# --- Top terms tidy ---
top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

# --- Natural color palette ---
topic_colors <- c("#6C8B3C", "#A7C957", "#D4A373", "#F4E285", "#C6AC8F", "#9FAF90", "grey70")

theme_minimal(base_family = "Times New Roman") 
# --- Plot: 2 colonne, 3 righe, layout chiaro ---
ggplot(top_terms, aes(x = beta, y = term, fill = factor(topic))) +
  geom_col(show.legend = FALSE, width = 0.7) +
  facet_wrap(~ topic, scales = "free_y", ncol = 2) +
  scale_y_reordered() +
  scale_fill_manual(values = topic_colors) +
  labs(
    title = "Top Terms per Topic in Permaculture Blogs",
    x = "Probability (β)",
    y = NULL
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

ggsave("lda_topics_final_bse_ready.png", width = 8, height = 9, dpi = 300)


###heatmap
library(forcats)

# ---top terms per heatmap (long format) ---
top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10) %>%
  ungroup()

# order terms
top_terms <- top_terms %>%
  group_by(term) %>%
  mutate(avg_beta = mean(beta)) %>%
  ungroup() %>%
  arrange(desc(avg_beta)) %>%
  mutate(term = fct_reorder(term, avg_beta))

###plot heathmap

ggplot(top_terms, aes(x = factor(topic), y = term, fill = beta)) +
  geom_tile(color = "white", linewidth = 0.3) +
  # Gradient personalizzato oliva
  scale_fill_gradient(
    low = "#D8E8C0", high = "#3C4E14",
    name = expression(beta),
    guide = guide_colorbar(barwidth = 0.5, barheight = 10)
  ) +
  # Linee divisorie tra i topic
  geom_vline(xintercept = seq(1.5, 6.5, by = 1), color = "grey80", linewidth = 0.5) +
  # Etichette e tema elegante
  labs(
    title = "Latent Topics in Permaculture Blogs",
    subtitle = "Heatmap of top 10 terms per topic (LDA model, k = 6)",
    x = "Topic",
    y = NULL
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10, lineheight = 0.9),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank()
  )

ggsave("lda_heatmap_olive_bse_ready.png", width = 8, height = 11, dpi = 320)


##### SCELTA DI K
gamma <- tidy(lda_model, matrix = "gamma")
gamma %>% group_by(topic) %>% summarise(mean_gamma = mean(gamma)) %>% arrange(desc(mean_gamma))
#topic 6 troppo preponderante; topic 1 quasi rumore 


library(ldatuning)

result <- FindTopicsNumber(
  dtm,
  topics = 3:10,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "VEM",
  control = list(seed = 2025),
  mc.cores = 2,
  verbose = TRUE
)

FindTopicsNumber_plot(result)


###topics common terms 
overlap_terms <- tidy(lda_model, matrix = "beta") %>%
  filter(beta > 0.01) %>%
  count(term, sort = TRUE) %>%
  filter(n > 1)


library(topicmodels)
library(corrr)

gamma_wide <- gamma %>%
  pivot_wider(names_from = topic, values_from = gamma)

gamma_cor <- correlate(gamma_wide[,-1])  # rimuove doc_id




######## TEXT CLUSTERING #### 
# --- Librerie necessarie ---
library(tm)
library(proxy)
library(cluster)
library(factoextra)
library(dendextend)
library(tibble)

# --- Costruzione TDM e normalizzazione TF-IDF ---
# mx.tdm è la term-document matrix (TDM): termini = righe, documenti = colonne
tdm<-readRDS("data/tdm.rds")
tdm_sparse_07 <- removeSparseTerms(tdm, sparse = 0.70)
tdm_matrix <- as.matrix(tdm_sparse_07)
library(proxy)

# Distanza coseno tra righe (quindi: se stai clusterizzando parole, devi trasporre la matrice)
dist_matrix <- proxy::dist(tdm_matrix, method = "cosine")

# Clustering con Ward
hc_w <- hclust(dist_matrix, method = "ward.D2")

# Dendrogramma
par(mar=c(1,2,2,1))
plot(hc_w, hang = -1, sub = "", cex = 0.5,
     main = "Hierarchical Clustering (Cosine-Ward)", 
     cex.main = 0.9, xlab = "", cex.axis = 0.6, cex.lab = 0.2)


# --- Valutazione del numero ottimale di cluster ---
# Elbow method
fviz_nbclust(tdm_matrix, FUN = hcut, method = "wss", diss = dist_matrix) +
  labs(title = "Elbow Method") +
  geom_vline(xintercept = 4, linetype = 2) +
  theme_minimal(base_family = "Times New Roman")



# --- k = 4  ---
k <- 3
term_clusters <- cutree(hc_w, k = k)

# --- Riepilogo: numero di parole per cluster ---
table(term_clusters)

# Dendrogramma con rettangoli colorati
plot(hc_w, hang = -1, main = "Hierarchical Clustering (Cosine-Ward)", 
     xlab = "", sub = "", cex = 0.5)
rect.hclust(hc_w, k = k, border = c("#6C8B3C", "#BFD19A", "#D8E8C0"))  # verdi eleganti

# --- Estrai parole per cluster in un tibble ordinato ---
library(tibble)
terms_by_cluster <- lapply(1:k, function(i) {
  names(term_clusters[term_clusters == i])
})
names(terms_by_cluster) <- paste0("Cluster_", 1:k)

# Opzionale: tabella per salvataggio o ispezione
terms_by_cluster_df <- tibble::tibble(
  Cluster = rep(paste0("Cluster_", 1:k), times = sapply(terms_by_cluster, length)),
  Term = unlist(terms_by_cluster)
)

# Visualizza primi termini per ciascun cluster
lapply(terms_by_cluster, head, 10)

write.csv(terms_by_cluster_df, "clusters_terms_k3.csv", row.names = FALSE)


# --- Dendrogramma colorato per presentazione ---
fviz_dend(hc_terms, k = k, k_colors = c("#D8E8C0", "#A3C572", "#739A41", "#3C4E14", "#89BA27", "#6B8F2A", "#BFD19A"),
          type = "phylogenic", repel = TRUE,
          show_labels = TRUE, main = "Semantic Clustering of Terms",
          font.family = "Times New Roman")


####from sas
source("functions/Consensus_Clustering_Validation.R")
# Esegui 100 iterazioni su un campione dell’85% del TDM
consensus_matrix_k3 <- consensus_clustering(tdm_matrix, k = 3, reps = 100, sample_frac = 0.85)
#matrice di consenso -  i punti chiari indicano osservazioni spesso nello stesso cluster.
plot_consensus_matrix(consensus_matrix_k3) 
#Valuta stabilità per diversi valori di k (2–7)
consensus_scores <- evaluate_k_consensus(tdm_matrix, k_range = 2:7, reps = 100, sample_frac = 0.85)
#curva di consenso
plot_consensus_curve(consensus_scores)
#punto in cui la curva cresce molto fino a un certo k (es. 3 o 4) poi si appiattisce → quel punto è il tuo “k ottimale”.





###### (4.4) SENTIMENT ANALYSIS: PERMACULTURE BLOGS ######

# Caricamento delle funzioni
source("functions/EmotionFunction.R")
source("functions/PolarityFunction.R")
source("functions/SentimentFunction_syuzhet.R")
source("functions/SentimentFunction_udipipe.R")
source("create_matrix.R")

# -------- POLARITY: SUBJECTIVITY DICTIONARY --------
cl.Pol <- PolarityFunction(textColumns = freqLem_tdm$lemma, algorithm = "bayes", lexicon = "subjectivity_en.csv")
pol_lemmas <- data.frame(cl.Pol$words$word, cl.Pol$words$category)

# Plot della distribuzione della polarità
plot_sub <- cl.Pol$documents %>% 
  group_by(polarity = best_fit) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n/sum(n)*100) %>%
  ggplot(aes(x = polarity, y = perc, fill = polarity))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#2A3613", "grey80", "#ABC17E"))+
  theme_light() +
  theme(legend.position = "none") +
  labs(title = "Sentiment polarity distribution",
       subtitle = "Subjective polarity dictionary",
       x = "Polarity", y = "Relative frequency (%)") +
  theme(text = element_text(family = "Times New Roman"))

# Wordcloud comparativo
cl.Pol_words <- cl.Pol$words %>% group_by(category) %>% summarise(txtL = paste(word, collapse = " "))
corpPol <- Corpus(VectorSource(cl.Pol_words$txtL))
tdmPol <- TermDocumentMatrix(corpPol)
tdmPol <- as.matrix(tdmPol)
colnames(tdmPol) <- cl.Pol_words$category

comparison.cloud(tdmPol, max.words = 95, rot.per = 0.1, shape = "round",
                 colors = c("#5A7302", "#B6D93B"),
                 scale = c(1,1), match.colors = T)
text(0.5,1,"comparison cloud most important terms in polarity classification")

# -------- POLARITY: MULTI-DICTIONARY APPROACH --------
dic.HaIV <- loadDictionaryGI()
dic.LMFi <- loadDictionaryLM()
dic.QDAP <- loadDictionaryQDAP()

sent_SA <- analyzeSentiment(freqLem_tdm$lemma)
sent_SA_GI <- convertToDirection(sent_SA$SentimentGI)
gi <- as.data.frame(prop.table(table(sent_SA_GI))*100)

sent_SA_LM <- convertToDirection(sent_SA$SentimentLM)
lm <- as.data.frame(prop.table(table(sent_SA_LM))*100)

sent_SA_QDAP <- convertToDirection(sent_SA$SentimentQDAP)
qdap <- as.data.frame(prop.table(table(sent_SA_QDAP))*100)

# Plots per le 3 dizionari
plot_gi <- ggplot(data = gi, aes(x = sent_SA_GI, y = Freq, fill = sent_SA_GI)) +
  geom_bar(stat = "identity") + theme_light() +
  scale_fill_manual(values = c("#2A3613", "grey80", "#ABC17E")) +
  theme(legend.position = "none") +
  labs(title = "Sentiment polarity distribution",
       subtitle = "Harvard-IV polarity dictionary",
       x = "Polarity", y = "Relative frequency (%)") +
  theme(text = element_text(family = "Times New Roman"))

plot_lm <- ggplot(data = lm, aes(x = sent_SA_LM, y = Freq, fill = sent_SA_LM)) +
  geom_bar(stat = "identity") + theme_light() +
  scale_fill_manual(values = c("#2A3613", "grey80", "#ABC17E")) +
  theme(legend.position = "none") +
  labs(title = "Sentiment polarity distribution",
       subtitle = "Loughran-McDonald polarity dictionary",
       x = "Polarity", y = "Relative frequency (%)") +
  theme(text = element_text(family = "Times New Roman"))

plot_qdap <- ggplot(data = qdap, aes(x = sent_SA_QDAP, y = Freq, fill = sent_SA_QDAP)) +
  geom_bar(stat = "identity") + theme_light() +
  scale_fill_manual(values = c("#2A3613", "grey80", "#ABC17E")) +
  theme(legend.position = "none") +
  labs(title = "Sentiment polarity distribution",
       subtitle = expression(paste(italic("qdap"), " R package polarity dictionary")),
       x = "Polarity", y = "Relative frequency (%)") +
  theme(text = element_text(family = "Times New Roman"))

grid.arrange(plot_sub, plot_gi, plot_lm, plot_qdap, ncol = 4)

# -------- SENTICNET POLARITY HISTOGRAM --------
sent_SR <- sentiment(text.var = freqLem_tdm$lemma,
                     polarity_dt = lexicon::hash_sentiment_senticnet,
                     valence_shifters_dt = lexicon::hash_valence_shifters,
                     amplifier.weight = 0.8, n.before = 5, n.after = 2)

hist(sent_SR$sentiment, main = "Sentiment Histogram", 
     col = "grey90", xlab = "Polarity value", ylab = "Absolute frequency (n)",
     family = "Times New Roman")

# -------- EMOTION BAYESIAN ANALYSIS --------
cl.Emo <- EmotionFunction(textColumns = freqLem_tdm$lemma , algorithm = "bayes", lexicon = "emotions_en.csv")

plot_emo <- cl.Emo$documents %>% 
  filter(best_fit != "NC") %>% 
  group_by(emotion = best_fit) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n/sum(n)*100) %>%
  ggplot(aes(x = reorder(emotion,n), y = perc, fill = emotion)) +
  geom_bar(stat = "identity", width = 0.3) + coord_flip() +
  scale_fill_manual(values = c("#66822E", "#ABC17E", "#2A3613", "#89BA27")) +
  theme_light() + theme(legend.position = "none") +
  labs(title = "Emotions distribution",
       x = "Emotions", y = "Relative Frequency (%)") +
  theme(text = element_text(family = "Times New Roman"))

# -------- EMOTION SYUZHET ANALYSIS --------
sent_SYn <- get_nrc_sentiment(char_v = freqLem_tdm$lemma, language = "english")
syuz_sentEm <- sent_SYn[,1:8]
tabEmoSyuz <- data.frame(Emotion = names(colSums(syuz_sentEm)),
                         Freq = colSums(syuz_sentEm),
                         Perc = colSums(syuz_sentEm)/sum(syuz_sentEm)*100)

plot_emo_sy <- tabEmoSyuz %>%
  ggplot(aes(x = reorder(Emotion, Perc), y = Perc, fill = Emotion)) +
  geom_bar(stat = "identity", color = "gray50") +
  scale_fill_manual(values = brewer.pal(8, "BuGn")) +
  theme_light() + theme(legend.position = "none") + coord_flip() +
  labs(title = "Syuzhet NRC Sentiment Emotions",
       x = "Emotions", y = "Relative Frequency (%)") +
  theme(text = element_text(family = "Times New Roman"))



#####sentiment 
###vettore di parole
freq_lemmas <- read.csv("outputs/freqLem_tdm_normalized.csv",
                        sep = ";",
                        quote = "",
                        fileEncoding = "UTF-8",
                        stringsAsFactors = FALSE)
# Prepara i lemmi ripetuti in base alla frequenza
text_input <- rep(freq_lemmas$lemma, times = freq_lemmas$freq)

library(syuzhet)
syuz_sent <- get_nrc_sentiment(text_input)



library(sentimentr)
sent_SR <- sentiment(text.var = text_input,
                     polarity_dt = lexicon::hash_sentiment_senticnet,
                     valence_shifters_dt = lexicon::hash_valence_shifters)

# Polarity score con SenticNet
sent_sentic <- sentiment(text_input, polarity_dt = lexicon::hash_sentiment_senticnet)
mean_sentiment <- mean(sent_sentic$sentiment, na.rm = TRUE)

# Calcolo direzione (positivo, negativo, neutro)
sent_sentic <- sent_sentic %>%
  mutate(polarity = case_when(
    sentiment > 0 ~ "Positive",
    sentiment < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))

# Frequenze relative
sent_summary <- sent_sentic %>%
  count(polarity) %>%
  mutate(freq = n / sum(n) * 100)

# Grafico
ggplot(sent_summary, aes(x = reorder(polarity, -freq), y = freq, fill = polarity)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Positive" = "#D8E8C0", "Negative" = "#3C4E14", "Neutral" = "grey70")) +
  labs(title = "Sentiment Polarity - SenticNet",
       y = "Relative Frequency (%)", x = "Polarity") +
  theme_minimal(base_family = "Times New Roman")




###dizionari 
# NRC (emozioni)
sent_nrc <- get_nrc_sentiment(text_input)

# Aggrega in percentuali
nrc_df <- colSums(sent_nrc[,1:8])
nrc_df <- data.frame(emotion = names(nrc_df), perc = (nrc_df / sum(nrc_df)) * 100)

# Grafico emozioni
ggplot(nrc_df, aes(x = reorder(emotion, perc), y = perc, fill = emotion)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "BuGn") +
  theme_minimal(base_family = "Times New Roman") +
  labs(title = "NRC Emotion Distribution", x = "Emotion", y = "Relative Frequency (%)")




