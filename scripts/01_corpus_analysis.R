# 01_corpus_analysis
# ---------------------
# Analysis of corpus quality: Zipf's law, lexical diversity, growth curves

# --- libraries ---
library(tm)
library(stringr)
library(ggplot2)
library(zipfR)
library(koRpus)
library(readxl)
library(knitr)

# --- corpus ---
corpus_df <- read_excel("data/corpus_blogs.xlsx")  

########## (1) CORPUS ANALYSIS ####

#single text vector
corpus_text <- tolower(paste(corpus_df$text, collapse = " "))
corpus_text <- removePunctuation(corpus_text)
corpus_tokens <- unlist(strsplit(corpus_text, "\\s+"))

#### ZIPF'S LAW -------
word_freq <- table(corpus_tokens)
word_freq_sorted <- sort(word_freq, decreasing = TRUE)

zipf_data <- data.frame(
  rank = 1:length(word_freq_sorted),
  freq = as.numeric(word_freq_sorted),
  word = names(word_freq_sorted)
)
zipf_data$logFreq <- log2(zipf_data$freq)
zipf_data$logRank <- log2(zipf_data$rank)

# Plot Zipf's Law
plot(zipf_data$logRank, zipf_data$logFreq, 
     main = "Zipf's Law - Log Rank vs Log Frequency",
     xlab = "Log(Rank)", ylab = "Log(Frequency)", pch = 20, col = "darkblue")

# Linear regression
zipf_model <- lm(logFreq ~ logRank, data = zipf_data, weights = freq)
abline(zipf_model, col = "red", lwd = 2)
summary(zipf_model)
#98.9% of the variance in log frequency explained by log rank

#### --- LEXICAL DIVERSITY METRICS -----
N <- sum(word_freq)     # total tokens - 457118
V <- length(word_freq)  # total types - 18025

TTR <- V / N #0.03943183
G <- V / sqrt(N)
CTTR <- V / sqrt(2*N)
H <- log(V) / log(N)
M <- (log(N) - log(V)) / log2(N)

diversity_stats <- c(TTR = TTR, Guiraud = G, CTTR = CTTR, Herdan = H, Maas = M)
div_table <- enframe(diversity_stats, name = "Index", value = "Value")
div_table <- tibble(
  Index = c("TTR", "Guiraud", "CTTR", "Herdan", "Maas"),
  Value = c(0.0394, 26.66, 18.85, 0.752, 0.172),
  Comment = c(
    "Low TTR is expected in large corpora; confirms lexical richness.",
    "Guiraud corrects for size; value indicates a diverse vocabulary.",
    "CTTR stabilizes TTR across tokens; confirms robustness.",
    "Herdan's C > 0.7 indicates consistent vocabulary growth.",
    "Low Maas values reflect high lexical diversity."
  )
)
write.csv(div_table, "outputs/lexical_diversity_stats.csv", row.names = FALSE)


