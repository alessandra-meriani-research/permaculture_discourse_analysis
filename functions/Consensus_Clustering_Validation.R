# Consensus Clustering Validation in R
# Ispirato al codice SAS fornito

# Librerie necessarie
library(cluster)
library(factoextra)
library(reshape2)
library(ggplot2)
library(tibble)
library(dplyr)
library(proxy)

# Supponiamo di avere: tdm_matrix (term-document matrix, parole x documenti)
# Funzione per eseguire clustering ripetuto e costruire matrice di consenso
consensus_clustering <- function(data, k = 3, reps = 100, sample_frac = 0.85) {
  n <- nrow(data)
  consensus <- matrix(0, n, n)
  rownames(consensus) <- colnames(consensus) <- rownames(data)
  
  for (i in 1:reps) {
    set.seed(i)
    idx <- sample(1:n, size = floor(n * sample_frac))
    sub_data <- data[idx, ]
    d <- proxy::dist(sub_data, method = "cosine")
    hc <- hclust(d, method = "ward.D2")
    clusters <- cutree(hc, k = k)
    
    for (j in 1:(length(idx)-1)) {
      for (l in (j+1):length(idx)) {
        if (clusters[j] == clusters[l]) {
          consensus[idx[j], idx[l]] <- consensus[idx[j], idx[l]] + 1
          consensus[idx[l], idx[j]] <- consensus[idx[l], idx[j]] + 1
        }
      }
    }
  }
  
  consensus <- consensus / reps
  return(consensus)
}

# Visualizzazione heatmap matrice di consenso
plot_consensus_matrix <- function(consensus_matrix) {
  melted <- melt(consensus_matrix)
  ggplot(melted, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "#D8E8C0", high = "#3C4E14", midpoint = 0.5) +
    theme_minimal() +
    labs(title = "Consensus Matrix", x = NULL, y = NULL, fill = "Proportion") +
    theme(axis.text = element_blank(), axis.ticks = element_blank())
}

# Consensus clustering per diversi valori di k
evaluate_k_consensus <- function(data, k_range = 2:6, reps = 100, sample_frac = 0.85) {
  res <- tibble()
  for (k in k_range) {
    consensus <- consensus_clustering(data, k = k, reps = reps, sample_frac = sample_frac)
    avg_cons <- mean(consensus[lower.tri(consensus)])
    res <- bind_rows(res, tibble(k = k, consensus = avg_cons))
  }
  return(res)
}

# Plot curva di consenso
plot_consensus_curve <- function(consensus_df) {
  ggplot(consensus_df, aes(x = k, y = consensus)) +
    geom_line(color = "#3C4E14", size = 1.2) +
    geom_point(color = "#6C8B3C", size = 2) +
    theme_minimal() +
    labs(title = "Consensus Clustering Curve", x = "Number of Clusters (k)", y = "Average Consensus") +
    theme(text = element_text(family = "Times New Roman"))
}

# Esempio d’uso:
# consensus <- consensus_clustering(tdm_matrix, k = 3, reps = 100)
# plot_consensus_matrix(consensus)
# consensus_scores <- evaluate_k_consensus(tdm_matrix, 2:7, reps = 100)
# plot_consensus_curve(consensus_scores)
