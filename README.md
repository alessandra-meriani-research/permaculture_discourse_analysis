# 🌱 Permaculture Discourse Analysis  
*Text mining and sentiment analysis of permaculture blogs using R and open data*

Created by **Alessandra Meriani**  PhD Researcher · University of Bologna  

> Exploring the semantic and emotional landscape of European permaculture discourse  
> through lexical analysis, sentiment classification, and semiotic interpretation.  

---

## 📦 Folder Structure

| Folder/File                 | Description                                               |
|-----------------------------|-----------------------------------------------------------|
| `data/`                     | Sample corpus and metadata (anonymized)                   |
| `scripts/`                  | R scripts for text mining workflow                        |
| ├── `00_corpus_retrieval.R` | Web scraping and corpus construction                      |
| ├── `01_preprocessing.R`    | Text cleaning and lemmatization                           |
| ├── `02_lexical_analysis.R` | Frequencies, n-grams, and co-occurrence patterns          |
| ├── `03_sentiment.R`        | Emotion classification using NRC Lexicon                  |
| ├── `04_clustering.R`       | Semantic clusters and network-based analysis              |
| `outputs/`                  | Generated results and exports                             |
| ├── `figures/`              | Plots, wordclouds, and semantic visualizations            |
| └── `tables/`               | CSV tables of frequencies, emotions, and cluster metrics  |
| `README.md`                 | Project overview and usage instructions                   |
| `LICENSE`                   | MIT License for code reuse                                |

---

## Project Overview

This repository supports the empirical analysis conducted for a scientific article on  
**permaculture discourse and its role in sustainability transitions**.

Through text mining and sentiment analysis of grassroots blogs, the project investigates:
- 🌍 Dominant themes and framing in permaculture narratives  
- 💬 Emotional tone and affective patterns across the corpus  
- 🧩 Emergence of semantic clusters reflecting cultural values  

All scripts are written in R and follow a modular, transparent pipeline.

---

## 🛠️ Tools & Packages

This project was developed using:

- **R** (≥ 4.x)
- `tidytext`, `quanteda`, `textdata`, `syuzhet`
- `igraph`, `text2vec`, `ggraph`, `ggplot2`
- Lexical resource: **NRC Emotion Lexicon**

All scripts are executable individually or as a pipeline.

---

## 🚀 How to Run

Open and run the scripts in the `scripts/` folder:

1. `00_corpus_retrieval.R` – Web scraping and corpus building  
2. `01_preprocessing.R` – Cleaning and lemmatization  
3. `02_lexical_analysis.R` – Frequency analysis and n-grams  
4. `03_sentiment.R` – Emotional orientation with NRC  
5. `04_clustering.R` – Semantic clusters and interpretative framing

Output files (figures and tables) will be saved in `/outputs/`.

---

## 📂 Data

The `data/` folder includes:

- `corpus_sample.csv` — a reduced and anonymized portion of the blog corpus  
- `metadata.csv` *(optional)* — blog titles, URLs, authors (if public), dates, language  

Due to copyright and ethical considerations, the full dataset is not included.  
It may be shared upon request for academic use only.

---

## 📜 License

All code in this repository is released under the **MIT License**.  
You are free to use, modify, and distribute it with attribution.

---

## ✍️ Credits & Citation

Created by **Alessandra Meriani**  
PhD Researcher · Public Governance, Management and Policy 
*University of Bologna*  
Department of Statistical Sciences - Department of Agricultural and Food Sciences

> Suggested citation:  
> Meriani, A. (2025). *Permaculture Discourse Analysis: Text Mining of Sustainability Narratives*. GitHub. https://github.com/alessandra_meriani/permaculture-discourse-analysis

---


                                               *Made with R & radical hope 💚*




