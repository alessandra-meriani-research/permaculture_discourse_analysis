# 00_web_retrieval
# -----------------------------------
# Web Scraping of selected permaculture blogs
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(RSelenium)

########## (0) WEB DATA RETRIEVAL ########

#-Function for scraping
scrape_blog <- function(base_url, article_selector, paragraph_selector,
                        page_prefix = "", pages = 1, url_needs_prefix = FALSE,
                        prefix_url = "", max_articles = Inf) {
  
  all_texts <- c()
  article_count <- 0
  
  all_texts <- c()
  
  for (k in 1:pages) {
    if (article_count >= max_articles) break
    page_url <- paste0(base_url, page_prefix, k, "/")
    message("Scraping page: ", page_url)
    
    try({
      blog_page <- read_html(page_url)
      links <- blog_page %>% html_nodes(article_selector) %>% html_attr("href") %>% unique()
      links <- grep("https?://", links, value = TRUE)
      if (url_needs_prefix) {
        links <- paste0(prefix_url, links)
      }
      
      for (link in links) {
        if (article_count >= max_articles) break
        message(" - Article: ", link)
        try({
          article <- read_html(link)
          paragraphs <- article %>% html_nodes(paragraph_selector) %>% html_text2()
          clean_text <- paste(paragraphs, collapse = " ") %>% 
            str_replace_all('\"', '') %>% 
            str_replace_all("[[:digit:]]", "")
          all_texts <- c(all_texts, clean_text)
          article_count <- article_count + 1
        }, silent = TRUE)
      }
    }, silent = TRUE)
  }
  
  return(data.frame(text = all_texts))
}



#Blogs scraping  -----------------------------

# 1. Permaculture Apprentice Blog (manual entry needed - no pagination or index)
apprentice <- {
  url <- "https://permacultureapprentice.com/blog/"
  blog_page <- read_html(url)
  
  # Prendi i link che non contengono anchor, autore o servizio
  links <- blog_page %>% 
    html_nodes("article a") %>% 
    html_attr("href") %>%
    unique() %>%
    grep("^https://permacultureapprentice.com/[^#]*$", ., value = TRUE) %>%
    grep("author|respond|service", ., invert = TRUE, value = TRUE)
  
  vText <- c()
  for (link in links) {
    message(" - Article: ", link)
    try({
      page <- read_html(link)
      txt <- page %>%
        html_nodes("article p") %>%
        html_text2() %>%
        paste(collapse = " ") %>%
        str_replace_all('\"', '') %>%
        str_replace_all("[[:digit:]]", "")
      vText <- c(vText, txt)
    }, silent = TRUE)
  }
  data.frame(text = vText)
}


# 2. Jardin Feuilles de Vie – UK (good rvest compatibility)
jfdv <- scrape_blog(
  base_url = "https://jardinfeuillesdevie.com/blog-en/",
  article_selector = "h2.entry-title a",
  paragraph_selector = "div.entry-content p",
  pages = 5,
  max_articles = 60
)

# 3. Milkwood Blog
milkwood <- scrape_blog(
  base_url = "https://www.milkwood.net/blog/",
  article_selector = ".entry-title a",
  paragraph_selector = "article p",
  pages = 1,
  max_articles = 60
) 

# 4. Deep Green Permaculture
deep_green <- scrape_blog(
  base_url = "https://deepgreenpermaculture.com/blog/page/",
  article_selector = ".entry-title a",
  paragraph_selector = ".entry-content p",
  pages = 5,
  max_articles = 60
)

# 5. Permaculture Principles
p_principles <- scrape_blog(
  base_url = "https://permacultureprinciples.com/blog/",
  article_selector = ".entry-title a",
  paragraph_selector = ".entry-content p",
  pages = 5, 
  max_articles = 60
)

# 6. Permaculture Visions
tag_url <- "https://permaculturevisions.com/tag/permaculture-principles/"
page <- read_html(tag_url)
tag_text <- page %>%
  html_nodes("p") %>% 
  html_text2() %>%
  paste(collapse = " ") %>%
  str_replace_all('"', '') %>%
  str_replace_all("[[:digit:]]", "")
visions <- data.frame(text = tag_text)

# 7. Free Permaculture 
free_permaculture <- scrape_blog(
  base_url = "https://www.freepermaculture.com/blog/page/",
  article_selector = ".entry-title a",
  paragraph_selector = "article p",
  pages = 5,          
  max_articles = 60   
)

#8. Permaculture Magazine UK 
##read the html page + vector for text information
vText1 <- c()
for(k in 1:10){
  link1 <- paste("https://www.permaculture.co.uk/articles/",k,"/",sep="")
  blog1 <- read_html(link1)
  link_fulltext1<- blog1 %>% html_nodes(".IndexGrid a ") %>% html_attr("href") %>% unique()
  link_fulltext_cleaned1 <- grep("https", link_fulltext1, value = TRUE) # Extract elements with https only
  # vectors declaration
  for(i in 1:length(link_fulltext_cleaned1)){
    print(paste(k,i))
    # reads every single news item
    hhtmp1 <- read_html(link_fulltext_cleaned1[i])
    lstP1 <- hhtmp1 %>% html_nodes("body p")
    vText1 <- c(vText1,
                paste(lstP1 %>% html_text2(),collapse = " "))
  }
} 
perm_uk <- data.frame(text=vText1)


# 9. Permaculture Association
vText5 <- c()
base_url <- "https://www.permaculture.org.uk"
blog_url <- paste0(base_url, "/blog")
blog5 <- read_html(blog_url)
link_partial <- blog5 %>% html_nodes("article h2 a") %>% html_attr("href") %>% unique()
link_full <- paste0(base_url, link_partial)
for (i in seq_along(link_full)) {
  message(i)
  hhtmp <- tryCatch(read_html(link_full[i]), error = function(e) NULL)
  if (!is.null(hhtmp)) {
    paragraphs <- hhtmp %>% html_nodes("article p") %>% html_text2()
    if (length(paragraphs) > 0) {
      text_clean <- paste(paragraphs, collapse = " ") %>%
        str_replace_all('"', '') %>%
        str_replace_all("[[:digit:]]", "") %>%
        str_squish()
      vText5 <- c(vText5, text_clean)
    }
  }
}
p_association <- data.frame(text = vText5)
p_association$text <- noquote(p_association$text)

#10.Permaculture women's guild
#read the html page 
blog10 <- read_html("https://www.permaculturewomen.com/magazine/") 
# vector with the links to all full news in the first page 
link_fulltext10 <- blog10 %>% html_nodes(".ultp-block-title  a ") %>% html_attr("href") %>% unique() 
link_fulltext_cleaned10 <- grep("https", link_fulltext10, value = TRUE)
#vector for text information
vText10 <- c()
for(i in 1:length(link_fulltext_cleaned10)){
  print(i)
  hhtmp10 <- read_html(link_fulltext_cleaned10 [i])
  lstP10 <- hhtmp10 %>% html_nodes("article p")
  vText10 <- c(vText10,
               paste(lstP10[1:(length(lstP10)-1)] %>% html_text2()),collapse = " ")
} 
women_magazine<- data.frame(text = vText10, stringsAsFactors = FALSE)


########## CORPUS CREATION --------------------------------------------

#normalization
#function
normalize_blog_df <- function(df, blogname) {
  df$text <- as.character(df$text)
  df$blog <- blogname
  df <- df[, c("text", "blog")]
  return(df)
}

apprentice           <- normalize_blog_df(apprentice, "Permaculture Apprentice")
jfdv                 <- normalize_blog_df(jfdv, "Jardin Feuilles de Vie")
milkwood             <- normalize_blog_df(milkwood, "Milkwood")
deep_green           <- normalize_blog_df(deep_green, "Deep Green Permaculture")
p_principles         <- normalize_blog_df(p_principles, "Permaculture Principles")
visions              <- normalize_blog_df(visions, "Permaculture Visions")
free_permaculture    <- normalize_blog_df(free_permaculture, "Free Permaculture")
perm_uk              <- normalize_blog_df(perm_uk, "Permaculture Magazine UK")
p_association        <- normalize_blog_df(p_association, "Permaculture Association")
women_magazine       <- normalize_blog_df(women_magazine, "Permaculture Women's Guild")


corpus_df <- bind_rows(
  apprentice,
  jfdv,
  milkwood,
  deep_green,
  p_principles,
  visions,
  free_permaculture,
  perm_uk,
  p_association,
  women_magazine
)


#Save CSV
write_csv(corpus_df, file = "data/corpus_blogs.csv")

#saver RDS (only the dataset)
saveRDS(corpus_df, file = "data/corpus_blogs.rds")
#corpus_df <- readRDS("data/corpus_blogs.rds")

#Save RDdata (environment)
#save.image("data/environment_web_retrieval.RData")
###load("data/environment_web_retrieval.RData")




