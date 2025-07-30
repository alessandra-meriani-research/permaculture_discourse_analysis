setwd("/Users/studiomeriani/Desktop/tesi_r")

#### libraries ####
library(rvest) #scraping
library(dplyr)
library(ggplot2)
library(udpipe)
library(stopwords)
library(doc2concrete)
library(utility)
library(tidyr)
library(tidytext)
library(corpus)
library(quanteda)
library(tm)
library(Rcpp)
library(devtools)
library(tidyverse)
library(wordcloud)
library(readxl)
library(gganimate)
library(corpus)
library(TextWiller)
library(ngram)
library(cld2)
library(ggraph) 
library(igraph)
library(languageR)
library(zipfR)
library(xlsx)
require(igraph)
library(textmineR)
library(lda)
library(ldatuning)
library(topicmodels)
library(proxy)
library(factoextra)
library(NbClust)
library(pals)
library(sentimentr)
library(SentimentAnalysis)
library(syuzhet)

#### environments ####
load("/Users/studiomeriani/Desktop/tesi_r/scraping.RData") #scraping
load("/Users/studiomeriani/Desktop/tesi_r/zipf.RData") #zipf's law, vocabulary
load("/Users/studiomeriani/Desktop/tesi_r/tok_lem.RData") #cleaning+lem+tok
load("/Users/studiomeriani/Desktop/tesi_r/tok_lem.RData") #noStopWords
load("/Users/studiomeriani/Desktop/tesi_r/treetag.RData") #treetag
load("/Users/studiomeriani/Desktop/tesi_r/tdm.RData") #tdm
load("/Users/studiomeriani/Desktop/tesi_r/cooc.RData") #cooc
load("/Users/studiomeriani/Desktop/tesi_r/topicmod.RData") #topic modeling 
load("/Users/studiomeriani/Desktop/tesi_r/sensitive.RData") #sentiment analysis

########## (1) BIG DATA RETRIEVAL ########

# 1) permaculture.uk
#read the html page + vector for text information
vText1 <- c()
for(k in 1:10){
  link1 <- paste("https://www.permaculture.co.uk/articles/category/articles/page/",k,"/",sep="")
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
#print vector of text without quotes and pages numbers
perm_uk<-noquote(perm_uk)
perm_uk<-gsub('["]', '', perm_uk)
perm_uk<-gsub("[[:digit:]]", "", perm_uk) 



# 2) permaculture research institute
#read the html page 
vText2 <- c()
for(k in 1:2){
  link2 <- paste("https://www.permaculturenews.org/permaculture-news/page/",k,"/",sep="")
  blog2 <- read_html(link2)
  link_fulltext2<- blog2 %>% html_nodes(".post-title  a ") %>% html_attr("href") %>% unique()
  link_fulltext_cleaned2 <- grep("https", link_fulltext2, value = TRUE) # Extract elements with https only
  # vectors declaration
  for(i in 1:length(link_fulltext_cleaned2)){
    print(paste(k,i))
    # reads every single news item
    hhtmp2 <- read_html(link_fulltext_cleaned2[i])
    lstP2 <- hhtmp2 %>% html_nodes(".entry-content p")
    vText2 <- c(vText2,
                paste(lstP2 %>% html_text2(),collapse = " "))
  }
} 
pri <- data.frame(text=vText2)
#print vector of text without quotes and pages numbers
pri<-noquote(pri)
pri<-gsub('["]', '', pri)
pri<-gsub("[[:digit:]]", "", pri) 


# 3) Milkwood
#read the html page 
blog3 <- read_html("https://www.milkwood.net/blog/") 
# vector with the links to all full news in the first page 
link_fulltext3 <- blog3 %>% html_nodes(".entry-title a ") %>% html_attr("href") %>% unique() 
link_fulltext_cleaned3 <- grep("https", link_fulltext3, value = TRUE)      # Extract elements with https only
#vector for text information
vText3 <- c()
for(i in 1:25){
  print(i)
  hhtmp3 <- read_html(link_fulltext_cleaned3 [i])
  lstP3 <- hhtmp3 %>% html_nodes("article p")
  vText3 <- c(vText3,
              paste(lstP3[1:(length(lstP3)-1)] %>% html_text2()),collapse = " ")
}
milkwood<- data.frame(text=vText3)
#print vector of text without quotes and pages numbers
milkwood<-noquote(milkwood)
milkwood<-gsub('["]', '', milkwood)
milkwood<-gsub("[[:digit:]]", "", milkwood)


# 4) permaculture principles
#read the html page 
vText4 <- c()
for(k in 1:4){
  link4 <- paste("https://permacultureprinciples.com/blog/page/",k,"/",sep="")
  blog4 <- read_html(link4)
  link_fulltext4<- blog4 %>% html_nodes(".entry-title  a ") %>% html_attr("href") %>% unique()
  link_fulltext_cleaned4 <- grep("https", link_fulltext4, value = TRUE) # Extract elements with https only
  # vectors declaration
  for(i in 1:length(link_fulltext_cleaned4)){
    print(paste(k,i))
    # reads every single news item
    hhtmp4 <- read_html(link_fulltext_cleaned4[i])
    lstP4 <- hhtmp4 %>% html_nodes(".entry-content p")
    vText4 <- c(vText4,
                paste(lstP4 %>% html_text2(),collapse = " "))
  }
} 
p_principles <- data.frame(text=vText4)
#print vector of text without quotes and pages numbers
p_principles<-noquote(p_principles)
p_principles<-gsub('["]', '', p_principles)
p_principles<-gsub("[[:digit:]]", "", p_principles) 


# 5) Permaculture Association
#read the html page 
blog5 <- read_html("https://blog.permaculture.org.uk/") 
# vector with the links to all full news in the first page 
link_fulltext5 <- blog5 %>% html_nodes(".field-content a ") %>% html_attr("href") %>% unique() 
link_fulltext5h = paste0('https://blog.permaculture.org.uk', link_fulltext5)
#vector for text information
vText5 <- c()
for(i in 1:length(link_fulltext5h)){
  print(i)
  hhtmp5 <- read_html(link_fulltext5h [i])
  lstP5 <- hhtmp5 %>% html_nodes(".field-item p")
  vText5 <- c(vText5,
              paste(lstP5[1:(length(lstP5)-1)] %>% html_text2()),collapse = " ")
} 
p_association<- data.frame(text=vText5)
#print vector of text without quotes and pages numbers
p_association<-noquote(p_association)
p_association<-gsub('["]', '', p_association)
p_association<-gsub("[[:digit:]]", "", p_association) 


# 6) shades of green
#read the html page 
vText6 <- c()
for(k in 1:10){
  link6 <- paste("https://shadesofgreenpermaculture.com/blog/page/",k,"/",sep="")
  blog6 <- read_html(link6)
  link_fulltext6<- blog6 %>% html_nodes(".se-t  a ") %>% html_attr("href") %>% unique()
  link_fulltext_cleaned6 <- grep("https", link_fulltext6, value = TRUE) # Extract elements with https only
  link_fulltext_cleaned6 <-link_fulltext_cleaned6[-c(6, 7)] 
  # vectors declaration
  for(i in 1:length(link_fulltext_cleaned6)){
    print(paste(k,i))
    # reads every single news item
    hhtmp6 <- read_html(link_fulltext_cleaned6[i])
    lstP6 <- hhtmp6 %>% html_nodes("body p")
    vText6 <- c(vText6,
                paste(lstP6 %>% html_text2(),collapse = " "))
  }
} 
shades_green <- data.frame(text=vText6)
#print vector of text without quotes and pages numbers
shades_green<-noquote(shades_green)
shades_green<-gsub('["]', '', shades_green)
shades_green<-gsub("[[:digit:]]", "", shades_green) 


# 7) Tenth Acre Farm
#read the html page 
vText7 <- c()
for(k in 1:4){
  link7 <- paste("https://www.tenthacrefarm.com/category/article/page/",k,"/",sep="")
  blog7 <- read_html(link7)
  link_fulltext7<- blog7 %>% html_nodes("article  a ") %>% html_attr("href") %>% unique()
  link_fulltext_cleaned7 <- grep("https", link_fulltext7, value = TRUE) # Extract elements with https only
  # vectors declaration
  for(i in 1:length(link_fulltext_cleaned7)){
    print(paste(k,i))
    # reads every single news item
    hhtmp7 <- read_html(link_fulltext_cleaned7[i])
    lstP7 <- hhtmp7 %>% html_nodes("article p")
    vText7 <- c(vText7,
                paste(lstP7 %>% html_text2(),collapse = " "))
  }
} 
thenth_acre <- data.frame(text=vText7)
#print vector of text without quotes and pages numbers
thenth_acre<-noquote(thenth_acre)
thenth_acre<-gsub('["]', '', thenth_acre)
thenth_acre<-gsub("[[:digit:]]", "", thenth_acre) 


# 8) Deep Green Permaculture
#read the html page 
vText8 <- c()
for(k in 1:2){
  link8 <- paste("https://deepgreenpermaculture.com/blog/page/",k,"/",sep="")
  blog8 <- read_html(link8)
  link_fulltext8<- blog8 %>% html_nodes(".entry-title  a ") %>% html_attr("href") %>% unique()
  link_fulltext_cleaned8 <- grep("https", link_fulltext8, value = TRUE) # Extract elements with https only
  # vectors declaration
  for(i in 1:length(link_fulltext_cleaned8)){
    print(paste(k,i))
    # reads every single news item
    hhtmp8 <- read_html(link_fulltext_cleaned8[i])
    lstP8 <- hhtmp8 %>% html_nodes(".entry-content p")
    vText8 <- c(vText8,
                paste(lstP8 %>% html_text2(),collapse = " "))
  }
} #40 articles (2 pages)
deep_green <- data.frame(text=vText8)
#print vector of text without quotes and pages numbers
deep_green<-noquote(deep_green)
deep_green<-gsub('["]', '', deep_green)
deep_green<-gsub("[[:digit:]]", "", deep_green) 


# 9) Mid West Permaculture
#read the html page 
vText9 <- c()
for(k in 1:4){
  link9 <- paste("https://midwestpermaculture.com/blog/page/",k,"/",sep="")
  blog9 <- read_html(link9)
  link_fulltext9<- blog9 %>% html_nodes(".elementor-post__title  a ") %>% html_attr("href") %>% unique()
  link_fulltext_cleaned9 <- grep("https", link_fulltext9, value = TRUE) # Extract elements with https only
  # vectors declaration
  for(i in 1:length(link_fulltext_cleaned9)){
    print(paste(k,i))
    # reads every single news item
    hhtmp9 <- read_html(link_fulltext_cleaned9[i])
    lstP9  <- hhtmp9 %>% html_nodes("body p")
    vText9 <- c(vText9,
                paste(lstP9 %>% html_text2(),collapse = " "))
  }
} #48 articles (4 pages)
mid_west <- data.frame(text=vText9)
#print vector of text without quotes and pages numbers
mid_west<-noquote(mid_west)
mid_west<-gsub('["]', '', mid_west)
mid_west<-gsub("[[:digit:]]", "", mid_west) 


# 10) Freepermaculture
#read the html page 
blog10 <- read_html("https://www.freepermaculture.com/index/") 
# vector with the links to all full news in the first page 
link_fulltext10 <- blog10 %>% html_nodes(".entry-title a ") %>% html_attr("href") %>% unique() 
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
free_permaculture<- data.frame(text=vText10)
#print vector of text without quotes and pages numbers
free_permaculture<-noquote(free_permaculture)
free_permaculture<-gsub('["]', '', free_permaculture)
free_permaculture<-gsub("[[:digit:]]", "", free_permaculture) 


# 11) Verge
#read the html page 
vText11 <- c()
for(k in 1:2){
  link11 <- paste("https://vergepermaculture.ca/blog-2/page/",k,"/",sep="")
  blog11 <- read_html(link11)
  link_fulltext11<- blog11 %>% html_nodes(".the-title  a ") %>% html_attr("href") %>% unique()
  link_fulltext_cleaned11 <- grep("https", link_fulltext11, value = TRUE) # Extract elements with https only
  # vectors declaration
  for(i in 1:length(link_fulltext_cleaned11)){
    print(paste(k,i))
    # reads every single news item
    hhtmp11 <- read_html(link_fulltext_cleaned11[i])
    lstP11  <- hhtmp11 %>% html_nodes("article p")
    vText11 <- c(vText11,
                 paste(lstP11 %>% html_text2(),collapse = " "))
  }
} # 60 articles (2 pages)
verge <- data.frame(text=vText11)
#print vector of text without quotes and pages numbers
verge<-noquote(verge)
verge<-gsub('["]', '', verge)
verge<-gsub("[[:digit:]]", "", verge) 


# 12) Good Life Permaculture
#read the html page 
blog12 <- read_html("https://goodlifepermaculture.com.au/blog/") 
# vector with the links to all full news in the first page 
link_fulltext12 <- blog12 %>% html_nodes(".entry-title a ") %>% html_attr("href") %>% unique() 
link_fulltext_cleaned12 <- grep("https", link_fulltext12, value = TRUE)
#vector for text information
vText12 <- c()
for(i in 1:length(link_fulltext_cleaned12)){
  print(i)
  hhtmp12 <- read_html(link_fulltext_cleaned12 [i])
  lstP12 <- hhtmp12 %>% html_nodes(".et_pb_column p")
  vText12 <- c(vText12,
               paste(lstP12[1:(length(lstP12)-1)] %>% html_text2()),collapse = " ")
} 
goodlife<- data.frame(text=vText12)
#print vector of text without quotes and pages numbers
goodlife<-noquote(goodlife)
goodlife<-gsub('["]', '', goodlife)
goodlife<-gsub("[[:digit:]]", "", goodlife) 


# 13) Permaculture women's guild
#read the html page 
blog13 <- read_html("https://www.permaculturewomen.com/magazine/") 
# vector with the links to all full news in the first page 
link_fulltext13 <- blog13 %>% html_nodes(".ultp-block-title  a ") %>% html_attr("href") %>% unique() 
link_fulltext_cleaned13 <- grep("https", link_fulltext13, value = TRUE)
#vector for text information
vText13 <- c()
for(i in 1:length(link_fulltext_cleaned13)){
  print(i)
  hhtmp13 <- read_html(link_fulltext_cleaned13 [i])
  lstP13 <- hhtmp13 %>% html_nodes("article p")
  vText13 <- c(vText13,
               paste(lstP13[1:(length(lstP13)-1)] %>% html_text2()),collapse = " ")
} 
women_magazine<- data.frame(text=vText13)
#print vector of text without quotes and pages numbers
women_magazine<-noquote(women_magazine)
women_magazine<-gsub('["]', '', women_magazine)
women_magazine<-gsub("[[:digit:]]", "", women_magazine) 


# 14) Morag Gamble
#read the html page 
vText14 <- c()
for(k in 1:4){
  link14 <- paste("https://ourpermaculturelife.com/category/permaculture/page/",k,"/",sep="")
  blog14 <- read_html(link14)
  link_fulltext14<- blog14 %>% html_nodes(".entry-title  a ") %>% html_attr("href") %>% unique()
  link_fulltext_cleaned14 <- grep("https", link_fulltext14, value = TRUE) # Extract elements with https only
  # vectors declaration
  for(i in 1:length(link_fulltext_cleaned14)){
    print(paste(k,i))
    # reads every single news item
    hhtmp14 <- read_html(link_fulltext_cleaned14[i])
    lstP14  <- hhtmp14 %>% html_nodes("body p")
    vText14 <- c(vText14,
                 paste(lstP14 %>% html_text2(),collapse = " "))
  }
} 
morag_gamble <- data.frame(text=vText14)
#print vector of text without quotes and pages numbers
morag_gamble<-noquote(morag_gamble)
morag_gamble<-gsub('["]', '', morag_gamble)
morag_gamble<-gsub("[[:digit:]]", "", morag_gamble) 


# 15) Permaresilience
#read the html page 
blog15 <- read_html("https://permaresilience.com/permaculture-articles/") 
# vector with the links to all full news in the first page 
link_fulltext15 <- blog15 %>% html_nodes(".title  a ") %>% html_attr("href") %>% unique() 
link_fulltext_cleaned15 <- grep("https", link_fulltext15, value = TRUE)
#vector for text information
vText15 <- c()
for(i in 1:length(link_fulltext_cleaned15)){
  print(i)
  hhtmp15 <- read_html(link_fulltext_cleaned15 [i])
  lstP15 <- hhtmp15 %>% html_nodes("article p")
  vText15 <- c(vText15,
               paste(lstP15[1:(length(lstP15)-1)] %>% html_text2()),collapse = " ")
} 
permaresilience<- data.frame(text=vText15)
#print vector of text without quotes and pages numbers
permaresilience<-noquote(permaresilience)
permaresilience<-gsub('["]', '', permaresilience)
permaresilience<-gsub("[[:digit:]]", "", permaresilience) 

## ---------- DATAFRAME - document text ---------- ##
blog_number<- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")
blog_name<- c("Permaculture.uk", "Permaculture Research Institute", "Milkwood", "Permaculture Principles", "Permaculture Association", "Shades of Green", "Tenth Acre Farm", "Deep Green Permaculture", "Mid West Permaculture", "Freepermaculture", "Verge", "Good Life Permaculture", "Permaculture women's guild", "Morag Gamble", "Permaresilience")
text <- c(perm_uk, pri, milkwood, p_principles, p_association, shades_green, thenth_acre, deep_green, mid_west, free_permaculture, verge, goodlife, women_magazine, morag_gamble, permaresilience )
corpus_df <- data.frame(blog_number, blog_name, text)
corpus_fulltext<-as.vector(as.String(corpus_df$text))


########## (2) THE CORPUS ####
## ZIPF'S LAW ###
corpus_ft <- scan("corpus_fulltext.txt", what = "character") #851233 items
corpus_ft <- removePunctuation(corpus_ft)
corpus_ft <- tolower(corpus_ft)
#corpus size: 851,233

## Word Frequency Matrix alpha (count list per word)
corpus.table <- table(corpus_ft)
corpus.table <- sort(corpus.table, decreasing = TRUE)
corpus.table [1:10] #the, and, to, of, a, in
# dataframe: rank, frequency, word (for the alpha analysis), log of word freq
a.corpus = data.frame(rank = c(1:length(corpus.table)), 
                      freq = as.numeric(corpus.table), 
                      type = names(corpus.table))
a.corpus$logFreq <- log2(a.corpus$freq)
a.corpus$logRank <- log2(a.corpus$rank)
head(a.corpus, 4)
write.table(a.corpus, file="wfm.csv", sep=",")
# alpha distribution 
par(mfrow = c(1,2))
plot(a.corpus$logRank, a.corpus$logFreq, main = "Alpha distribution Corpus", xlab = "log rank", ylab = "log frequency", family = "Times New Roman", cex=0.7)
#linear regression
a.corpus.lm <- lm(logFreq ~ logRank, weights = freq, data = a.corpus)
abline(a.corpus.lm, col = "red", lwd=2.0)
summary(a.corpus.lm)
#log rank (slope) close to 1: -0.9990092 (p-value<2e-16) 
#adjusted R-squared:0.9862 (98.6% of data variability is explained)
#--> This is a good fit, so we can conclude that Zipf's law applies.


###GROWTH CURVE ###

#chunk size: 1000, n chunk:850
corpus.growth <- growth.fnc(text = corpus_ft, size = 1000, nchunks = 850)
corpus.g <- corpus.growth@data$data
  par(mfrow = c(1,3))
  options(scipen=5)
  plot(corpus.g$Tokens,corpus.g$Types, xlab = "Tokens", ylab ="Types", family = "Times New Roman")
  plot(corpus.g$Tokens, corpus.g$HapaxLegomena, xlab = "Tokens", ylab ="Legomena", pch = 3, col = "red", family = "Times New Roman") 
  points(corpus.g$Tokens, corpus.g$DisLegomena,  pch = 4, col = "blue")
  points(corpus.g$Tokens, corpus.g$TrisLegomena, pch = 8, col = "green")

#### ZIPF-MANDELBROT MODEL / GIGP ###
c.table <- table(table(corpus))
head(c.table) #12223 hapax, 4710 dis, 2036 tris
c.spc <- spc(m = as.numeric(names(c.table)), Vm = as.numeric(c.table))
#Zipf-Mandelbrot (ZM) LNRE model -- see lnre.zm (formulas)
#or Generalized Inverse Gauss-Poisson (GIGP) LNRE model see lnre.gigp
c.lnre.gp <- lnre("gigp", c.spc)
c.lnre.gp #goodness of fit
par(mfrow = c(1,2))
plot(c.spc, lnre.spc(c.lnre.gp, 623985), family = "Times New Roman")

#### VOCABULARY GROWTH CURVES ###
c.ext.gp<- lnre.vgc(c.lnre.gp, seq(N(c.lnre.gp),
                                   N(c.lnre.gp)*3, length = 20), m.max = 3)
c.int.gp<- lnre.vgc(c.lnre.gp, seq(1,
                                   N(c.lnre.gp), length = 20), m.max = 3)
c.vgc <- growth2vgc.fnc(corpus.growth)
plot(c.int.gp, c.ext.gp, c.vgc, add.m = 1:3, main ="Vocabulary Growth",
     xlab = "sample size", ylab ="types")

##-- LEXICAL DIVERSITY ##
head(c.spc) #Vm #m #N(tokens): 851233 #V(types):36484
N<-851233
V<-36484
ttr<-V/N #type/token ratio #4.28% #lexical diversity
G<- V/sqrt(N)
cttr<-V/sqrt(2*N)
H<- log(V)/log(N)
S<- log(logV)/log(log(N))
M<- (log(N)-log(V))/log2(N)
c(ttr, G, cttr, M)
MSTTR(tagged.text)
MTLD(tagged.text)
HDD(tagged.text)
#lexical originality
(V*100)/N #--> 4.28 (kumar, 2016)







########## (3) TEXT PREPROCESSING #####
##### (3.1.) CLEANING #####
cleanText <- function(xtxt = NULL, hashtag = TRUE, mention = TRUE, numbers = FALSE,
                      punctation = FALSE, lowercase = TRUE){
  # check if x is defined and is an object of type character
  if(is.null(xtxt) | class(xtxt) != "character"){stop("invalid character vector")}
  # check if the other arguments are logical
  if(!is.logical(hashtag) | !is.logical(hashtag) | !is.logical(numbers) | !is.logical(punctation) | !is.logical(lowercase)){
    stop("invalid argument")}
  # html symbols vector
  htmlsymbols <- c("&copy;", "&reg;", "&trade;", "&ldquo;", "&lsquo;", "&rsquo;", "&bull;", "&middot;", "&sdot;",
                   "&ndash;", "&mdash;", "&cent;", "&pound;", "&euro;", "&ne;", "&frac12;", "&frac14;", "&frac34;",
                   "&deg;", "&larr;", "&rarr;", "&hellip;", "&nbsp;", "&lt;", "&gt;", "&amp;", "&quot;")
  htmlsymbolsU <- paste(htmlsymbols,collapse = "|")
  # remove links
  xtxt = gsub("(f|ht)(tp)(s?)(://)(.\\S+)[.|/](.\\S+)", " ", xtxt)
  # remove references in retweets
  xtxt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", xtxt)
  xtxt = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", xtxt)
  # html symbols
  xtxt = gsub(htmlsymbolsU," ",xtxt)
  # punctation
  if(punctation == TRUE){xtxt = gsub("([#@])|[[:punct:]]", " \\1", xtxt)}
  # control characters
  xtxt = gsub('[[:cntrl:]]', ' ', xtxt)
  # those that are not graphic characters (what is not [[:alnum:][:punct:]])
  xtxt = gsub('[^[:graph:]]', ' ', xtxt)
  # hashtag
  if(hashtag == TRUE) xtxt = gsub("#\\S+", " ", xtxt)
  # mention
  if(mention == TRUE) xtxt = gsub("@\\S+", " ", xtxt)
  # numbers
  if(numbers == TRUE) xtxt = gsub("[[:digit:]]", "", xtxt)
  # tabulations and more spaces in the middle of the text
  xtxt = gsub("[ \t]{2,}", " ", xtxt)
  xtxt = gsub('\\s+',' ',xtxt)
  # spaces at the beginning and end of texts
  xtxt = gsub("^\\s+|\\s+$", "", xtxt)
  # turns everything into lowercase
  if(lowercase == TRUE) xtxt = tolower(xtxt)
  return(xtxt)
}
corpus_df$text_clean<- cleanText(xtxt = corpus_df$text,
                                 hashtag = T, mention = T, numbers = T,
                                 punctation = T, lowercase = T)

##### (3.2) NORMALIZATION ##### 

  #Tokenization -Lemmatization - PoS Tagging
  corpus_df$text_utf<- iconv(corpus_df$text_clean, to = "UTF-8")
  udpipe_download_model("english-ewt", model_dir = "./functions/")
  ud_model <- udpipe_load_model("./functions/english-ewt-ud-2.5-191206.udpipe")
  corpus_tag<- udpipe(x = corpus_df$text_utf, object = ud_model, doc_id = 1:nrow(corpus_df))
  corpus_tag<- corpus_tag %>% select(token,lemma,upos)
  tibble(text = corpus_tag)
  
  # Stopwords removal # 
  sort(tm::stopwords("english"))
  corpus_tag$stopwords <- ifelse(tolower(corpus_tag$lemma) %in% stopwords("english") |
                                   tolower(corpus_tag$token) %in% stopwords("english"),TRUE,FALSE)
  corpus_tag_noSW<-corpus_tag[corpus_tag$stopwords == 'FALSE' & !corpus_tag$upos 
                              %in% c("PUNCT","X","PART","SYM","DET", "CCONJ", "ADP", "PRON", "SCONJ", "PROPN"),]
  nrow(corpus_tag)-nrow(corpus_tag_noSW) #402023 stopwords removed 
  #now corpus has 461641 occurrences!
  corpus_tag_noSW_t<-corpus_tag_noSW[!(nchar(corpus_tag_noSW$token) == 2),]
  Corpus_no_SA <- tm_map(corpus_df$text_utf, content_transformer(function(x) removeWords(x, stopwords("en"))))
  inspect(Corpus_no_SA[[1]])

#### (3.3) STRUCTURING:####
  
  #TD matrix
    corpus_bow <- Corpus(VectorSource(corpus_df$text_clean))
    corpus_bow <- tm_map(corpus_bow, removeWords, stopwords("english"))
    tdm_sp <- TermDocumentMatrix(x = corpus_bow, control = 
                                   list(stopwords = stopwords::stopwords(language = "en", source = "smart")))
    #sparsity: 80% (terms that occurs just a few times)
    tdm<-removeSparseTerms(tdm_sp, 0.3)
    #sparsity: 16% 
    mx.tdm <- as.matrix(tdm)
     #write.table(mx.tdm, file="tdm.csv", sep=",")
    


########## (4) TEXT ANALYSIS #####   
 #### (4.1) WORD-LEVEL ####
    
    # Frequency (unigrams)
    findFreqTerms(tdm, 200) #frequent tokens (occurring more than 50 times)
    fw.tdm <- data.frame(words=rownames(mx.tdm),mx.tdm,freq=rowSums(mx.tdm))
    rownames(fw.tdm) <- NULL
    fw.tdm.utf <- iconv(fw.tdm$words, to = "UTF-8")
    fw_pos<- udpipe(x = fw.utf, object = ud_model)
    fw_pos<-fw_pos %>% select(token,lemma,upos)
    nrow(fw_pos) #1219
    sum(fw.tdm$freq) #213903 token freq
    #merge between fw(words, freq) and fw_pos(token, lemma, upos)
    merge_full<-merge(x = fw_pos, y = fw.tdm, by.x = "lemma", by.y = "words")
    freqLem_tdm<-merge_full %>% select(!(token:upos)) %>% distinct(lemma, .keep_all = TRUE)
      #write.table(freqLem_tdm, file="freqLem_tdm.csv", sep=",")
    freqLem_tdm<- read.csv("freqLem_tdm.csv",header=TRUE, sep=";")
    freqLem_tdm %>% select(lemma, freq) 
    tot.tokens<-sum(freqLem_tdm$freq,na.rm=TRUE) #162406
    freqLem_tdm$rfreq<-as.numeric(as.character(freqLem_tdm$freq)) / tot.tokens
    tot.r<-sum(freqLem_tdm$rfreq,na.rm=TRUE) #1
    freqLem_tdm$nrfreq<-as.numeric(as.character(freqLem_tdm$rfreq)) * 10000
    #write.table(freqLem_tdm, file="freqLem_tdm_rn.csv", sep=",")
    
    ### wordcloud
    set.seed(1515)
    wordcloud(words = freqLem_tdm$lemma,freq = freqLem_tdm$freq, scale = c(3,0.5),
              max.words = 1000, random.order = FALSE, rot.per= 0.15, shape="round", 
              colors=c("#8FD047", "#498505", "#4A6927")) 
    set.seed(1234)
    wordcloud(words = freqLem_tdm$lemma,freq = freqLem_tdm$freq
              , scale=c(3,0.5), min.freq = 1,
              max.words=1000, random.order=FALSE, rot.per=0.15, 
              colors=c("#8FD047", "#65B807", "#4A6927"))
    
    ### bar plot (most frequent 20)
    plot_LemFreq<- freqLem_tdm %>% select(lemma, nrfreq) %>%
      arrange(-nrfreq) %>% slice(1:20)%>%
      ggplot(aes(x = reorder(lemma, nrfreq), y = nrfreq)) +
      geom_bar(stat = "identity", width=0.8, fill="grey70") + coord_flip() +
      xlab("") + ylab("") + theme_bw() + 
      labs(title = "Words frequency",
           subtitle = "20 most frequent lemmas",
           x = "Lemmas",
           y = "Normalized Frequency") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman", size = 9),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))

    ## ngrams 
    source("multWordPOS.R") # function to identify multiwords using pos-tagging
    multWordPOS(x = corpus_df$text_clean, model = "english", n = Inf, save.xlsx = T)
    ngrams<-read_xlsx("ngrams.xlsx")
    ngrams[grep("permaculture", ngrams$tokens), ] #only containing 'permaculture'
    
    #plot
    plot_ngrams<-ngrams %>% select(tokens, freq) %>%
      arrange(-freq) %>% slice(1:25)%>%
      ggplot(aes(x = reorder(tokens, freq), y = freq)) +
      geom_bar(stat = "identity", width=0.8, fill="grey60") + coord_flip() +
      xlab("") + ylab("") + theme_bw() + 
      labs(title = "n-grams frequency",
           subtitle = "25 most frequent n-grams",
           x = "n-grams",
           y = "Absolute occurrence") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman", size = 9),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))






####  (4.2) TWO-WORDS-LEVEL ####
    
    #### Document level associations 
    perm_corr <-data.frame(findAssocs(tdm, "permaculture", 0.7))
    #write.table(perm_corr, file="perm_corr.csv", sep=",")
    perm_corr<-read_xlsx("perm_corr.xlsx")
    perm_corr<-as.data.frame(perm_corr)
    # Plot 
    perm_corr %>%
      ggplot(aes(x=reorder(word, corr), y=corr)) + 
      geom_point(shape=20, size=3.5, aes(colour = cut(corr, c(-Inf, 0.78, Inf)))) +  
      scale_color_manual(name = "corr", values = c("(-Inf, 0.78]" = "grey",
                                    "(0.78, Inf]" = "#2A3613"),
                         labels = c("corr => 0.80"))+ coord_flip()+
      labs(x = "Tokens", y = "Association score") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman", size = 9),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))

    
    #### sentence-wise co-occurrence ### 
    corpus_sent <- corpus(corpus_df$text, docnames = corpus_df$blog_name)
    ndoc(corpus_sent) #15 doc
    corpus_sentences <- corpus_reshape(corpus_sent, to = "sentences")
    ndoc(corpus_sentences) #40051 sentences
    #pre-processing according to sentences instead of documents
    corpus_sentences<- removePunctuation(corpus_sentences)
    corpus_sentences <- tolower(corpus_sentences)
    #dtm
    corpus_sentences <- Corpus(VectorSource(corpus_sentences))
    toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
    corpus_sentences <- tm_map(corpus_sentences, toSpace, "–")
    corpus_sentences <- tm_map(corpus_sentences, toSpace, "’s")
    corpus_sentences <- tm_map(corpus_sentences, toSpace, "’re")
    corpus_sentences <- tm_map(corpus_sentences, removeWords, c(stopwords("english"), "morag", "...", "the", "bed", "beds", "blog", "online", "related", "pdc"))
    dtm_sp<-DocumentTermMatrix(x = corpus_sentences)
    #40051
    dtm<-removeSparseTerms(dtm_sp, 0.995)
    mx.dtm<-as.matrix(dtm)
    #binary dtm
    mx.dtm[mx.dtm > 0] <- 1
    # term-term adjacency matrix (co-occurrence matrix) joint occurrences
    cooc <- t(mx.dtm) %*% mx.dtm
    as.matrix(cooc[155:160, 155:160]) ### number they co-occur in same sentence
    # coocurrences measures for permaculture  
    source("CoocStatisticsFunction.R")
    numberOfCoocs <- 15
    coocTerm <- "permaculture"
    coocs <- CoocStatisticsFunction(coocTerm, mx.dtm, measure="LOGLIK")
    print(coocs[1:numberOfCoocs])
    resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
    # Entry of the search word into the first column in all lines
    tmpGraph[, 1] <- coocTerm
    # Entry of the co-occurrences into the second column of the respective line
    tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
    # Set the significances
    tmpGraph[, 3] <- coocs[1:numberOfCoocs]
    # Attach the triples to resultGraph
    resultGraph <- rbind(resultGraph, tmpGraph)
    # Iteration over the most significant numberOfCoocs co-occurrences of the search term
    for (i in 1:numberOfCoocs){
      newCoocTerm <- names(coocs)[i]
      coocs2 <- CoocStatisticsFunction(newCoocTerm, mx.dtm, measure="LOGLIK")
      # Structure of the temporary graph object
      tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
      tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
      tmpGraph[, 1] <- newCoocTerm
      tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
      tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
      resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])}

    ### plot 
    set.seed(1)
    graphNetwork <- graph.data.frame(resultGraph, directed = F)
   # nodes properties
    V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, "#8C8C8C", "#D9D9D9") 
    V(graphNetwork)$size <- scales::rescale(log(degree(graphNetwork)), to = c(5, 15))
    # edge properties
    E(graphNetwork)$color <- adjustcolor("DarkGray", alpha.f = .5)
    E(graphNetwork)$width <- scales::rescale(E(graphNetwork)$sig, to = c(1, 10))
    E(graphNetwork)$curved <- 0.15 
    par(mai=c(0,0,0,0)) 
    # Final Plot
    plot( graphNetwork, layout = layout.fruchterman.reingold, 
      vertex.label.family = "Times New Roman", vertex.label.cex = 0.8,
      vertex.shape = "circle",vertex.label.dist = 0.5,          
      vertex.frame.color = adjustcolor("darkgray", alpha.f = .5),
      vertex.label.color = 'black', vertex.label.font = 1,            
      vertex.label = V(graphNetwork)$name, vertex.label.cex = 0.3)
    
    
    
    
    
    
    
    #correlation between those words
    vector_words<-c("permaculture", "design", "education", "principles", "sustainable", "life", 
                    "business", "course", "check", "community", "knowledge", 
                    "urban", "guide", "farm", "earth", "garden", 
                    "projects", "garden", "projects", "people", "ways")
    permaculture_corr<-word_cor(tdm, vector_words, type="tdm", method = "pearson", min = 0.7, p = 0.4)
    permaculture_corr$pMatrix[is.na(permaculture_corr$pMatrix)] <- 1 
    library(corrplot)
    corrplot(permaculture_corr$pMatrix, method="circle") 
    
    
    
    
    
##### (4.3) CONTEXT LEVEL ####
     
    
    #### Topic Modeling  ##
    #dtm
    text_vector<-as.vector(corpus_df$text_clean)
    name_vector<-as.vector(corpus_df$blog_name)
    dtm.d <- CreateDtm(doc_vec = text_vector, doc_names = name_vector, 
                       ngram_window = c(1, 2), # min, max n-gram length
                       stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart"),"accomplished_brings","logo" , "pawpaw", "estates_ga","rights_reserved","midwest_permaculture","shades_green","passive_solar","abalone_recipe", "related_tag","articles_show", "aback_banks","hannah_moloney","aba_accelerates", "morag", "amy","ab_american", "aadi_rajpoot","aafco","aaron","aaron_day","aafco_website", "aafc","aafc_developing", "aadi", "rajpoot"), 
                       lower = TRUE, remove_punctuation = TRUE, remove_numbers = TRUE, 
                       verbose = FALSE, cpus = 2) 
    #choice of k:number of topics
    ?ldatuning
    bestk <-ldatuning::FindTopicsNumber(dtm.d, topics = seq(from = 2, to = 10, by = 1),
                                        metrics = c("CaoJuan2009",  "Deveaud2014"), method = "Gibbs", 
                                        control = list(seed = 15), verbose = TRUE)
    FindTopicsNumber_plot(bestk)
    k <- 6
    
    #Latent Dirichlet Allocation model (LDA)
    set.seed(15)
    lda_4<- LDA(dtm.d, k, method="Gibbs", control=list(iter = 500, verbose = 25))#Gibbs sampling
    beta_topics<- tidy(lda_4, matrix="beta") #prob that a word is associated to a topic (higher beta, more frequent the terms appears)
    terms(lda_4, 10)
    beta_top_terms <-beta_topics %>% group_by(topic) %>% slice_max(beta, n=10) %>% 
      ungroup() %>% arrange(topic, -beta)
    #plots
    beta_top_terms %>%
      mutate(term= reorder_within(term,beta,topic)) %>%
      ggplot(aes(beta,term,fill=factor(topic)))+
      geom_col(show.legend = TRUE)+ facet_wrap(~ topic, scales="free")+
      scale_y_reordered() + scale_fill_grey() + xlab("") + ylab("")  + 
      labs(title = "Latent Dirichlet Allocation Topics",
           subtitle = "10 most frequent words of 6 topics",
           x = "words", y = "beta") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman", size = 9),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))
  
    
    #### Text Clustering ##

    #Cosine/Jaccard distance
    dist_matrix<-dist(mx.tdm, method="jaccard")
    #(I tried with euclidean, but jaccard and cosine give a better dendrogram)
    # Hierarchical Clustering with Ward method
    hc_w <- hclust(d = dist_matrix, method = "ward.D2")
    # dendrogram
    par(mar=c(1,2,2,1))
    plot(hc_w, hang = -1,sub = "", cex=0.5,
         main="Hierarchical Clustering (Jaccard-Ward)", 
         cex.main=0.9, xlab="", cex.axis=0.6, cex.lab=0.2)
    #best k "silhouette", "wss", "gap_stat"
    par(mfrow = c(1,3))
    #elbow (WSS)
    options(scipen=5)
    fviz_nbclust(mx.tdm, hcut, method = "wss")+
      geom_vline(xintercept = 4, linetype = 2)+
      labs(subtitle = "Elbow method")+
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman"),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))
    # Silhouette method
    fviz_nbclust(mx.tdm, hcut, method = "silhouette")+
      labs(subtitle = "Silhouette method") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman"),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))
    # Gap statistic
    set.seed(123)
    ?fviz_nbclust
    fviz_nbclust(mx.tdm, hcut, nstart = 25,  method = "gap_stat", nboot = 50)+
      labs(subtitle = "Gap statistic method")+ 
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman"),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))
    
    # NbClust() function: 30 indices for choosing the best number of clusters
    NbClust(data = mx.tdm, diss = NULL, distance = "manhattan",
            min.nc = 2, max.nc = 15, method = "ward.D")+
      labs(subtitle = "Optimal number")+ 
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman"),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))
    # 11 proposed 3 as the best number of clusters 
    #According to the majority rule, the best number of clusters is  3 
    k_num<-   c("2","3","4","5","6","7","8","9","10")
    freq_mes<-c(2,11,2,0,3,0,1,2,0)
    df_k <- data.frame(k_num, freq_mes)
    # plot
    ggplot(df_k, aes(x=factor(k_num, level=c("2","3","4","5","6","7","8","9","10")), y=freq_mes)) +
      geom_bar(stat="identity")+
      labs(title = "Optimal number of clusters",
           subtitle = expression(paste("30 indices by ", italic("NbClust"), " package")),
           x = "Number of clusters k",
           y = "Frequency among indices") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman", size = 14),
            axis.title.x  = element_text(family = "Times New Roman", size = 12),
            axis.title.y  = element_text(family = "Times New Roman", size = 12),
            axis.text.x   = element_text(family = "Times New Roman", size = 12),
            axis.text.y   = element_text(family = "Times New Roman", size = 12))
    
    #3 is chosen also according to the dendrogram
    nclu <- 3 # number of clusters
    par(mar=c(1,2,2,1))
    plot(hc_w, hang = -1,sub = "", cex=0.5,
         main="Hierarchical Clustering (Jaccard-Ward)", 
         cex.main=0.9, xlab="", cex.axis=0.6, cex.lab=0.2)
    rect.hclust(hc_w,k = nclu)
    clust <- cutree(hc_w,k = nclu)
    # number of terms by cluster
    table(clust)
    # 1: 273/1219 = 0.2239541 
    # 2: 421/1219 =0.3453651
    # 3: 525/1219= 0.4306809
    
    # dendrogram 2
    fviz_dend(hc_w, k = 3, k_colors = c("#C7F059", "#145616","#89BA27"),
              type = "phylogenic", repel = TRUE,
              show_labels = TRUE, phylo_layout = "layout.gem")
    
    
    # group terms by cluster
    vTop <- c()
    for(i in 1:max(clust)){
      vTop <- c(vTop,paste(rownames(mx.tdm)[clust==i],collapse = " "))
    }
    clust_words<-data.frame(clust=unique(clust),terms=vTop) %>% as_tibble()
    #write.xlsx(clust_words, file="clustering_R.xlsx", sheetName = "clust_content",col.names = TRUE, row.names = TRUE, append = FALSE)
    
 
    
    
    
    
    
###### (4.4) GENERAL LEVEL: SENTIMENT #####
   
    source("EmotionFunction.R")
    source("PolarityFunction.R")
    source("SentimentFunction_syuzhet.R")
    source("SentimentFunction_udipipe.R")
    source("create_matrix.R")
    
    ######## polarity ##
    #subj
    cl.Pol <- PolarityFunction(textColumns = freqLem_tdm$lemma, algorithm = "bayes", lexicon = "subjectivity_en.csv")
    cl.Pol$words %>% head(4)
    pol_lemmas<- data.frame(cl.Pol$words$word, cl.Pol$words$category)
    write_csv(pol_lemmas, "polarity_lemmas.csv")
    plot_sub<-cl.Pol$documents  %>% group_by(polarity=best_fit) %>% summarise(n=n())%>% mutate(perc=n/sum(n)*100) %>%
      ggplot(aes(x=polarity,y=perc,fill=polarity))+
      geom_bar(stat = "identity")+ theme_light()+
      scale_fill_manual(values = c("#2A3613", "grey80", "#ABC17E"))+
      theme(legend.position = "none")+
      labs(title = "Sentiment polarity distribution",
           subtitle = "Subjective polarity dictionary",
           x = "Polarity", y = "Relative frequency (%)") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman", size = 9),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))
    
    cl.Pol_words<- cl.Pol$words %>% group_by(category) %>% summarise(txtL=paste(word,collapse = " "))
    corpPol<-Corpus(VectorSource
                    (cl.Pol_words$txtL))
    tdmPol<- TermDocumentMatrix(corpPol)
    tdmPol<- as.matrix(tdmPol)
    colnames(tdmPol) <- cl.Pol_words$category
    library(wordcloud)
    par(mar=c(0,0,0,0))
    set.seed(1234)
    comparison.cloud(tdmPol,max.words = 95, rot.per= 0.1, shape="round", colors = c("#5A7302","#B6D93B"),
                     scale = c(1,1), match.colors = T)
    text(0.5,1,"comparison cloud most important terms in polarity classification") 
    
    #Polarity with dictionaries
    dic.HaIV <- loadDictionaryGI() # Harvard-IV dictionary
    dic.LMFi <- loadDictionaryLM() #Loughran-McDonald dictionary 
    dic.QDAP <- loadDictionaryQDAP() #Polarity words from qdap package
    sent_SA <- analyzeSentiment(freqLem_tdm$lemma)
    sent_SA_GI <- convertToDirection(sent_SA$SentimentGI)
    gi<-as.data.frame(prop.table(table(sent_SA_GI))*100)
    sent_SA_LM <- convertToDirection(sent_SA$SentimentLM)
    lm<-as.data.frame(prop.table(table(sent_SA_LM))*100)
    sent_SA_QDAP <- convertToDirection(sent_SA$SentimentQDAP)
    qdap<-as.data.frame(prop.table(table(sent_SA_QDAP))*100)
    
    plot_gi<-ggplot(data = gi, aes(x=sent_SA_GI,y=Freq,fill=sent_SA_GI))+
      geom_bar(stat = "identity")+ theme_light()+
      scale_fill_manual(values = c("#2A3613", "grey80", "#ABC17E"))+
      theme(legend.position = "none")+
      labs(title = "Sentiment polarity distribution",
           subtitle = "Harvard-IV polarity dictionary",
           x = "Polarity", y = "Relative frequency (%)") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman", size = 9),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))
    
    plot_lm<-ggplot(data = lm, aes(x=sent_SA_LM,y=Freq,fill=sent_SA_LM))+
      geom_bar(stat = "identity")+ theme_light()+
      scale_fill_manual(values = c("#2A3613", "grey80", "#ABC17E"))+
      theme(legend.position = "none")+
      labs(title = "Sentiment polarity distribution",
           subtitle = "Loughran-McDonald  polarity dictionary",
           x = "Polarity", y = "Relative frequency (%)") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman", size = 9),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))
    
    plot_qdap<-ggplot(data = qdap, aes(x=sent_SA_QDAP,y=Freq,fill=sent_SA_QDAP))+
      geom_bar(stat = "identity")+ theme_light()+
      scale_fill_manual(values = c("#2A3613", "grey80", "#ABC17E"))+
      theme(legend.position = "none")+
      labs(title = "Sentiment polarity distribution",
           subtitle = expression(paste(italic("qdap"), " R package polarity dictionary")),
           x = "Polarity", y = "Relative frequency (%)") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            plot.subtitle = element_text(family = "Times New Roman", size = 9),
            axis.title.x  = element_text(family = "Times New Roman", size = 9),
            axis.title.y  = element_text(family = "Times New Roman", size = 9),
            axis.text.x   = element_text(family = "Times New Roman", size = 9),
            axis.text.y   = element_text(family = "Times New Roman", size = 8))
    
    grid.arrange(plot_sub, plot_gi, plot_lm, plot_qdap, ncol=4)
    
    sent_SR <- sentiment(text.var = freqLem_tdm$lemma,
                         polarity_dt = lexicon::hash_sentiment_senticnet,
                         valence_shifters_dt = lexicon::hash_valence_shifters,
                         amplifier.weight = 0.8, n.before = 5, n.after = 2)
    dist_pol<-hist(sent_SR$sentiment, main = "sentiment histogram", cex.main=1, 
                   col="grey90", xlab="Polarity value", ylab="Absolute frequency (n)", 
                   family="Times New Roman")
    
    
    ######## emotion #  
    #function
    cl.Emo<- EmotionFunction(textColumns = freqLem_tdm$lemma , algorithm = "bayes", lexicon = "emotions_en.csv")
    cl.Emo$words %>% head(4)
    cl.Emo$documents %>% group_by(emotion=best_fit) %>% summarise(n=n()) %>%mutate(perc.=n/sum(n)*100) %>% arrange(-n)
    plot_emo<-cl.Emo$documents %>% filter(best_fit != "NC") %>%
      group_by(emotion =best_fit) %>% summarise(n=n()) %>%
      mutate(perc=n/sum(n)*100) %>%
      ggplot(aes(x=reorder(emotion,n),y=perc,fill=emotion))+geom_bar(stat = "identity") +
      theme(legend.position = "none")+ theme_light()+ coord_flip()+
      geom_bar(stat = "identity", width = 0.3)+
      scale_fill_manual(values = c("#66822E", "#ABC17E", "#2A3613","#89BA27"))+
      theme(legend.position = "none")+
      labs(title = "Emotions distribution",
           x = "Emotions", y = "Relative Frequency (%)") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 14),
            axis.title.x  = element_text(family = "Times New Roman", size = 12),
            axis.title.y  = element_text(family = "Times New Roman", size = 12),
            axis.text.x   = element_text(family = "Times New Roman", size = 12),
            axis.text.y   = element_text(family = "Times New Roman", size = 12))
    
    
    #emo 2
    split_text <- get_sentences(freqLem_tdm$lemma)
    emot_SR <- emotion(text.var = split_text, emotion_dt = lexicon::hash_nrc_emotions,
                       valence_shifters_dt = lexicon::hash_valence_shifters)
    plot(emot_SR, facet = 'negated')
    
    
    #emo syuzhet
    get_sentiment_dictionary("syuzhet") %>% head()
    get_sentiment_dictionary("afinn") %>% head()
    get_sentiment_dictionary(dictionary = "bing") %>% head()
    get_sentiment_dictionary(dictionary = 'nrc',language = "english") %>% head()
    sent_SYn <- get_nrc_sentiment(char_v = freqLem_tdm$lemma,language = "english")
    syuz_sentEm <- sent_SYn[,1:8]
    tabEmoSyuz <- data.frame(Emotion=names(colSums(syuz_sentEm)),Freq=colSums(syuz_sentEm), Perc=colSums(syuz_sentEm)/sum(syuz_sentEm)*100)
    rownames(tabEmoSyuz) <- NULL
    
    plot_emo_sy<-tabEmoSyuz %>%
      ggplot(aes(x = reorder(Emotion,Perc),y = Perc,fill=Emotion))+
      geom_bar(stat="Identity",color="gray50")+
      scale_fill_manual(values=brewer.pal(8,"BuGn")) +
      theme_light()+ theme(legend.position="none")+
      coord_flip()+
      labs(title = "Syuzhet NRC Sentiment Emotions",
           x = "Emotions", y = "Relative Frequency (%)") +
      theme(plot.title    = element_text(family = "Times New Roman", size = 12),
            axis.title.x  = element_text(family = "Times New Roman", size = 12),
            axis.title.y  = element_text(family = "Times New Roman", size = 12),
            axis.text.x   = element_text(family = "Times New Roman", size = 12),
            axis.text.y   = element_text(family = "Times New Roman", size = 12))
    
  
    
    
    
    
    