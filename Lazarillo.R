#install.packages("lessR")
#install.packages("spacyr")
#install.packages("quanteda")
library(utf8)
library(spacyr)
library(tokenizers.bpe)
library(quanteda)

# We input our URL. Originally, the URL linked in the email led to a file
# encoded in ISO-8859-1. We could format it, but since we have an UTF-8
# mirror at our disposal on the same page, we will use that instead.

urlLazarillo <- "https://www.gutenberg.org/cache/epub/320/pg320.txt"
lines <- readLines(urlLazarillo, encoding = "UTF-8")
grep(pattern = "***", lines, fixed = TRUE)
linesQ <- lines[21:2145]
length(linesQ)
linesQ[1:10]
linesQ[2116:2125]

# We skip the prologue and preliminary notes by greping the
# first words of the first chapter. "Tratado"

grep(pattern = "Tratado", linesQ, fixed = TRUE) # [1]   78  575  981 1659 1677 1993 2021
linesQ <- linesQ[-c(1:77)]
linesQ <- linesQ[-c(2040:2048)]
length(linesQ)
linesQ[78:88]
linesQ[2030:2039]

paste(linesQ[1:5], collapse = " ")

linesQ[!utf8_valid((linesQ))]
linesQ_NFC <- utf8_normalize(linesQ)
sum(linesQ_NFC != linesQ)

stringQ <- paste(linesQ, collapse ="\n")
paras <- unlist(strsplit(stringQ, "\n\n\n", fixed = TRUE))
parEmpty <- which(paras == "")
length(paras)

substring(paras[1], 1, 50)
parclean <- gsub("[\n]{1,}"," ", paras)
paras <- gsub("[\n]{2,}"," ", parclean)
substring(paras[1], 1, 50)

spacy_install()
spacy_download_langmodel('es')
spacy_initialize(model = "es_core_news_sm")

sentences <- spacy_tokenize(paras, what="sentence")
v_sentences <- unlist(sentences)
nsentences <- length(v_sentences) #626
sum(v_sentences=="") #1

#v_sentences <- v_sentences[-which(v_sentences=="")]
histSentences <- hist(nchar(v_sentences), 
     main = "Sentence size", 
     xlab = "Size",
     ylab = "Times"
     )

# Number of tokens

n_tokens <- spacy_tokenize(paras)
v_tokens <- unlist(n_tokens) 
v_tokens[1:10]
length(v_tokens) 
length(unique(v_tokens)) 
tokens_no_punct <- spacy_tokenize(paras, remove_punct = TRUE)
v_tokens_no_punct <- unlist(tokens_no_punct)
length(v_tokens_no_punct)
length(unique(v_tokens_no_punct))

tableTokens <- head(sort(table(v_tokens), decreasing = TRUE), n = 20)
tableNPTokens <- head(sort(table(v_tokens_no_punct), decreasing = TRUE), n = 20)
tableLeastTokens <- head(sort(table(v_tokens), decreasing = FALSE), n = 20)
tableLeastNPTokens <- head(sort(table(v_tokens_no_punct), decreasing = FALSE), n = 20)

# Token plots 

plot(tableTokens,
     xlab = "Tokens",
     ylab = "Times",
     main = "Most tokens with punctuation")

plot(tableNPTokens,
     xlab = "Tokens",
     ylab = "Times",
     main = "Most tokens without punctuation")

plot(tableLeastTokens,
     xlab = "Tokens",
     ylab = "Times",
     main = "Least tokens with punctuation")

plot(tableLeastNPTokens,
     xlab = "Tokens",
     ylab = "Times",
     main = "Least tokens without punctuation")

spacy_finalize()

# Chapter analysis

grep(pattern = "Tratado ", linesQ, fixed = TRUE)
linesQ[1:20]

cap1 <- c(linesQ[1:497])
cap2 <- c(linesQ[498:903])
cap3 <- c(linesQ[904:1581])
cap4 <- c(linesQ[1582:1599])
cap5 <- c(linesQ[1600:1915])
cap6 <- c(linesQ[1916:1944])
cap7 <- c(linesQ[1944:length(linesQ)])

caps <- list()
caps[[1]] <- c(cap1)
caps[[2]] <- c(cap2)
caps[[3]] <- c(cap3)
caps[[4]] <- c(cap4)
caps[[5]] <- c(cap5)
caps[[6]] <- c(cap6)
caps[[7]] <- c(cap7)

# Regular halves

caps1st <- paste(unlist(caps[1:3]))
caps2nd <- paste(unlist(caps[4:7]))

model <- bpe(unlist(caps[1:3]))
subtoks2 <- bpe_encode(model, x = caps2nd, type = "subwords")
head(unlist(subtoks2), n = 50)

# Fair halves

caps1stfair <- paste(unlist(caps[1:2]))
caps2ndfair <- paste(unlist(caps[3:7]))

model <- bpe(unlist(caps[1:2]))
subtoksfair <- bpe_encode(model, x = caps2ndfair, type = "subwords")
head(unlist(subtoksfair), n = 50)

model2 <- bpe(unlist(linesQ))
subtoks3 <- bpe_encode(model2, x = caps2ndfair, type = "subwords")
head(unlist(subtoks3), n = 50)

# Corpus definition

texts_lines <- unlist(linesQ)
names(texts_lines) <- paste("Línea ", 1:length(texts_lines))
corpus_lines <- corpus(texts_lines)
docvars(corpus_lines, field = "Línea") <- 1:length(texts_lines)
corpus_lines

# Dendogram setup

dfm_lines <- dfm(tokens(corpus_lines),)
distMatrixLines <- dist(as.matrix(dfm_lines))
groups <- hclust(distMatrixLines, method="ward.D")

plot(groups,
     cex = 0.25,
     hang = -1,
     xlab = "",
     ylab = "",
     main = "Dendogram")
rect.hclust(groups, k = 5, border = 1:5)

topfeatures(dfm_lines)
dfm_lines_NP <- dfm(tokens(corpus_lines, remove_punct = TRUE), )
dfm_lines_NP2 <- dfm_remove(dfm_lines_NP, stopwords("es"))
topfeatures(dfm_lines_NP2)
topfeatures(dfm_lines_NP2, decreasing = FALSE)

# Using docvars with fair halves
corpus_pt1 <- corpus_subset(corpus_lines,
                            Línea < 904) #Chapters 1-2
corpus_pt2 <- corpus_subset(corpus_lines,
                            Línea > 903) #Chapters 3-7

# First half vs second half
dfm_pt1NP <- dfm(tokens(corpus_pt1, remove_punct = TRUE))
dfm_pt2NP <- dfm(tokens(corpus_pt2, remove_punct = TRUE))

dfm_pt1NP <- dfm_remove(dfm_pt1NP, stopwords("es"))
dfm_pt2NP <- dfm_remove(dfm_pt2NP, stopwords("es"))

topfeatures(dfm_pt1NP)
topfeatures(dfm_pt2NP)
topfeatures(dfm_pt1NP, decreasing = FALSE)
topfeatures(dfm_pt2NP, decreasing = FALSE)

# Chapter vs chapter
corpus_ch1 <- corpus_subset(corpus_lines,
                            Línea < 498) #Chapter 1
corpus_ch2 <- corpus_subset(corpus_lines,
                            ((497 < Línea) & (Línea < 904))) #Chapter 2
corpus_ch3 <- corpus_subset(corpus_lines,
                            ((903 < Línea) & (Línea < 1581))) #Chapter 3
corpus_ch4 <- corpus_subset(corpus_lines,
                            ((1580 < Línea) & (Línea < 1600))) #Chapter 4
corpus_ch5 <- corpus_subset(corpus_lines,
                            ((1599 < Línea) & (Línea < 1916))) #Chapter 5
corpus_ch6 <- corpus_subset(corpus_lines,
                            ((1915 < Línea) & (Línea < 1944))) #Chapter 6
corpus_ch7 <- corpus_subset(corpus_lines,
                            1943 < Línea) #Chapter 7

dfm_ch1NP <- dfm(tokens(corpus_ch1, remove_punct = TRUE))
dfm_ch2NP <- dfm(tokens(corpus_ch2, remove_punct = TRUE))
dfm_ch3NP <- dfm(tokens(corpus_ch3, remove_punct = TRUE))
dfm_ch4NP <- dfm(tokens(corpus_ch4, remove_punct = TRUE))
dfm_ch5NP <- dfm(tokens(corpus_ch5, remove_punct = TRUE))
dfm_ch6NP <- dfm(tokens(corpus_ch6, remove_punct = TRUE))
dfm_ch7NP <- dfm(tokens(corpus_ch7, remove_punct = TRUE))

dfm_ch1NP <- dfm_remove(dfm_ch1NP, stopwords("es"))
dfm_ch2NP <- dfm_remove(dfm_ch2NP, stopwords("es"))
dfm_ch3NP <- dfm_remove(dfm_ch3NP, stopwords("es"))
dfm_ch4NP <- dfm_remove(dfm_ch4NP, stopwords("es"))
dfm_ch5NP <- dfm_remove(dfm_ch5NP, stopwords("es"))
dfm_ch6NP <- dfm_remove(dfm_ch6NP, stopwords("es"))
dfm_ch7NP <- dfm_remove(dfm_ch7NP, stopwords("es"))

topfeatures(dfm_ch1NP)
topfeatures(dfm_ch2NP)
topfeatures(dfm_ch3NP)
topfeatures(dfm_ch4NP)
topfeatures(dfm_ch5NP)
topfeatures(dfm_ch6NP)
topfeatures(dfm_ch7NP)

# Prologue vs book
grep(pattern = "Prólogo", lines, fixed = TRUE)
grep(pattern = "Tratado", lines, fixed = TRUE)
linesR <- lines[52:97]
linesR

texts_lines_prolog <- unlist(linesR)
names(texts_lines_prolog) <- paste("Línea ", 1:length(texts_lines_prolog))
corpus_lines_prolog <- corpus(texts_lines_prolog)
docvars(corpus_lines_prolog, field = "Línea") <- 1:length(texts_lines_prolog)
corpus_lines_prolog

dfm_prolog_NP <- dfm(tokens(corpus_lines_prolog, remove_punct = TRUE))
dfm_prolog_NP <- dfm_remove(dfm_prolog_NP, stopwords("es"))
topfeatures(dfm_prolog_NP)
topfeatures(dfm_prolog_NP, decreasing = FALSE)
