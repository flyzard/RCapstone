## Test prediction based on the validation set
source("./algorithms.R")
library(stringi)
library(quanteda)
library(dplyr)

unigrams <- readRDS("final_unigrams.rds")
bigrams <- readRDS("final_bigrams.rds")
trigrams <- readRDS("final_trigrams.rds")
quadgrams <- readRDS("final_quadgrams.rds")


# Open file connection
validationSet <- readRDS("validationSet.rds")

len <- length(validationSet)
sampledSet <- validationSet[sample(len, len * 0.0005)]
rm(validationSet, len)
gc()

test.model <- function(sampleText, FUN = basic.predict.word, n = 3) {
  matched <- 0
  totalWords <- 0
  t <- NULL
  t = rbind(t, data.frame("sentence", "predicted", "word", stringsAsFactors = FALSE))
  colnames(t) <- c("sent", "pred", "word")
  setkeyv(quadgrams, c("word_1", "word_2", "word_3"))
  setkeyv(trigrams, c("word_1", "word_2"))
  setkey(bigrams, word_1)
  
  for (paragraph in sampleText) {
    # break sentences in words
    for (sentence in strsplit(paragraph, "(?<=\\.|\\?)\\s(?=[A-Z])", perl = TRUE)[[1]]) {
      new_sentence <- c()
      i <- 0
      s <- ""
      for (word in stri_extract_all_words(sentence)[[1]]) {
          stemed <- char_wordstem(word)
          
          if (i > 0) { # if given at least one word, predict the next one
            predicted <- FUN(s, n)
            if (tolower(word) %in% predicted) {
                matched <- matched+1
            } 
            
            totalWords <- totalWords + 1
            p <- paste(predicted, collapse = ", ")
            t[totalWords,] = c(s, p, word)
          }
          new_sentence <- append(tail(new_sentence, 2), stemed)
          s <- paste(new_sentence, collapse=" ")
          i <- i + 1
      }
    }
  }

  cat("Total words:", totalWords, "\n")
  cat("Correctly predicted words:", matched, "\n")
  cat("result: ", matched / totalWords, "\n")
  
  return(t)
}

# Test the most basic model
system.time({ res <- test.model(sampledSet, n = 1) })
colnames(res) <- c("sent", "pred", "word")
head(res, 1000)

#Check the wrongly predicted duplicated in the 4 gram 
res <- as.data.table(res)
wrong <- as.data.frame(res[pred != tolower(word)])


count <- 0
for (i in 1:nrow(wrong)) {
  context <- context.get(wrong[i, "sent"])
  if (context$length == 3) {
    tri_results <- trigrams[word_1 == context$context[2] & word_2 == context$context[3]][order(-prob)]
    if ((length(quad_results$word_4) > 1) & (wrong[i, "word"] %in% tri_results$word_3[1])) {
      print(wrong[i, "sent"])
      print(wrong[i, "word"])
      print(tri_results[1])
      print(tri_results[1]$prob)
      count <- count+1
    }
    #restri <- trigrams[word_1 == context$context[2] & word_2 == context$context[3]][order(-prob)]$word_3
  }
}



quadgrams[word_1 == "as" & word_2 == "they" & word_3 == "turn"][order(-prob)]
trigrams[word_1 == "they" & word_2 == "turn"][order(-prob)]

quadgrams[word_1 == "get" & word_2 == "it" & word_3 == "into"][order(-prob)]
trigrams[word_1 == "it" & word_2 == "into"][order(-prob)]

