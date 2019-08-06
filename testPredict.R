## Test prediction based on the validation set
source("./algorithms.R")
library(stringi)
library(quanteda)
library(dplyr)

# unigrams <- readRDS("final_unigrams.rds")
# bigrams <- readRDS("final_bigrams.rds")
# trigrams <- readRDS("final_trigrams.rds")
# quadgrams <- readRDS("final_quadgrams.rds")


# Open file connection
validationSet <- readRDS("validationSet.rds")

len <- length(validationSet)
sampledSet <- validationSet[sample(len, len * 0.0005)]
rm(validationSet, len)
gc()

test.model <- function(sampleText, FUN = basic.predict.word) {
  matched <- 0
  totalWords <- 0
  t <- NULL
  t = rbind(t, data.frame("sentence", "predicted", "word", stringsAsFactors = FALSE))
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
          
          if (i > 0) {
            
            predicted <- FUN(s, 1)
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
system.time({ res <- test.model(sampledSet) })
head(res, 1000)

results <- quadgrams[word_1 == word1 & word_2 == word2 & word_3 == word3][order(-prob)]$word_4

restri <- trigrams[word_1 == word2 & word_2 == word3][order(-prob)]$word_3

resbi <- bigrams[word_1 == word3][order(-prob)]$word_2


res3 <- as.data.table(res)

res4 <- as.data.table(res)
colnames(res4)<- c("sent", "pred", "word")
colnames(res3) <- c("sent", "pred", "word")

setkey(res4, sent)
setkey(res3, sent)

quadgrams[word_1 == "" & word_2 == "", word_3 == ""]

diff <- as.data.frame(res4[res3][pred != i.pred & i.pred == word, .(sent, pred, word)])

for (row in 1:nrow(diff)) {
  context <- context.get(diff[row, "sent"])
  print(quadgrams[word_1 == context$context[1] & word_2 == context$context[2] & word_3 == context$context[3]])
  print(trigrams[word_1 == context$context[2] & word_2 == context$context[3]])
}

basic.predict.word("ceasar meets asdfs")
