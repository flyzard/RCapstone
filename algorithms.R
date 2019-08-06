library(quanteda)
library(data.table)
setDTthreads(4)

context.get <- function(text = "") {
  text <- trimws(gsub(".*\\.|.*\\?|.*\\!", "", tolower(as.character(text))))
  words <- char_wordstem(strsplit(x = text, split = "\\s")[[1]])
  n_words <- length(words)
  
  # In case we are in the end of the sentence, provide a empty list
  if (n_words == 0) {
    return(list(
      "length" = 0,
      "context" = c()
    ))
  }
  
  len <- min(n_words, 3)
  return(list(
    "context" = tail(words, 3),
    "length" = len
  ))
}

theThreeMostLikelyWords <- unigrams[order(-prob)][1:3]$word_1

searchUnigram <- function(qty = 3) {
  theThreeMostLikelyWords[1]
}

searchBigram <- function(word, qty = 3) {
  qty <- as.numeric(qty)
  word <- as.character(word)
  results <- bigrams[word_1 == word][order(-prob)]$word_2
  if (length(results[!is.na(results)]) > 0) return(results[!is.na(results)][1])

  return(searchUnigram())
}

searchTrigram <- function(word1, word2, qty = 1) {
  qty <- as.numeric(qty)
  word1 <- as.character(word1)
  word2 <- as.character(word2)

  results <- trigrams[word_1 == word1 & word_2 == word2][order(-prob)]$word_3
  if (length(results[!is.na(results)]) > 0) return(results[!is.na(results)][1])
  
  results <- bigrams[word_1 == word2][order(-prob)]$word_2
  if (length(results[!is.na(results)]) > 0) return(results[!is.na(results)][1])

  return(searchUnigram())
}

searchQuadgram <- function(word1, word2, word3, qty = 1) {
  qty <- as.numeric(qty)
  word1 <- as.character(word1)
  word2 <- as.character(word2)
  word3 <- as.character(word3)
  
  results <- quadgrams[word_1 == word1 & word_2 == word2 & word_3 == word3][order(-prob)]$word_4
  if (length(results[!is.na(results)]) > 0) return(results[!is.na(results)][1])

  results <- trigrams[word_1 == word2 & word_2 == word3][order(-prob)]$word_3
  if (length(results[!is.na(results)]) > 0) return(results[!is.na(results)][1])
  
  results <- bigrams[word_1 == word3][order(-prob)]$word_2
  if (length(results[!is.na(results)]) > 0) return(results[!is.na(results)][1])
  
  return(searchUnigram())
}

basic.predict.word <- function(sentence = "") {

  # Get context - Get the the full sentence
  context <- context.get(sentence)
  
  # If only one word is provided, show the 5 most likely following words in the bigrams
  if (context$length == 0) {
    predictions <- searchUnigram()
  }else if (context$length == 1) {
    predictions <- searchBigram(context$context[1])
  } else if (context$length == 2) {
    predictions <- searchTrigram(context$context[1], context$context[2])
  } else if(context$length == 3) {
    #predictions <- searchTrigram(context$context[2], context$context[3])
    predictions <- searchQuadgram(context$context[1], context$context[2], context$context[3])
  }
  predictions
}

basic.predict.word("ceasar meets asdfs")





### Calculate probabilities based on kneser ney algorithm
## For unigram:
# Calculate nrow for bigrams

# Calculate the number of entries in bigrams that start with each word in the unigrams