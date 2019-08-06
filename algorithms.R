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
  theThreeMostLikelyWords[1:qty]
}

searchBigram <- function(word, qty = 3) {

  results <- bigrams[word_1 == word][order(-prob)]$word_2
  results <- results[!is.na(results)]
  n_results <- length(results)
  if (n_results >= qty) return(results[1:qty])

  return(append(results, searchUnigram()))
}

searchTrigram <- function(word1, word2, qty = 3) {
  results <- trigrams[word_1 == word1 & word_2 == word2][order(-prob)]$word_3
  results <- results[!is.na(results)]
  n_results <- length(results)
  if (n_results >= qty) return(results[1:qty])
  
  resbi <- bigrams[word_1 == word2][order(-prob)]$word_2
  results <- append(results, resbi[!is.na(resbi)])
  n_results <- length(results)
  if (n_results >= qty) return(results[1:qty])

  return(append(results, searchUnigram()))
}

searchQuadgram <- function(word1, word2, word3, qty = 3) {
  results <- quadgrams[word_1 == word1 & word_2 == word2 & word_3 == word3][order(-prob)]$word_4
  results <- results[!is.na(results)]
  n_results <- length(results)
  if (n_results >= qty) return(results[1:qty])

  restri <- trigrams[word_1 == word2 & word_2 == word3][order(-prob)]$word_3
  results <- append(results, restri[!is.na(restri)])
  n_results <- length(results)
  if (n_results >= qty) return(results[1:qty])
  
  resbi <- bigrams[word_1 == word3][order(-prob)]$word_2
  results <- append(results, resbi[!is.na(resbi)])
  n_results <- length(results)
  if (n_results >= qty) return(results[1:qty])
  
  return(append(results, searchUnigram()))
}

basic.predict.word <- function(sentence = "", n_predict = 3) {

  # Get context - Get the the full sentence
  context <- context.get(sentence)
  
  # If only one word is provided, show the 5 most likely following words in the bigrams
  if (context$length == 0) {
    predictions <- searchUnigram(n_predict)
  }else if (context$length == 1) {
    predictions <- searchBigram(context$context[1], n_predict)
  } else if (context$length == 2) {
    predictions <- searchTrigram(context$context[1], context$context[2], n_predict)
  } else if(context$length == 3) {
    #predictions <- searchTrigram(context$context[2], context$context[3])
    predictions <- searchQuadgram(context$context[1], context$context[2], context$context[3], n_predict)
  }
  predictions
}
