set.seed(1111)
library(data.table)
library(quanteda)
quanteda_options(threads = 4)

# Get profanity words
getProfanityWords <- function() {
  profanity_words_url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
  profanity <- read.csv(profanity_words_url, header = FALSE, stringsAsFactors = FALSE)
  profanity$V1
}

textToTokens <- function(trainningSet) {
  # Create a corpus from the training set text
  corp <- corpus(trainningSet)

  # separate into sentences because the context of is important. Not worth to predict a word after two words and a period...
  corp_sent <- corpus_reshape(corp, to = 'sentences', use_docvars = FALSE)

  ## Tokenising words, removing punctuation, twitter specifics, numbers, hyphens, symbols, URLs and separators
  toks <- tokens(
    tolower(corp_sent),
    "word",
    remove_punct = TRUE,
    remove_twitter = TRUE,
    remove_numbers = TRUE,
    remove_hyphens = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE,
    remove_separators = TRUE,
    include_docvars = FALSE
  )

  return(toks)
}

# Read trainnigSet from file, previous stored with code in extractText.R
trainningSet <- readRDS("trainningSet.rds")
# Create tokens from the text, stemed when possible
# stemed_words <- textToStemedWords(trainningSet)
words <- textToTokens(trainningSet)

# Save the words (textToStemedWords it's a expensive process that we don't want to repeat to often)
#saveRDS(stemed_words, "stemed_words.rds")
saveRDS(words, "all_words.rds")

# Clean memory
rm(list = ls())
gc()
words <- readRDS("all_words.rds")

# Get Tokenised setemed words
# stemed_words <- readRDS("all_words.rds")

# For bi-grams
bi_gram <- tokens_ngrams(words, n = 2)
# # For tri-grams
tri_gram <- tokens_ngrams(words, n = 3)
## To Be continued
quad_gram <- tokens_ngrams(words, n = 4)

# Profanity filtering ####
profanity <- getProfanityWords()

# Remove profanity from all n-grams
uni_DFM <- dfm(words, remove = profanity)
bi_DFM <- dfm(bi_gram, remove = profanity)
tri_DFM <- dfm(tri_gram, remove = profanity)
quad_DFM <- dfm(quad_gram, remove = profanity)

# Create named vectors with counts of words 
sums_U <- colSums(uni_DFM)
sums_B <- colSums(bi_DFM)
sums_T <- colSums(tri_DFM)
sums_Q <- colSums(quad_DFM)
rm(words, bi_gram, tri_gram, quad_gram, profanity, uni_DFM, bi_DFM, tri_DFM, quad_DFM)
# gc()

# Create data table with word -> count for unigrams
uni_grams <- data.table(word_1 = names(sums_U), count = sums_U)

# Create data table with word_1, word-2 -> count for bigrams
bi_grams <- data.table(
  word_1 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 2),
  count = sums_B)

# Create data table with word_1, word-2, word_3 -> count for trigrams
tri_grams <- data.table(
  word_1 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 3),
  count = sums_T)

quad_grams <- data.table(
  word_1 = sapply(strsplit(names(sums_Q), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_Q), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_Q), "_", fixed = TRUE), '[[', 3),
  word_4 = sapply(strsplit(names(sums_Q), "_", fixed = TRUE), '[[', 4),
  count = sums_Q)

# Saving n-grams to files
saveRDS(uni_grams, "raw_unigram.rds")
saveRDS(bi_grams, "raw_bigram.rds")
saveRDS(tri_grams, "raw_trigram.rds")
saveRDS(quad_grams, "raw_quadgram.rds")

# Cleaning workspace and memory

rm(sums_U, sums_B, sums_T, sums_Q)
gc()

#unigram_stemed <- uni_grams[, word_1 := char_wordstem(word_1)]

bigram_stemed <- bi_grams[, word_1 := char_wordstem(word_1)]

trigram_stemed <- tri_grams[, word_1 := char_wordstem(word_1)]
trigram_stemed <- trigram_stemed[, word_2 := char_wordstem(word_2)]

quadgram_stemed <- quad_grams[, word_1 := char_wordstem(word_1)]
quadgram_stemed <- quadgram_stemed[, word_2 := char_wordstem(word_2)]
quadgram_stemed <- quadgram_stemed[, word_3 := char_wordstem(word_3)]

# Remove the duplication from the steming
#unigrams <- unigram_stemed[, .(count = sum(count)), by = word_1]
bigrams <- bigram_stemed[, .(count = sum(count)), by = c("word_1", "word_2")]
trigrams <- trigram_stemed[, .(count = sum(count)), by = c("word_1", "word_2", "word_3")]
quadgrams <- quadgram_stemed[, .(count = sum(count)), by = c("word_1", "word_2", "word_3", "word_4")]

saveRDS(unigrams, "stemed_unigram.rds")
saveRDS(bigrams, "stemed_bigram.rds")
saveRDS(trigrams, "stemed_trigram.rds")
saveRDS(quadgrams, "stemed_quadgram.rds")

rm(list = ls())
gc()