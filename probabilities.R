# unigrams <- readRDS("raw_unigram.rds")
# bigrams <- readRDS("raw_bigram.rds")
# trigrams <- readRDS("raw_trigram.rds")
# quadgrams <- readRDS("raw_quadgram.rds")

unigrams <- readRDS("stemed_unigram.rds")
bigrams <- readRDS("stemed_bigram.rds")
trigrams <- readRDS("stemed_trigram.rds")
quadgrams <- readRDS("stemed_quadgram.rds")

library(data.table)

# Calculate the log(prob) for each word in the unigrams
unigram_count_total <- unigrams[, sum(count)]
unigrams[, prob := (count / unigram_count_total)]
unigrams[order(-prob)]
unigrams[, count := NULL]

# Calculating probabilities of bigrams
bigrams <- bigrams[word_1 != "" & word_2 != ""]
bigrams[, prob := count / sum(count), by = word_1]
bigrams[, count := NULL]
bigrams <- bigrams[!is.na(prob)]

# Calculating probabilities of trigrams
trigrams <- trigrams[word_1 != "" & word_2 != "" & word_3 != ""]
trigrams[, prob := count / sum(count), by = .(word_1, word_2)]
trigrams[, count := NULL]
trigrams <- trigrams[!is.na(prob)]

# Calculating probabilities of quadgrams
# setkeyv(quadgrams, c("word_1", "word_2", "word_3"))
quadgrams <- quadgrams[word_1 != "" & word_2 != "" & word_3 != "" & word_4 != ""]
quadgrams[, prob := count / sum(count), by = .(word_1, word_2, word_3)]
quadgrams[, count := NULL]
quadgrams <- quadgrams[!is.na(prob)]


saveRDS(unigrams, "prob_unigrams.rds")
saveRDS(bigrams, "prob_bigrams.rds")
saveRDS(trigrams, "prob_trigrams.rds")
saveRDS(quadgrams, "prob_quadgrams.rds")

rm(list = ls())

unigrams <- readRDS("prob_unigrams.rds")
bigrams <- readRDS("prob_bigrams.rds")
trigrams <- readRDS("prob_trigrams.rds")
quadgrams <- readRDS("prob_quadgrams.rds")


#### Pruning models ####
library(dplyr)

bigrams <- bigrams %>%
  group_by(word_1) %>%
  filter(prob == max(prob))
bigrams <- as.data.table(bigrams)
# as.data.frame(bigrams[duplicated(bigrams, by = c("word_1"))][order(word_1),])[1:500,]
# anyDuplicated(bigrams, by = c("word_1"))

trigrams <- trigrams %>%
  group_by(word_1, word_2) %>%
  filter(prob == max(prob))
trigrams <- as.data.table(trigrams)
# trigrams[duplicated(trigrams, by = c("word_1", "word_2"))][order(word_1,word_2)]

quadgrams <- quadgrams %>%
  group_by(word_1, word_2, word_3) %>%
  filter(prob == max(prob))
quadgrams <- as.data.table(quadgrams)
quadgrams[duplicated(quadgrams, by = c("word_1", "word_2", "word_3"))][order(word_1, word_2, word_3)]
quadgrams[word_1 == "00pm" & word_2 == "at" & word_3 == "the"]


#quadgrams[grepl("^([0-9])+", word_1, ignore.case = TRUE)] ## Check the numbers case



# Remove non unfiltered profanity words
profanity <- c("lovemak", "fxckyoupaym", "homoerot", "hardcor", "paedophil", "genit", "orgi", "dildo", "butt", "a2z", "coon", "clitori", "tosser", "lolita", "twat", "swastika", "rapist", "sodomi", "nuditi", "hooker", "octopussi", "milf", "santorum", "handjob", "ejacul", "orgasm", "genit", "motherfuck", "asshol", "slut", "nympho", "pedophil", "cum", "fuck", "blowjob", "jizz", "butthol", "dick", "cunt", "porn", "cock", "negro", "bollock", "boner", "wank", "vagina", "rectum", "nigger", "pube", "shit", "shitti", "boob", "panti", "intercours", "bondag", "pornographi", "tit", "topless", "anus", "sexi", "kinki", "ass", "peni", "porno", "arsehol", "titti", "erot", "sex", "bullshit", "pussi", "bitch", "threesom", "nude", "anal", "playboy", "clit")
unigrams <- unigrams[!(word_1 %in% profanity)]
bigrams <- bigrams[!(word_1 %in% profanity | word_2 %in% profanity)]
trigrams <- trigrams[!(word_1 %in% profanity | word_2 %in% profanity | word_3 %in% profanity)]
quadgrams <- quadgrams[!(word_1 %in% profanity | word_2 %in% profanity | word_3 %in% profanity | word_4 %in% profanity)]















# Remove close to zero probabilities ### Revise this strategy
final_unigrams <- unigrams[order(-prob)][1:100]
final_bigrams <- bigrams[prob > -30.662159]
final_trigrams <- trigrams[prob > -46.15185]
final_quadgrams <- quadgrams[prob > -63]




# ind <- unigrams[!(grep("[A-Za-z]*[0-9]+[A-Za-z]*", word_1) & count > 100)]
# unigrams[-ind]


# Remove all the occurrencies with numbers in word_1 and with count < 100, for not being significative representations
# unigrams <- unigrams[!(grep("[A-Za-z]*[0-9]+[A-Za-z]*", word_1) & count < 100)][order(-count)]
saveRDS(final_unigrams, "final_unigrams.rds")
saveRDS(final_bigrams, "final_bigrams.rds")
saveRDS(final_trigrams, "final_trigrams.rds")
saveRDS(final_quadgrams, "final_quadgrams.rds")
