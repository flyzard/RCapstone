library(data.table)

# Load files save on the gettingCleanNGrams.R
unigrams <- readRDS("stemed_unigram.rds")
bigrams <- readRDS("stemed_bigram.rds")
trigrams <- readRDS("stemed_trigram.rds")
quadgrams <- readRDS("stemed_quadgram.rds")

# Remove some unfiltered profanity words that weren't present on the list of the profanity words
profanity <- c("cunnilingus", "dingleberry", "beaner", "beaners", "arsehole", "bitches", "arsehole", "arsehole", "artdealer", "asshole", "autoerotic", "bareback", "barenaked", "bastard", "bbw", "bdsm", "lovemak", "anilingus", "apeshit", "fxckyoupaym", "homoerot", "hardcor", "paedophil", "genit", "orgi", "dildo", "butt", "a2z", "coon", "clitori", "tosser", "lolita", "twat", "swastika", "rapist", "sodomi", "nuditi", "hooker", "octopussi", "milf", "santorum", "handjob", "ejacul", "orgasm", "genit", "motherfuck", "asshol", "slut", "nympho", "pedophil", "cum", "fuck", "blowjob", "jizz", "butthol", "dick", "cunt", "porn", "cock", "negro", "bollock", "boner", "wank", "vagina", "rectum", "nigger", "pube", "shit", "shitti", "boob", "panti", "intercours", "bondag", "pornographi", "tit", "topless", "anus", "sexi", "kinki", "ass", "peni", "porno", "arsehol", "titti", "erot", "sex", "bullshit", "pussi", "bitch", "threesom", "nude", "anal", "playboy", "clit")
unigrams <- unigrams[!(word_1 %in% profanity)]
bigrams <- bigrams[!(word_1 %in% profanity | word_2 %in% profanity)]
trigrams <- trigrams[!(word_1 %in% profanity | word_2 %in% profanity | word_3 %in% profanity)]
quadgrams <- quadgrams[!(word_1 %in% profanity | word_2 %in% profanity | word_3 %in% profanity | word_4 %in% profanity)]

######################################################
# Calculate maximum likelihood estimation in n-grams #
######################################################
unigram_count_total <- unigrams[, sum(count)]
unigrams[, prob := (count / unigram_count_total)]
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
quadgrams <- quadgrams[word_1 != "" & word_2 != "" & word_3 != "" & word_4 != ""]
quadgrams[, prob := count / sum(count), by = .(word_1, word_2, word_3)]
quadgrams[, count := NULL]
quadgrams <- quadgrams[!is.na(prob)]

#### Pruning models ####
############################################################################
# Differentiate probabilities summing 5% of the likelihood of the n-1 gram #
############################################################################
setkey(bigrams, word_2)
setkey(unigrams, word_1)
## Try to add some more probabilitie to the ones more likely to occurr to distinguish from the less likly
bigrams_t <- bigrams[unigrams, prob_t := (prob + (i.prob * 0.05))]
bigrams <- bigrams_t[!is.na(prob_t)]
bigrams[, prob := log(prob)]
bigrams[, prob_t := NULL]
bigrams <- bigrams[prob != 0]


setkeyv(bigrams, c("word_1", "word_2"))
setkeyv(trigrams, c("word_2", "word_3"))
trigrams_t <- trigrams[bigrams, prob_t := (prob + (i.prob * 0.05))]
trigrams <- trigrams_t[!is.na(prob_t)]
trigrams[, prob := log(prob)]
trigrams[, prob_t := NULL]
trigrams <- trigrams[prob != 0]


setkeyv(quadgrams, c("word_2", "word_3", "word_4"))
setkeyv(trigrams, c("word_1", "word_2", "word_3"))
quadgrams_t <- quadgrams[trigrams, prob_t := (prob + (i.prob * 0.05))]
quadgrams <- quadgrams_t[!is.na(prob_t)]
quadgrams[, prob := log(prob)]
quadgrams[, prob_t := NULL]
quadgrams <- quadgrams[prob != 0]

# Once we will be predicting at maximum 3 words, we'll remove the other less likely ngrmas
final_bigrams <- bigrams[order(-prob)][, head(.SD, 3), by = word_1]
final_trigrams <- trigrams[order(-prob)][, head(.SD, 3), by = .(word_1, word_2)]
final_quadgrams <- quadgrams[order(-prob)][, head(.SD, 3), by = .(word_1, word_2, word_3)]

# We don't need all of the words in the unigram for perdiction
final_unigrams <- unigrams[order(-prob)][1:100]

# Remove all the occurrencies with numbers in word_1 and with count < 100, for not being significative representations
# unigrams <- unigrams[!(grep("[A-Za-z]*[0-9]+[A-Za-z]*", word_1) & count < 100)][order(-count)]
saveRDS(final_unigrams, "final_unigrams.rds")
saveRDS(final_bigrams, "final_bigrams.rds")
saveRDS(final_trigrams, "final_trigrams.rds")
saveRDS(final_quadgrams, "final_quadgrams.rds")


# final_trigrams[, prev := paste(word_1, word_2, sep = "")]
# final_trigrams[, word_2 := NULL]
# final_trigrams[, word_1 := NULL]
# saveRDS(final_quadgrams, "ligth_trigrams.rds")

# final_quadgrams[, prev := paste(word_1, word_2, word_3, sep = "")]
# final_quadgrams[, word_3 := NULL]
# final_quadgrams[, word_2 := NULL]
# final_quadgrams[, word_1 := NULL]
# saveRDS(final_quadgrams, "ligth_quadgrams.rds")