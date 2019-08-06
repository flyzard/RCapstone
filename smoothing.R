unigrams <- readRDS("unigram.rds")
bigrams <- readRDS("bigram.rds")
trigrams <- readRDS("trigram.rds")
quadgrams <- readRDS("quadgram.rds")

quadgrams

##########################################################
#------------Calculating ngram probabilities-------------#
##########################################################
setkey(bigrams, word_1)
setkey(unigrams, word_1)
bigrams[,c_uni := unigrams[word_1, count]]
bigrams[,prob := count / c_uni]

setkeyv(trigrams, c("word_1", "word_2"))
setkeyv(bigrams, c("word_1", "word_2"))
trigrams[,c_bi := bigrams[.(word_1, word_2), count]]
bigrams[,prob := count / c_uni]



discount_value <- 0.75

n_bigrams <- nrow(bigrams)
ckn <- bigrams[, .(Prob = ((.N) / n_bigrams)), by = word_2]
setkey(ckn, word_2)
unigrams[, Prob := ckn[word_1, Prob]]
unigrams <- unigrams[!is.na(unigrams$Prob)]


n1wi <- bigrams[, .(N = .N), by = word_1]
setkey(n1wi, word_1)

setkey(bigrams, word_1)
setkey(unigrams, word_1)
bigrams[unigrams[word_1] == word_1]


bigrams[, Prob := ((count - discount_value) / Cn1 + discount_value / Cn1 * n1wi[word_1, N] * unigrams[word_2, Prob])]

bigrams <- bigrams[!is.na(bigrams$Prob)]





setkey(bigrams, .(word_1, word_2))

# Finding count of word1-word2 combination in bigram 
trigrams[, Cn2 := bigrams[, count, by=.(word_1, word_2)]$count]

# Finding count of word1-word2 combination in trigram
n1w12 <- tri_words[, .N, by = .(word_1, word_2)]
setkey(n1w12, word_1, word_2)

# Kneser Kney Algorithm
tri_words[, Prob := (count - discount_value) / Cn2 + discount_value / Cn2 * n1w12[.(word_1, word_2), N] *
            bi_words[.(word_1, word_2), Prob]]


