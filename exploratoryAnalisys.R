unigrams <- readRDS("unigram.rds")
bigrams <- readRDS("bigram.rds")
trigrams <- readRDS("trigram.rds")

unigrams <- unigrams[order(-count)]
bigrams <- bigrams[order(-count)]
trigrams <- trigrams[order(-count)]

library(ggplot2)

toGraph <- unigrams[1:20, ]
toGraph$word_1 <- factor(toGraph$word_1, levels = toGraph$word_1)

ggplot(data=toGraph, aes(x=word_1, y=count)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

toGraph <- bigrams[1:20, ]
toGraph$word <- paste(toGraph$word_1, toGraph$word_2)
toGraph$word <- factor(toGraph$word, levels = toGraph$word)

ggplot(data=toGraph, aes(x=word, y=count)) + geom_bar(stat="identity")  +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

trigrams <- trigrams[1:20, ]
trigrams$word <- paste(trigrams$word_1, trigrams$word_2, trigrams$word_3)
trigrams$word <- factor(trigrams$word, levels = trigrams$word)

ggplot(data=trigrams, aes(x=word, y=count)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))


# check what more there is to explore on the other works


# Conclusions

