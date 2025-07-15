## NLP CREST doctoral course, 11/2023
## Companion script to lecture NLP2 - Word embeddings
## Fun with word embeddings: similar words, vector semantics, PCA representation
## Julien Boelaert

## First download and extract the fastText pretrained embeddings for english: 
## https://dl.fbaipublicfiles.com/fasttext/vectors-crawl/cc.en.300.vec.gz

## Import and prepare the embeddings matrix
ft <- data.table::fread("cc.en.300.vec", quote ="", skip = 1, data.table = F, 
                        nrows = 5e5) # loading the full 2e6 words takes a lot of RAM
names <- ft$V1
ft <- as.matrix(ft[, -1])
rownames(ft) <- names

## Function to get best matches
norms <- sqrt(rowSums(ft^2))
bestMatches <- function(x, n = 5) {
  cosine <- (ft %*% as.matrix(x)) / (norms * sqrt(sum(x^2)))
  best_n <- order(cosine, decreasing = T)[1:n]
  data.frame(word = names[best_n], cosine = cosine[best_n])
}
gc()


###################
## List best matches for a given word

bestMatches(ft["France", ], 10)

bestMatches(ft["Marx", ])
bestMatches(ft["Durkheim", ])
bestMatches(ft["Bourdieu", ])

bestMatches(ft["Julien", ])
bestMatches(ft["Etienne", ])
bestMatches(ft["Felix", ])


###################
## Semantic algebra

bestMatches(ft["king", ] - ft["man", ] + ft["woman", ])

bestMatches(ft["Paris", ] - ft["France", ] + ft["Spain", ])

bestMatches(ft["match", ] - ft["walk", ] + ft["walking", ])


###################
## PCA representation of a selection of queries

library(ggplot2)

quer <- c("Bach", "Mozart", "Haendel", "Verdi", "Bizet", "Poulenc", "Debussy", 
          "Tupac", "Eminem", "Wu-Tang", 
          "Coltrane", "Miles", "Armstrong", "Ellington", "Dolphy", "Hawkins")

quer <- c("Julien", "Etienne", "Felix", 
          "Marie", "Anne", "Camille", 
          "Panagiotis", "Nikos", "Vassilis",
          "Maria", "Eugenia", "Myrto",
          "Khaled", "Karim", "Abdellatif",
          "Khalida", "Karima", "Aminata", 
          "Gunther", "Gunnar", "Anders", 
          "Greta", "Ursula", "Helga")

quer <- c("economist", "sociologist", "psychologist", "anthropologist", 
          "historian", "geographer", "archeologist", "theologist")


pca <- prcomp(ft[quer, ])
ggplot(data.frame(pca$x, label = quer), aes(PC1, PC2, label = label)) + geom_label() 



###################
## Scoring a word on differences in cosine similarities to two poles 
## (here male / female)

cosine_word <- function(word1, word2) {
  sum(ft[word1, ] * ft[word2, ]) / 
    (sqrt(sum(ft[word1, ]^2)) * sqrt(sum(ft[word2, ]^2)))
}

cosine_vec <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}


pole1 <- c("man", "father", "brother", "he", "him", "gentleman",
           "boy", "dude", "sir", "male")
pole2 <- c("woman", "lady", "girl", "women", "female", "mother", "sister", 
           "she", "her", "gal")

avg_pole1 <- colMeans(ft[pole1, ])
avg_pole2 <- colMeans(ft[pole2, ])

cosine_vec(avg_pole1, avg_pole2)

cosine_dif <- function(vec) {
  cosine_vec(vec, avg_pole1) - cosine_vec(vec, avg_pole2)
}

cosine_dif(ft["Paris", ])
cosine_dif(ft["doctor", ])
cosine_dif(ft["nurse", ])
cosine_dif(ft["family", ])
cosine_dif(ft["strong", ])
cosine_dif(ft["beautiful", ])
