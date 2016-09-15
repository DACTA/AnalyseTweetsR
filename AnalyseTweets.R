### Récupération des tweets :

library(twitteR)

setup_twitter_oauth(consumer_key = "XXX", 
                    consumer_secret = "XXX", 
                    access_token = "XXX", 
                    access_secret = "XXX")


tweets <- searchTwitter("facebook", n = 1000, lang = "fr", resultType = "mixed", since = "2016-09-14")

tweets_df <- twListToDF(tweets) # cette fonction va permettre de transformer la liste de tweets extraite en un "dataframe"


write.csv2(tweets_df, file = "tweets.csv", row.names = FALSE)


### Travail sur les données textuelles :

library(stringi)

tweets_text <- stri_trans_general(tweets_df$text, "lower") # on va ici mettre tous les mots en minuscules

# on crée ci-dessous une fonction R qui va nous permettre de faire un peu de nettoyage de nos données : 
# suppression des liens web et d'une liste de certains mots (contenue dans "mots")

myCleaningFunction <- function(x, mots) {
  
  s <- x
  
  s <- gsub("(https://[^\\s]+)", "", s, perl = TRUE)
  s <- gsub("(http://[^\\s]+)", "", s, perl = TRUE)
  
  for(k in 1:length(mots)) {
    
    s <- gsub(mots[k], "", s, perl = TRUE)
    
  }
  
  return(s)
  
}

mots_supp <- c("facebook", "via", "c'est", "plus", "tous") # vecteur de mots à supprimer

tweets_text <- sapply(tweets_text, myCleaningFunction, mots_supp)
names(tweets_text) <- NULL


# le package "tm" propose un certain nombre de fonctions intéressantes pour le retravail de données textuelles

library(tm)

tweets_corpus <- Corpus(VectorSource(tweets_text))

tweets_corpus <- tm_map(tweets_corpus, removePunctuation) # ici cela va supprimer automatiquement tous les caractères de ponctuation
tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("fr")) # ici cela va supprimer automatiquement une bonne partie des mots français "basiques", tels que "le", "la", etc. mais il manque ! Il manque par exemple "les"...
tweets_corpus <- tm_map(tweets_corpus, stripWhitespace) # ici cela va supprimer automatiquement tous les espaces vides


# Construire la matrice des fréquences :

dtm <- TermDocumentMatrix(tweets_corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v),freq = v)

head(d, 10) 
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Mots les plus fréquents",
        ylab = "Fréquences")


### Restitution des résultats sous forme de nuage de mots (= "wordcloud") :

library(wordcloud)

set.seed(123456)
wordcloud(tweets_corpus, max.words = 20, colors = brewer.pal(8, "Dark2"))

