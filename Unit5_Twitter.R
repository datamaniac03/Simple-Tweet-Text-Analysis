# Read in the data
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

# Create dependent variable
tweets$Negative = as.factor(tweets$Avg <= -1)

# Install new packages
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# Create corpus
corpus = Corpus(VectorSource(tweets$Tweet))

# Convert to lower-case
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)

# Remove punctuation
corpus = tm_map(corpus, removePunctuation)

# Remove stopwords and apple
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

# Stem document 
corpus = tm_map(corpus, stemDocument)


# Create document term matrix
dtm = DocumentTermMatrix(corpus)

# Look at matrix 
inspect(dtm[1000:1005,505:515])

# Check for sparsity
findFreqTerms(dtm, lowfreq=20)

# Remove sparse terms
sparse = removeSparseTerms(dtm, 0.99)

# Convert to a data frame
tweetsSparse = as.data.frame(as.matrix(sparse), row.names = F)


#### Visualization #####
# Install packages for visualization
library(RColorBrewer)
library(wordcloud)

# Get data
freq <- sort(colSums(tweetsSparse), TRUE)
word <- names(freq)

# Prepare palette
color <- brewer.pal(10, "RdYlGn")

# Top 10 frequent words overall
barplot(freq[10:1], horiz = T, las = 1, col = brewer.pal(10, "RdYlGn"))

# Word Cloud - overall
wordcloud(words = word[2:length(word)],
          freq = freq[2:length(freq)],
          scale = c(4,.5), 
          colors = colorssss, 
          random.order = F, 
          random.color = F)



# Positive vs. Negative
tweetsSparse$Avg <- tweets$Avg

# Neutral
freq.neu <- sort(colSums(tweetsSparse[abs(tweetsSparse$Avg) < .4,
                                      1:ncol(tweetsSparse)-1]),
                 TRUE)
word.neu <- names(freq.neu)

barplot(freq.neu[10:1], horiz = T, las = 1, col = brewer.pal(10, "RdYlGn"))

color <- c(brewer.pal(6, "Accent")[1:3],brewer.pal(6, "Accent")[5:6])

wordcloud(words = word.neu,
          freq = freq.neu,
          scale = c(4, .5), 
          colors = color, 
          random.order = F, 
          random.color = F)

# Positive
freq.pos <- sort(colSums(tweetsSparse[tweetsSparse$Avg>.4,
                                      1:ncol(tweetsSparse)-1]),
                 TRUE)
word.pos <- names(freq.pos)

barplot(freq.pos[10:1], horiz = T, las = 1, col = brewer.pal(10, "RdYlGn"))

wordcloud(words = word.pos,
          freq = freq.pos,
          scale = c(4,1), 
          colors = brewer.pal(6, "Accent"), 
          random.order = F, 
          random.color = F)

# Negative
freq.neg <- sort(colSums(tweetsSparse[tweetsSparse$Avg< -.4,
                                      1:ncol(tweetsSparse)-1]),
                 TRUE)
word.neg <- names(freq.neg)

barplot(freq.neg[10:1], horiz = T, las = 1, col = brewer.pal(10, "RdYlGn"))

color <- c(brewer.pal(6, "Accent")[1:3],brewer.pal(6, "Accent")[5:6])

wordcloud(words = word.neg,
          freq = freq.neg,
          scale = c(4,.8), 
          colors = color, 
          random.order = F, 
          random.color = F)