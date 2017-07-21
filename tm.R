# Libraries
require("NLP")
require("tm")
require("cluster")
require("wordcloud")

# Read in notes file into String object
dir <- "/Users/stephenjohnson/Documents/NLP/"
base <- "notes.txt"
file <- paste0(dir,base)
lines <- scan(file=file,what="char",sep="\n",blank.lines.skip=FALSE,encoding="latin1")
text <-  paste(lines,collapse="\n") 
text <- tolower(text) # Convert to lower case
text <- String(text)

# Split notes at each blank line, and create virtual corpus
parags <- blankline_tokenizer(text)
notes <- text[parags]
corpus <- VCorpus(VectorSource(notes))

# Remove extra whitespace 
corpus <- tm_map(corpus, stripWhitespace)
# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
# Remove numbers
corpus <- tm_map(corpus, removeNumbers)
# Remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Reduce words to stems
corpus <- tm_map(corpus, stemDocument)

# Look at a document in the corpus
writeLines(as.character(corpus[[1]]))

# Create document-term matrix 
dtm <- DocumentTermMatrix(corpus)
m <- as.matrix(dtm)
m[1:10,1:10]

# Choose a word and look at associated words (you may need to change the limit)
term <- "head"
limit <- 0.6
findAssocs(dtm,term,limit)

# Remove sparse terms from matrix
sparsity <- 0.6
dtm2 <- removeSparseTerms(dtm, sparsity)
m2 <- as.matrix(dtm2)
m2[1:10,1:10]

# Cluster the terms, using a distance method and an agglomeration method
dmethod = "euclidean"
dmethod = "manhattan"
dmethod = "minkowski"
dmethod = "euclidean"
d <- dist(t(dtm2),method=dmethod)
cmethod = "ward.D2"
cmethod = "centroid"
cmethod = "average"
cmethod = "complete"
cmethod = "single"
cmethod = "median"
cmethod = "ward.D"
c <- hclust(d=d,method=cmethod)
plot(c)

# Create a term-term matrix
ttm <- t(m) %*% m
ttm[1:10,1:10]

# Examine frequencies of related words (may wish to drop most frequent words)
freq <- sort(ttm[term,],decreasing = TRUE)
freq <- freq[-1]

# Examine the word cloud for the term (can change colors and number of words)
p <- brewer.pal(9,"BrBG")
p <- brewer.pal(9,"YlOrRd")
wordcloud(names(freq),freq,max.words=50,colors=p)




