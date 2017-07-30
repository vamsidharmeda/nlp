
dir <- "/Users/vamsi"
base <- "notes.txt"
file <- paste0(dir,base)
fileLines <- scan(file=file,what="char",sep="\n",blank.lines.skip=FALSE)
wholeText <- String( paste(fileLines,collapse="\n") )
paragraphs <- blankline_tokenizer(wholeText)
rawCorpus <- wholeText[paragraphs]
sent_tokenizer <- Regexp_Tokenizer("\\.",invert=TRUE)
sent_token_annotator <- Simple_Sent_Token_Annotator(sent_tokenizer)
word_tokenizer <- Regexp_Tokenizer("[A-Za-z]+|[0-9]+|[^A-Za-z0-9 \n\t]+")
word_token_annotator <- Simple_Word_Token_Annotator(word_tokenizer)

annotateDoc <- function(doc) {
  a1 <- annotate(doc, sent_token_annotator)
  a2 <- annotate(doc, word_token_annotator, a1)
  AnnotatedPlainTextDocument(doc,a2)
}

corpus <- lapply(rawCorpus,annotateDoc)

# Use corpora.R to create a corpus from the progress notes.
# Create a list of tokens using the words function.
tokens <- unlist(lapply(corpus,function (x) words(x)))
# Select alphabetic words using grep.
words <- grep("^[a-z]+$",tolower(tokens),value=TRUE)
# Create a spectrum object.
notes.spc <- vec2spc(words)
plot(notes.spc)
# Create a vocabuary growth object.
notes.vgc <- vec2vgc(words,m.max=1)
# Plot the vocabularly and the hapax legomena.
plot(notes.vgc,add.m=1)

# Load the spectrum for the 100K Brown corpus.
data("Brown100k.spc")
# Fit an LRNE model to the Brown data.
Brown.gigp <- lnre("gigp",Brown100k.spc)
# Project growth over 100 intervals.
Brown.gigp.vgc <- lnre.vgc(Brown.gigp,1:100*1000)
# Plot the growth.
plot(Brown.gigp.vgc)

# Fit an LRNE model to the notes data.
notes.gigp <- lnre("gigp",notes.spc)
# Project growth over 100 intervals.
notes.gigp.vgc <- lnre.vgc(notes.gigp,1:100*(N(notes.spc)/100))
# Compare observed growth to expected growth.
plot(notes.vgc,notes.gigp.vgc,legend=c("observed","expected"))

# Project growth to size of Brown sample
notes.gigp.vgc <- lnre.vgc(notes.gigp,1:100*1000)
# Compare vocabulary growth
plot(notes.gigp.vgc,Brown.gigp.vgc,legend=c("notes","Brown"))

# Estimate spectrum for notes at 100000 words
notes.gigp.spc <- lnre.spc(notes.gigp,100000)
# Choose cut off frequency for rare words
m <- 5
# Determine number of tokens that have these frequencies
tokens.rare <- sum(Vm(notes.gigp.spc,1:m)*c(1:m))
# Determine what percent of the tokens in the corpus are rare
# Change m so that this is close to 5%
tokens.percent <- tokens.rare / 100000
# Determine how many types have these frequencies
types.rare <- sum(Vm(notes.gigp.spc,1:m))
# Determine how many total types there are in the corpus
types <- V(notes.gigp.spc)
# Determine size of lexicon for the common types
types.common <- types - types.rare

# Determine the frequencies of the first 100 words in the corpus
freq <- table(words[1:100])

