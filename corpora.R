# Step 1: Create a corpus of raw texts from a file. 
# Each element of the rawCorpus vector is a progress note for a patient.
require("NLP")
dir <- "/Users/stephenjohnson/Documents/NLP/"
base <- "notes.txt"
file <- paste0(dir,base)
fileLines <- scan(file=file,what="char",sep="\n",blank.lines.skip=FALSE)
wholeText <- String( paste(fileLines,collapse="\n") )
# Use blank lines to separate the text into paragraphs (individual notes)
paragraphs <- blankline_tokenizer(wholeText)
rawCorpus <- wholeText[paragraphs]

# Step 2: define regular expression patterns for sentences and for words
sent_tokenizer <- Regexp_Tokenizer("\\.",invert=TRUE)
sent_token_annotator <- Simple_Sent_Token_Annotator(sent_tokenizer)
word_tokenizer <- Regexp_Tokenizer("( |\n)",invert=TRUE)
#word_tokenizer <- function (x) wordpunct_tokenizer(x)
word_token_annotator <- Simple_Word_Token_Annotator(word_tokenizer)

# Step 3: annotate a single document.
# This function takes a raw document and returns an annotated document.
annotateDoc <- function(doc) {
  a1 <- annotate(doc, sent_token_annotator)
  a2 <- annotate(doc, word_token_annotator, a1)
  AnnotatedPlainTextDocument(doc,a2)
}

# Annotate the first note in the corpus
annotatedDoc <- annotateDoc(rawCorpus[1])
# Show what the sentences look like in note 1
sents(annotatedDoc)

# Step 4: Annotate the whole corpus
corpus <- lapply(notes,annotateDoc)
# Show what the sentences look like in note 10
sents(corpus[[10]])

# Step 5: Evaluate precision by examining sentences and counting the proportion that are tokenized correctly.
# Modfify the regular expressions above to improve tokeization, and report the new precision.

