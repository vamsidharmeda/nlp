# Set up openNLP libraries 
install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",
                 repos=NULL, type="source")
library(NLP)
library(openNLP)
library(igraph)
library(data.tree)

# Set up coreNLP libraries 
install.packages('coreNLP')
require(coreNLP)
downloadCoreNLP()

# Set up openNLP annototor
x1 <- Maxent_Sent_Token_Annotator()
x2 <- Maxent_Word_Token_Annotator()
parser <- Parse_Annotator()

# Set up coreNLP annototor
initCoreNLP()

# Function to build parse tree from annotation data
parse2tree <- function(ptext) {
  stopifnot(require(NLP) && require(igraph))
  
  ## Replace words with unique versions
  ms <- gregexpr("[^() ]+", ptext)                                      # just ignoring spaces and brackets?
  words <- regmatches(ptext, ms)[[1]]                                   # just words
  regmatches(ptext, ms) <- list(paste0(words, "_", seq.int(length(words))))  # add id to words
  
  ## Going to construct an edgelist and pass that to igraph
  ## allocate here since we know the size (number of nodes - 1) and -1 more to exclude 'TOP'
  edgelist <- matrix('', nrow=length(words)-2, ncol=2)
  
  ## Function to fill in edgelist in place
  edgemaker <- (function() {
    i <- 0                                       # row counter
    g <- function(node) {                        # the recursive function
      if (inherits(node, "Tree")) {            # only recurse subtrees
        if ((val <- node$value) != 'TOP_1') { # skip 'TOP' node (added '1' above)
          for (child in node$children) {
            childval <- if(inherits(child, "Tree")) child$value else child
            i <<- i+1
            edgelist[i,1:2] <<- c(val, childval)
          }
        }
        invisible(lapply(node$children, g))
      }
    }
  } )()
  
  ## Create the edgelist from the parse tree
  ptree <- Tree_parse(ptext)
  edgemaker(ptree)
  tree <- FromDataFrameNetwork(as.data.frame(edgelist))
  return (tree)
}

# Function to display a parse tree
showTree <- function(ptext) {
  tree <- parse2tree(ptext)
  SetNodeStyle(tree, style = "filled", shape = "box", fillcolor = "cornflowerblue", fontcolor="white", fontname="helvetica")
  SetEdgeStyle(tree, arrowhead = "vee", color = "blue", penwidth = 2)
  Do(tree$leaves, function(node) SetNodeStyle(node, shape = "oval", fillcolor = "gold", fontcolor="black"))
  plot(tree)
}

# Sample clinical text (edited from notes file)
text <- "This is a new patient, who is Spanish speaking only. Recent admission was for abdominal pain and diarrhea.
Workup revealed cholelithiasis without evidence of cholecystitis nephrolithiasis. She has otherwise normal abdominal CT. 
Diabetes was poorly controlled in hospital. Also she was found to have confusing iron panel. Need to repeat. 
Patient complains of itchy rash. Also has pain in foot and knee. Patient is checking blood sugar twice a day and says range is good. 
Social history: she lives with brother. Quit alcohol, no tobacco. Drives taxi. Bring new housing form.  
Glucophage 500 mg p.o. t.i.d., Glucotrol XL 10 mg p.o. q.d., lisinopril 40 mg p.o. q.d., and Hytrin 1 mg p.o. q.d.  
Physical exam: well appearing. Blood pressure 130/80. Heart normal. 
Lungs clear. Abdomen normal. Knees/ankles from feet with tinea rash. Normal pulses. Normal LT. 
1. DM: check labs. 
2. HTN: renew meds
Patient has been out of lisinopril fot 2 weeks. 
3. HM: check chol ophthalmology 
4. Pain: possibly neuropathic. Naprosyn bid." 

# Annotate the text using openNLP
a1 <- annotate(text,x1)
a2 <- annotate(text,x2,a1)
annotation1 <- parser(text,a2)
parses1 <- sapply(annotation1$features,`[[`, "parse")
parses1 <- lapply(parses1, function(x) gsub("[\n\r]","", x) )

# Choose a sentence 
sent <- 3

# Display parse tree created by openNLP 
showTree(parses1[[sent]])

# Annotate text using coreNLP
annotation2 = annotateString(text)
parses2 <- getParse(annotation2)
parses2 <- lapply(parses2, function(x) gsub("[\n\r]","", x) )
parses2 <- lapply(parses2, function(x) gsub("ROOT","TOP", x) )
parses3 <- getDependency(annotation2)

# Display parse tree created by coreNLP 
showTree(parses2[[sent]])

# Display dependency tree created by coreNLP 
dep.frame <- parses3[parses3$sentence == sent,]
dep.matrix <- as.matrix(dep.frame[,2:3])
dep.tree <- FromDataFrameNetwork(as.data.frame(dep.matrix))
SetNodeStyle(dep.tree, style = "filled", shape = "box", fillcolor = "LightBlue", fontcolor="black", fontname="helvetica")
SetEdgeStyle(dep.tree, arrowhead = "vee", color = "Purple", penwidth = 2)
plot(dep.tree)







