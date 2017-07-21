# Install libraries
install.packages("RTextTools")
library(RTextTools)
library(tm)

# Set the value for the sample size, and get a random sample from New York Times collection
size <- 3000
data("NYTimes")
data <- NYTimes[sample(1:3000,size=size,replace=FALSE),]

# Each document in the collection has a title, a subject and class (Topic Code).
titles <- data["Title"]
subjects <- data["Subject"]
classes <- data$Topic.Code
data[1:10,3:5]

# Choose options for processing the text, then create a document term matrix using titles and subjects.
stemming <- TRUE

# The weight of each term is term frequency (Tf) 
# Tf = number of times term occurs in a document)
weighting <- weightTf

# The weight of each term is term frequency (Tf) times inverse document frequency (Idf)
# Idf = log of inverse of number documents that contain term in collection
weighting <- weightTfIdf 

m <- create_matrix(cbind(titles,subjects), language="english", removeNumbers=TRUE, stemWords=stemming, weighting=weighting)

# Remove terms with sparse factor less than value
#m <- create_matrix(cbind(titles,subjects), language="english", removeNumbers=TRUE, stemWords=stemming, weighting=weighting, removeSparseTerms = 0.95)

inspect(m[1:10,1:10])

# Choose what proportion of the data will be used for training.
p <- 0.75
i <- floor(size*p)

# Create a container that has a training set and testing set, using the proportion you chose.
# We are going to classify the documents using the term matrix as input, and Topic Code as the output class. 
# The data is not "virgin" (new data) because someone has manually assigned a class.
container <- create_container(m,classes,trainSize = 1:i,testSize = (i+1):size, virgin=FALSE)

# The SVM and MAXENT algorithms use low amounts of memory. 
# The others may be VERY SLOW, so set the size variable to a small value (e.g. 100).
# GLMNET seemed to have errors, so may not work for you. 

# Train model using support vector machine
SVM <- train_model(container,"SVM")

# Train model using maximum entropy
MAXENT <- train_model(container,"MAXENT")

# Train model using generalized linear model
#GLMNET <- train_model(container,"GLMNET")

# Train model using scaled linear discriminant analysis 
#SLDA <- train_model(container,"SLDA")

# Train model using boosting
#BOOSTING <- train_model(container,"BOOSTING")

# Train model using bagging
#BAGGING <- train_model(container,"BAGGING")

# Train model using random forest 
#RF <- train_model(container,"RF")

# Train model using neural net
#NNET <- train_model(container,"NNET")

# Train model using regression tree
#TREE <- train_model(container,"TREE")

# Apply the models to the training and testing data in the container
SVM_CLASSIFY <- classify_model(container,SVM)
MAXENT_CLASSIFY <- classify_model(container,MAXENT)
#GLMNET_CLASSIFY <- classify_model(container,GLMNET)
#SLDA_CLASSIFY <- classify_model(container,SLDA)
#BOOSTING_CLASSIFY <- classify_model(container,BOOSTING)
#BAGGING_CLASSIFY <- classify_model(container,BAGGING)
#RF_CLASSIFY <- classify_model(container,RF)
#NNET_CLASSIFY <- classify_model(container,NNET)
#TREE_CLASSIFY <- classify_model(container,TREE)

# Combine the results of the different classifiers
#results <- cbind(SVM_CLASSIFY, MAXENT_CLASSIFY, SLDA_CLASSIFY, 
#                 BOOSTING_CLASSIFY,BAGGING_CLASSIFY, RF_CLASSIFY ,NNET_CLASSIFY,TREE_CLASSIFY)
#results <- cbind(SVM_CLASSIFY, MAXENT_CLASSIFY, RF_CLASSIFY)

results <- cbind(SVM_CLASSIFY, MAXENT_CLASSIFY)

# Compute analytics, based on comparing training and testing data
analytics <- create_analytics(container,results)
summary(analytics)


# The following data frames let you look at the results in greater detail

# PRECISION, RECALL, F-SCORES, AND ACCURACY for each algorithm, sorted by topic code
alg_summary <- analytics@algorithm_summary 

# For each topic code, show number of documents manually coded, consensus coded, and correctly coded
top_summary <- analytics@label_summary

# For each document, show prediction for each algorithm, probability and number that agreed
doc_summary <- analytics@document_summary 

# Shows coverage (percentage of documents that meet recall threshold) versus recall for the ensemble of algorithms 
ens_summary <- analytics@ensemble_summary 