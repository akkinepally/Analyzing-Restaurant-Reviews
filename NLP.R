# Import the data 

data <- read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors =  FALSE)

# preprocessing
library(tm)
corpus <- VCorpus(VectorSource(data$Review))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

# BAG OF WORDS 
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)

# applying classification model 
data1 <- as.data.frame(as.matrix(dtm))
data1$Liked <- data$Liked
data1$Liked <- factor(data1$Liked, levels = c(0,1))

# split the data
library(caTools)
set.seed(123)
split <-  sample.split(data1$Liked, SplitRatio = 0.8)
train <- subset(data1, split == TRUE)
test <- subset(data1, split == FALSE)

# MODEL
library(randomForest)
model <- randomForest(x = data1[-692], y = data1$Liked, ntree = 10)

# Predict 
y_pred <- predict(model, newdata = test[-692])

# Confusion Matrix 
cm <- table(test[, 692], y_pred)
cm



