# Reading a data frame into a Volatile Corpus for text analysis
# Marilyn Langley 4/6/16

library(tm)
getReaders()


# Example code from the getreaders() help info

df <- data.frame(contents = c("content 1", "content 2", "content 3"),
                 title    = c("title 1"  , "title 2"  , "title 3"  ),
                 authors  = c("author 1" , "author 2" , "author 3" ),
                 topics   = c("topic 1"  , "topic 2"  , "topic 3"  ),
                 stringsAsFactors = FALSE)
m <- list(content = "contents", heading = "title",
          author = "authors", topic = "topics")
myReader <- readTabular(mapping = m)
ds <- DataframeSource(df)
elem <- getElem(stepNext(ds))
(result <- myReader(elem, language = "en", id = "id1"))
meta(result)


# Adapting it to my data frame, a subset of the review file.

setwd("~/0 Projects/0 Data Science/2016 EDSO Program/Capstone")
bad_reviews <- read.csv("bad_reviews.csv")
selection <- sample(1:dim(bad_reviews)[1], dim(bad_reviews)[1]/100)
some_reviews <- bad_reviews[selection,]
View(some_reviews)

df <- some_reviews

m <- list(content = "text", business_id = "business_id", user_id = "user_id", 
           stars = "stars", date = "date", review_id = "review_id", type = "type",
           votes.cool = "votes.cool", votes.funny = "votes.funny", votes.useful = "votes.useful")
myReader <- readTabular(mapping = m)

ds <- DataframeSource(df)
#names(ds)
#ds$position

#elem <- getElem(stepNext(ds))
#(result <- myReader(elem, language = "en", id = "id1"))
#meta(result)
#meta(result,"id")
#content(result)

# So far so good. But we need the rest of the rows.
# Try to follow Nagiza's approach.

library(XML)

corpus <- VCorpus(DataframeSource(df), readerControl = list(reader = myReader))

class(corpus)
mode(corpus)
str(corpus[[1]])
str(corpus[[10]])

inspect(corpus[1:2])
content(corpus[[1]])

meta(corpus[[2]],"id")
identical(corpus[[2]], corpus[["2"]])

# To save the corpus...
# Set working directory to a location where you can write
# lots of files... 
# getwd() 
# writeCorpus(corpus)

#-----------------------------------------
# Ex. 2: Text Preprocessing: removing extra
# white spaces and stopwords, converting to lower case,
# transforming via stemming
#-----------------------------------------
getTransformations()
#help(tm_map)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
mode(corpus)
content(corpus[[1]])

corpus <- tm_map(corpus, removeWords, stopwords("english"))
content(corpus[[1]])

# install.packages("SnowballC")
library(SnowballC)
corpus <- tm_map(corpus, stemDocument)
content(corpus[[1]])

# Hey, I think it's working! :)

#-----------------------------------------
# Ex. 3: Build vector-space model for Corpora:
#  Create document-by-terms matrix.
# With such a model one can either view documents
#  in the space of terms/keywords or
#  view terms/keyword in the space of documents
# Once a vector/feature space is built,
# clustering, classification, etc, could be applied
#-----------------------------------------
# Create a term-document matrix from a corpus
# Check the number of documents in the Corpus
length(corpus)
# Documents as rows and terms as columns
dtm <- DocumentTermMatrix(corpus)
dim(dtm)
inspect(dtm[2:5, 200:205])
# Note: Matrix is very sparse (too many zero's); apply 
# R packages optimized for sparse matrices (if needed)

# or vice versa: terms is rows and docs as columns
tdm <- TermDocumentMatrix(corpus)
dim(tdm)
inspect(tdm[200:205, 2:5])

# Question: Can you take a transpose of
# a dtm matrix, such as t(dtm[2:5, 200:205])?
# Justify your answer


#-----------------------------------------
# Ex. 4: Basic operations over the Corpus:
#   most frequent terms, terms associated w/
#   other terms, removing sparse terms,
#   include only terms from the dictionary
#-----------------------------------------
# terms that appear in all the documents

findFreqTerms(dtm, dim(dtm)[1])
# terms that appear in at least half the documents
findFreqTerms(dtm, dim(dtm)[1]/2)

# terms that are at least 0.8 correlated with "govern"
findAssocs(dtm, "govern", 0.8)
findAssocs(dtm, "food", 0.3)
findAssocs(dtm, "servic", 0.3)


findAssocs(tdm, c("oil", "opec", "xyz"), c(0.7, 0.75, 0.1))

# remove sparse terms: removes those terms 
# that are at least 40 percent sparse,
# i.e. at least 40% or more docs do not have this term
inspect(removeSparseTerms(dtm, 0.4))

# keep only the terms from the dictionary
inspect(DocumentTermMatrix(corpus,
                           list(dictionary=c("oil","crude","price"))))

# Interesting... there's a lot of association with oil -ML

#------------------------------
# Ex. 5: TF-IDF Weighting, document-document distance, 
#   document clustering
#------------------------------
# refresh how dtm looks with raw frequencies as weights
inspect(dtm[5:10, 740:745])

# Create document-term matrix with TF-IDF weights
# help(DocumentTermMatrix)
dtm <- DocumentTermMatrix(corpus,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =FALSE), stopwords = FALSE))
inspect(dtm[5:10, 740:745])
as.matrix(dtm[5:10, 740:745])

d <- dist(as.matrix(dtm))
plot(hclust(d))
