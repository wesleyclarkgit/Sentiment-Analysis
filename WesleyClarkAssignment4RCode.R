# install the ibmdbR package.  This enables functionality with IBMDB2 in 
# conjunction with R
# Wesley Clark - Cloud NLP

install.packages("ibmdbR")

library(ibmdbR)

#connect to the cloud environment using proper log in credentials
dsn_driver <- c("BLUDB")
dsn_database <- c("BLUDB")
dsn_hostname <- c("dashdb-entry-yp-dal10-01.services.dal.bluemix.net")
dsn_port <- "50000"
dsn_protocol <- "TCPIP"
dsn_uid <- c("dash7433")
dsn_pwd <- c("_eoHQsJQ_u29")

conn_path <- paste(dsn_driver,  
                   ";DATABASE=",dsn_database,
                   ";HOSTNAME=",dsn_hostname,
                   ";PORT=",dsn_port,
                   ";PROTOCOL=",dsn_protocol,
                   ";UID=",dsn_uid,
                   ";PWD=",dsn_pwd,sep="")
mycon <- idaConnect(conn_path) 
idaInit(mycon)

#Create a Claritin varibale to read the data from database
Claritin <- idaQuery("SELECT * from CLARITIN_SIDEFFECTS")

#Check the row counts
nrow(Claritin)
idadf(mycon, "SELECT count(*) FROM CLARITIN_SIDEFFECTS")

# Another way to look at the databse - dimension and structure
dim(Claritin)
str(Claritin)
summary(Claritin)


#List the database tables
idaShowTables()
#List the database tables that contain AIR in the name
idaShowTables(matchStr='CLARITIN')
#To check if the table exists
idaExistTable('CLARITIN_SIDEFFECTS')
# Out of curiousity check to see if another table exists that should not
idaExistTable('CLARITIN')




#number of tweets per sentiment
table(SENTIMENT$CLARITIN_SIDEFFECTS)
idadf(mycon, "SELECT SENTIMENT, count(SENTIMENT) 
      FROM CLARITIN_SIDEFFECTS 
      GROUP by SENTIMENT
      ORDER BY SENTIMENT;")

#number of each Sentiment
table(Claritin$SENTIMENT)


#Pie charts
#Reset the margin
par(mar=c(1,1,1,1))
#Use default colors
pie (table(Claritin$SENTIMENT))
#Change colors
pie (table(Claritin$SENTIMENT), col=c("blue", "yellow", "green", "purple", "pink"))



#load the plyr package
library("plyr")
#List the most common complaints
reasonCounts<-na.omit(count(Claritin$SENTIMENT))
reasonCounts<-reasonCounts[order(reasonCounts$freq, decreasing=TRUE), ]
reasonCounts
# Using ggplot2 package
# Complaints frequency plot
library(ggplot2)
wf <- data.frame(reasonCounts)
p <- ggplot(wf, aes(wf$x, wf$freq))
p <- p + geom_bar(stat="identity", fill = "#FF6666")
p <- p + theme(axis.text.x=element_text(angle=25, hjust=1))

p + labs(x = "Sentiment", y  = "Number of Tweets", title = "Sentiment Frequency Bar Graph", caption = "Claritin Twitter Data")


#Tweets by sentiment proportions with NA
q <- ggplot(Claritin, aes(Claritin$SENTIMENT, fill=SENTIMENT)) + geom_bar()

# Add Labels
q + labs(x = "Sentiment", y  = "Number of Tweets", title = "Sentiment Frequency Bar Graph", caption = "Claritin Twitter Data")

#Text Mining
#Install the following packages (only once)
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")

#Load the packages in memory
library("tm")
library("wordcloud")
library ("SnowballC")

#Load the tweets with positive sentiment into the data frame positive
positive <- idadf(mycon, "SELECT CONTENT, SENTIMENT 
                  FROM CLARITIN_SIDEFFECTS
                  WHERE SENTIMENT = '4' OR SENTIMENT = '5';")
View(positive)


docs<-VectorSource(Claritin$CONTENT)
docs<-Corpus(docs)
inspect(docs[[1]])
inspect(docs[[2]])
inspect(docs[[15]])

#Strip the white space
docs <- tm_map(docs, stripWhitespace)

#Remove the URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))

#Remove non ASCII character
removeInvalid<-function(x) gsub("[^\x01-\x7F]", "", x)
docs <- tm_map(docs, content_transformer(removeInvalid))

#Remove punctuation
docs <- tm_map(docs, removePunctuation)



#remove the numbers
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "@")   #Remove @
docs <- tm_map(docs, toSpace, "/")   #Remove /
docs <- tm_map(docs, toSpace, "\\|") #Remove |


#Remove the stop word
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("SMART"))


docs <- tm_map(docs, stemDocument)


#Remove the white space introduced during the pre-processing
docs <- tm_map(docs, stripWhitespace)
dtm <- DocumentTermMatrix(docs)

m <- as.matrix(dtm)   #Convert dtm to a matrix
dim(m)                # Display number of terms and number of documents
View(m)               # Preview the matrix

neutral <- idadf(mycon, "SELECT 
                 CONTENT FROM Claritin 
                 WHERE SENTIMENT = '3'")

negative <- idadf(mycon, "SELECT 
                  CONTENT FROM Claritin 
                  WHERE SENTIMENT = '1', '2'")

#find the terms that appear at least 50 times
findFreqTerms(dtm, lowfreq=70)
#find the terms asosciated with good and great with correlation at least 0.15
findAssocs(dtm, c("great", "good"), corlimit=0.15)

dtms <- removeSparseTerms(dtm, 0.6) # Prepare the data (max 60% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, min.freq=35, max.words=100, rot.per=0.2, colors=dark2)    




#Network of terms
#Load the igraph package
library ("igraph")
tdm<-TermDocumentMatrix(docs)              # Term document matrix
tdm <- removeSparseTerms(tdm, 0.95)        # Remove sparse terms
termDocMatrix <- as.matrix(tdm)            # Convert tdm to matrix
View(termDocMatrix)
termDocMatrix[termDocMatrix>=1] <- 1       # Set non-zero entries to 1 (1=term present, 0=term absent)
#Term matrix tracks how many times the terms appear together
# %*% is a matrix multiplication operator, and t is a transpose
termMatrix <- termDocMatrix %*% t(termDocMatrix)   
View (termMatrix)
g <- graph.adjacency(termMatrix, weighted=T, mode="undirected")
g <- simplify(g)       # Remove the self-relationships
# V(g) is a graph vertex
V(g)$label <- V(g)$name  # Label each vertex with a term
V(g)$degree <- degree(g)
set.seed(1111)

#Try different layouts
plot(g, layout=layout.fruchterman.reingold(g), vertex.color="cyan")
plot(g, layout=layout_with_gem(g), vertex.color="pink")
plot(g, layout=layout_as_star(g), vertex.color="yellow", vertex.shape="square")
plot(g, layout=layout_on_sphere(g), vertex.color="magenta")
plot(g, layout=layout_randomly(g), vertex.size=10)
plot(g, layout=layout_in_circle(g), vertex.color="pink", vertex.size=35)
plot(g, layout=layout_nicely(g), vertex.color="plum", vertex.size=25)
# My personal favourite layout
plot(g, layout=layout_nicely(g), vertex.color="pink", vertex.size=35)
