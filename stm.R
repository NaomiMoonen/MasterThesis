library(parallel)
library(data.table)
library(stm)
library(tm)
library(wordcloud)
library(tidytext)
library(topicmodels)
library(textmineR)
library(quanteda)
library(stminsights)
library(devtools)
library(stmBrowser)
library(igraph)
library(ggthemes)
library(bigrquery)
library(tidyverse)
library(ggplot2)

#Set the directory and read in the stopwords and data
dir = "C:/"
setwd(dir)
load('insights.RData')

customstopwords <- read.csv(file="customstopwords.csv")
customstopwords <- customstopwords$words
length(customstopwords)

data <- read.csv(file="TokenizedAllScraper.csv", header=TRUE, skipNul = TRUE, stringsAsFactors = FALSE)
#Get the texts
text <- data$Tokenized_texts
length(text)
#STM'function to take in a raw vector of texts
processed <- textProcessor(text, metadata = data, lowercase=FALSE, customstopwords = FALSE, removestopwords=FALSE,removenumbers=FALSE,
                           removepunctuation=FALSE,stem=FALSE, verbose=TRUE)
#Plot documents, words and tokens removed at various word thresholds
par()
plotRemoved(processed$documents, lower.thresh = seq(1, 50, by = 1))
length(processed$documents)

#Prepare documents for analysis with stm
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh=15)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
names(data)

#Transform variables into the right type
out$meta$companytype <- as.factor(out$meta$companytype)
out$meta$period <- as.factor(out$meta$period)
out$meta$Macro.Industry <- as.factor(out$meta$Macro.Industry, levels=c("AHealthcare", "Financials", "Consumer Products and Services","Retail", "Telecommunications","High Technology", "Media and Entertainment","Consumer Staples","Energy and Power","Real Estate","Industrials","Government and Agencies"))
out$meta$DocumentDateInt <- as.numeric(out$meta$DocumentDateInt)
out$meta$DealDateInt <- as.numeric(out$meta$DealDateInt)
out$meta$DocumentYear <- as.numeric(out$meta$DocumentYear)
out$meta$DealYear <- as.numeric(out$meta$DealYear)

out$meta$Deal.Date <- as.factor(out$meta$Deal.Date)
out$meta$Document.Date <- as.factor(out$meta$Document.Date)
out$meta$Doc.count <- as.factor(out$meta$Doc.count)

#Create models with various numbers of topics
model10.2 <-stm(documents = out$documents, vocab = out$vocab, K = 10, prevalence =~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), data=out$meta, init.type = "Spectral", verbose=TRUE)
model20.2 <-stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence =~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), data=out$meta, init.type = "Spectral", verbose=TRUE)
model30.2 <-stm(documents = out$documents, vocab = out$vocab, K = 30, prevalence =~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), data=out$meta, init.type = "Spectral", verbose=TRUE)
model40.2 <-stm(documents = out$documents, vocab = out$vocab, K = 40, prevalence =~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), data=out$meta, init.type = "Spectral", verbose=TRUE)
model50.2 <-stm(documents = out$documents, vocab = out$vocab, K = 50, prevalence =~companytype+period+Macro.Industry+s(DocumentYear)+s(DealYear), data=out$meta, init.type = "Spectral", verbose=TRUE)
model60.2 <-stm(documents = out$documents, vocab = out$vocab, K = 60, prevalence =~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), data=out$meta, init.type = "Spectral", verbose=TRUE)
model70.2 <-stm(documents = out$documents, vocab = out$vocab, K = 70, prevalence =~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), data=out$meta, init.type = "Spectral", verbose=TRUE)
model75.2 <-stm(documents = out$documents, vocab = out$vocab, K = 75, prevalence =~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), data=out$meta, init.type = "Spectral", verbose=TRUE)
model80.2 <-stm(documents = out$documents, vocab = out$vocab, K = 80, prevalence =~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), data=out$meta, init.type = "Spectral", verbose=TRUE)
model90.2 <-stm(documents = out$documents, vocab = out$vocab, K = 90, prevalence =~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), data=out$meta, init.type = "Spectral", verbose=TRUE)
model100.2 <-stm(documents = out$documents, vocab = out$vocab, K = 100, prevalence =~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), data=out$meta, init.type = "Spectral", verbose=TRUE)

#Plot most common words per topic 
labelTopics(model50.2, n=5)
#Plot the topic qualities
topicQuality(model50.2, docs, xlab = "Semantic Coherence",ylab = "Exclusivity", main="Semantic Coherence versus Exclusivity 50 Topics", labels = 1:ncol(model50.2$theta), M = 10)
#Plot the topic proportions per topic
par(mar=c(4,0,2,0.1))
plot(model50.2, type="summary",xlim=c(0, 0.065), ylim=c(1,50),cex.main = 1, cex.axis=0.75,cex.lab= 0.75, text.cex = 1,labeltype = "prob", n=3)
plot(model50.2, type = "hist", topics = 1:3)
dt <- make.dt(model50.2, meta=out$meta)


#Estimate the regressions for the models with the different numbers of topics
effects10.2 <- estimateEffect(1:10 ~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), model10.2, meta=out$meta, uncertainty="Global")
effects20.2 <- estimateEffect(1:20 ~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), model20.2, meta=out$meta, uncertainty="Global")
effects30.2 <- estimateEffect(1:30 ~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), model30.2, meta=out$meta, uncertainty="Global")
effects40.2 <- estimateEffect(1:40 ~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), model40.2, meta=out$meta, uncertainty="Global")
effects50.2 <- estimateEffect(1:50 ~companytype+period+Macro.Industry+s(DealYear)+s(DocumentYear), model50.2, meta=out$meta, uncertainty="Global")
effects60.2 <- estimateEffect(1:60 ~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), model60.2, meta=out$meta, uncertainty="Global")
effects70.2 <- estimateEffect(1:70 ~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), model70.2, meta=out$meta, uncertainty="Global")
effects75.2 <- estimateEffect(1:75 ~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), model75.2, meta=out$meta, uncertainty="Global")
effects80.2 <- estimateEffect(1:80 ~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), model80.2, meta=out$meta, uncertainty="Global")
effects90.2 <- estimateEffect(1:90 ~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), model90.2, meta=out$meta, uncertainty="Global")
effects100.2 <- estimateEffect(1:100 ~companytype+period+Macro.Industry+s(DealYearMonth)+s(DocumentYearMonth), model100.2, meta=out$meta, uncertainty="Global")

#Topic names that replace the numbers for model with 50 topics
topicsNames <- c("Cancer and Imaging", "Employee safety", "Client services", "Apps", "Care home", "Retirement plan", 
            "Reinsurance","Data-driven solutions", "Health cover","Drug prescriptions and Pharmacy", 
          "Update information and browser", "Fraud and Safety", "Miscellaneous", "Care quality and Patient satisfaction", 
          "Community support", "Claim form", "Medicare", "Health insurance plans", "Veteran support and Diversity", 
          "Investments", "Liability insurance and Coverage", "Policy coverage and Claims", "Healthcare", 
          "Healthcare during Traveling", "Nonlife Insurance", "Diseases and Disorders", "Health plan options", "Events", "Dentistry", 
          "Pregnancy and Children", "Products and Services", "Healthy lifestyle", "Reports and Results", 
          "Service provider network", "Care services", "Life insurance", "Health insurance", "Travel insurance", 
          "Member programs", "Doctors and Medical indemnity", "Offices and Hospitals", "Business announcements", 
          "Healthcare quality and improvement", "Risk management", "Employee benefits", "Company portfolio and team",
          "Drug plan enrolment", "Healthy food", "Exercising", "Online information")

#Obtain and save the correlations between the topics
correlations <- topicCorr(model50.2)
write.table(correlations$poscor, sep=",",file="D:/ProcessedData/positivecorrelationstopics.csv")


#Save the regression effects to a text file
for (topic in c(effects50.2$topics)){
  number <- as.character(topic)
  name <- paste("D:/ProcessedData/finaleffects/topic",number,".txt", sep="", collapse="")
  sink(file=name )
  print(summary(effects50.2, topics=topic, max.print=1000))
  sink()
  
}
#Save the industry effects to a text file
for (topic in c(effects50.2$topics)){
  number <- as.character(topic)
  name <- paste("D:/ProcessedData/industrymeans/topic",number,".txt", sep="", collapse="")
  r<- plot(effects50.2, covariate="Macro.Industry", topics=topic, model=model50.2, method="pointestimate",cex.lab=0.5)
  sink(file=name)
  print(r$means)
  sink()
}

#Plot effect of covariates on topics
plot(effects50.2, covariate="Macro.Industry", topics=(20), model=model50.2, method="pointestimate")
par(mar=c(4,10,2,0.1))
plot(effects50.2, covariate="companytype", topics=c(1:50), model=model50.2, method="difference", cov.value1="Target", cov.value2 = "Acquiror",
     xlab="More Acquiror ... More Target", main="Effect of Target versus Acquiror", 
     labeltype="custom",custom.labels=topicsNames)
plot(effects50.2, covariate="period", topics=c(1:50), model=model50.2, method="difference", cov.value1="before", cov.value2 = "after",
     xlab="More After ... More Before", main="Effect of Before versus After", 
     labeltype="custom",custom.labels=topicsNames)
r<- plot(effects50.2, covariate="Macro.Industry", topics=c(28), model=model50.2, method="pointestimate", main="Effect of Industry on Topic 1",cex.lab=0.5)
plot(effects50.2, covariate="Macro.Industry", model=model50.2, method="difference", cov.value1="AHealthcare", cov.value2 ="Government and Agencies",
     xlab="More Healthcare ... More Financials", main="Effect of Healthcare versus Financial Industry",
     labeltype="custom", custom.labels=topicsNames)
plot(effects50.2, covariate="DealYear", model=model50.2, topics=c(8), method="continuous")
plot(model50.2, type="perspectives",topics=c(27,9), frexw=0.6, text.cex = 1.5, plabels=c("Health plan options","Health cover"))
r$uvals

summary(effects50.2, topics=6, max.prin=1000)

plot(effects50.2, covariate="DealDateInt", topics=c(4), model=model50.2, method="continuous",printlegend=FALSE, xaxt="n",  main="Effect of the Deal Year on Topic 1")
axis(1,at=2010:2018, labels=2010:2018)
plot(effects50.2, covariate="DocumentYear", topics=c(10), model=model60, method="continuous",printlegend=FALSE, xaxt="n",  main="Effect of the Deal Year on Topic 1")
axis(1,at=2009:2020, labels=2009:2020)

plot(model100, type="perspectives", labeltype="frex", topics=c(33, 35))

#Save the theta matrix and metadata to a csv file
write.table(model50.2$theta, sep=",",file="D:/ProcessedData/thetamatrix50.csv")
write.table(meta, sep=",", file="D:/ProcessedData/meta.csv")

#Create a word cloud for a topic
par(mar = rep(0, 4))
cloud(model50.2, topic =8,colors=brewer.pal(8, "Dark2"))

#Remove documents from the text vector that were removed in the preprocessing step
remainingdocs <- data$text[-processed$docs.removed]
remainingdocs <- remainingdocs[-out$docs.removed]
par(mfrow = c(1, 1),mar = c(5,1,5,1), cex=0.8)

#Output most representative documents for a particular topic and plot them as string
thoughts1 <- findThoughts(model50.2, texts = as.character(remainingdocs),n = 1000, topics = 8)
plotQuote(thoughts1$docs[[1]][1], width=175, main="Topic 1", text.cex = 1)

#Create a document-term matrix and print the 10 most often occurring tokens
corp <- Corpus(VectorSource(data$text))
dtm <- TermDocumentMatrix(corp)
dim(dtm)
dtm <- removeSparseTerms(dtm, 0.95)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
#Create a word cloud of the most occurring words
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Generates an interactive visualization of topic correlations/ hierarchy in a Structural Topic Model 
stmCorrViz::stmCorrViz(model60.2, file="D:/ProcessedData/Corr60.html")
save.image('insights.RData')
run_stminsights()

#Start browser to visualize both continuous and categorical covariate relationships with topics
stmBrowser(model50.2, data=out$meta, c("companytype", "period", "Macro.Industry", "DealYear", "DocumentYear"), text="text")

#Make a network graph either normal, for target/acquirer or for healthcare/financial topics and save them as pdfs.
cor(model50.2$theta)
model50.2$theta
datatarget <- read.csv(file="thetatarget.csv", header=TRUE, skipNul = TRUE, stringsAsFactors = FALSE)
datatarget <- as.data.frame(datatarget)
datatarget <- subset(datatarget, select=-c(X))

proptarget <- read.csv(file="targetprop.csv", header=TRUE, skipNul = TRUE, stringsAsFactors = FALSE)
topicsPropsInCorpus <- proptarget

dataacquirer <- read.csv(file="thetaacquirer.csv", header=TRUE, skipNul = TRUE, stringsAsFactors = FALSE)
dataacquirer <- as.data.frame(dataacquirer)
dataacquirer <- subset(dataacquirer, select=-c(X))

propacquirer <- read.csv(file="acquirerprop.csv", header=TRUE, skipNul = TRUE, stringsAsFactors = FALSE)
topicsPropsInCorpus <- propacquirer

databefore <- read.csv(file="thetabefore.csv", header=TRUE)
databefore <- subset(databefore, select=-c(X))
databefore <- as.double(databefore)
cor(databefore)

propbefore <- read.csv(file="beforeprop.csv", header=TRUE, skipNul = TRUE, stringsAsFactors = FALSE)
topicsPropsInCorpus <- propbefore

dataafter <- read.csv(file="thetaafter.csv", header=TRUE)
dataafter <- subset(dataafter, select=-c(X))
dataafter <- as.double(dataafter)

propafter <- read.csv(file="afterprop.csv", header=TRUE, skipNul = TRUE, stringsAsFactors = FALSE)
topicsPropsInCorpus <- propafter

targethc <- c(40,38,1,13,41,14,35,49,29,5,3,34)

topicnames <- topicsNames
names(topicPropsInCorpus) <- topicnames
mat2 <- cor(model50.2$theta)
mat2 <- cor(datatarget)
mat2 <- cor(dataacquirer)
mat2<- cor(databefore)
mat2 <- cor(dataafter)
mat2[mat2<0.025] <- 0
diag(mat2) <- 0
outmat2 <- mat2
typeof(outmat2)
mat2
set.seed(10)
g <- graph.adjacency(outmat2, mode="undirected", weighted=T)
g
if(length(labels)==0) labels = paste("Topic", topics)
E(g)
edges <- get.edgelist(g)
edgecors <- rep(NA,nrow(edges))
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(model50.2$theta)[edges[i,1],edges[i,2]]
}
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(datatarget)[edges[i,1],edges[i,2]]
}
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(dataacquirer)[edges[i,1],edges[i,2]]
}
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(databefore)[edges[i,1],edges[i,2]]
}
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(dataafter)[edges[i,1],edges[i,2]]
}
edgecors
edges
edge.width=35*edgecors
E(g)$weight
E(g)$size <- 1
E(g)$lty <- 1
E(g)$color <- "black"
prep <- plot.estimateEffect(effects50.2, "companytype", model=model50.2, method="difference",cov.value1="Target",cov.value2="Acquiror",custom.labels=topicnames,labeltype="custom")
prep <- plot.estimateEffect(effects50.2, "Macro.Industry", model=model50.2, method="difference",cov.value1="AHealthcare",cov.value2="Financials",custom.labels=topicnames,labeltype="custom")

est<- unlist(lapply(prep$means,function(x){return(x[1])}))
max(est)
mycols <- rev(colorRampPalette(c("red", "white", "blue"), bias=1)(5)) ## (n)
mycols <- rev(colorRampPalette(c("red","blue", "green","purple","orange","yellow","pink"),bias=1)(9))
mycols
seq(-.017,0.017,length.out=6)
#Sequence for targets acquirors
s <-c(-0.017, -0.0099,-0.0029, 0.0029, 0.0099, 0.017)
#Sequence for financial and healthcare
s <- c(-0.043, -0.01, -0.0025,0.0025, 0.01, 0.043)
s <- c("targethc","acquirerf","targethc","neutraln","targethc","neutralf","acquirerf","neutralhc","neutralhc","acquirerhc",
       "neutraln","neutralf","targethc","targethc","acquirern","neutralhc","acquirerhc","acquirerhc","acquirerhc",
       "neutralf","neutralf","targetf","acquirerhc","neutralhc","acquirerf","neutralhc","acquirerhc","targetn",
       "targethc","neutralhc","neutralf","acquirerhc","targetf","targethc","targethc","targetf","acquirerhc",
       "targethc","acquirerhc","targethc","targethc","acquirerf","acquirerhc","neutralf","neutraln","acquirerf",
       "acquirerhc","neutralhc","targethc","neutralhc")
       

colcat <- rep(NA,length(est))
mycols
for(i in 1:length(colcat)){
  colcat[i] <- max(which(est[i] > s))
}
for (i in 1:length(colcat)){
  if (s[i]=="targethc"){
    colcat[i] <- 1
  } else if (s[i]=="targetf"){
    colcat[i] <- 2
  } else if (s[i]=="targetn"){
    colcat[i] <- 3
  } else if (s[i]=="acquirerhc"){
    colcat[i] <- 4
  } else if (s[i]=="acquirerf"){
    colcat[i] <- 5
  } else if (s[i]=="acquirern"){
    colcat[i] <- 6
  } else if (s[i]=="neutralhc"){
    colcat[i] <- 7
  } else if (s[i]=="neutralf"){
    colcat[i] <- 8
  } else if (s[i]=="neutraln"){
    colcat[i] <- 9
  }

}

mycols[colcat]
plot(est,1:50,pch=19,cex=2,col=mycols[colcat]);abline(v=0)
V(g)$label=topicnames
corpus
V(g)$size <- topicPropsInCorpus*200
V(g)$size
vertex.color = mycols[colcat]
vertex.label.cex = 1
vertex.label.color = "black"
edge.color = "gray60"
set.seed(40)
wts <- E(g)$weight
mylayout <- layout.fruchterman.reingold(g,weight=wts)
pdf("corrNetworkAll.pdf",16,16)
plot(g, layout=mylayout,edge.color=edge.color,vertex.color=vertex.color, vertex.label.cex=vertex.label.cex, vertex.label.color=vertex.label.color,edge.width=edge.width)
dev.off()

