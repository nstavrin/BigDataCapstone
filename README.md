# BigDataCapstone
library(tidyr)
library(dplyr)
library("RPostgreSQL")
library(tm)
library(wordcloud)
library(rpart)
library(rpart.plot)
library(ggplot2)
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

#Connect to Mimic Database

drv <- dbDriver("PostgreSQL")
con<-dbConnect(drv,dbname="mimic", host="localhost", port=5432, user="mimic")

#Check connection

dbListTables(con)
dbExistsTable(con, c("mimiciii","noteevents"))
dbListFields(con,c("mimiciii","patients"))
dbGetQuery(con,"Select count(subject_id) from mimiciii.patients")

#Table of gender and status

PatientTable<-dbReadTable(con,c("mimiciii","patients"))
table(PatientTable$gender, PatientTable$expire_flag)

#Deceased patients
PatientIDDeceased<-dbGetQuery(con,"Select subject_id from mimiciii.patients where dod is not null")
length(PatientIDDeceased[[1]])
PatientIDDeceased["Status"]<-0

#Alive patients
PatientIDAlive<-dbGetQuery(con,"Select subject_id from mimiciii.patients where dod is null")
length(PatientIDAlive[[1]])
PatientIDAlive["Status"]<-1

#Combined Status
PatientStatus<-rbind(PatientIDAlive,PatientIDDeceased)

#Female patients
PatientFemale<-dbGetQuery(con,"Select subject_id from mimiciii.patients where gender='F'")
PatientFemale["gender"]<-0

#Male patients
PatientMale<-dbGetQuery(con,"Select subject_id from mimiciii.patients where gender='M'")
PatientMale["gender"]<-1

#Combined Gender
PatientGender<-rbind(PatientMale,PatientFemale)

#Information about noteevents table

dbListFields(con,c("mimiciii","noteevents"))
dbGetQuery(con,"select distinct category from mimiciii.noteevents")
dbGetQuery(con,"select category, count(category) from mimiciii.noteevents group by category")

#Reading noteevents by type – Nursing notes

NoteeventNursingDF<-dbGetQuery(con,"select * from mimiciii.noteevents where category ='Nursing'")

#Combine Nursing Notes and Status

PatientStatus<-rbind(PatientIDAlive,PatientIDDeceased)

#Create Combined Nurse Notes table with Status and gender

NurseStatus<-left_join(NoteeventNursingDF, PatientStatus, by="subject_id")
NurseStatus<-left_join(NurseStatus, PatientGender, by="subject_id")

#Filter by status
NurseAlive<-filter(NurseStatus, NurseStatus$Status==1)
NurseDec<-filter(NurseStatus, NurseStatus$Status==0)

#Sample for corpus (10%)
NurseAliveS<-sample(nrow(NurseAlive),nrow(NurseAlive)*0.1)
NurseDecS<-sample(nrow(NurseDec),nrow(NurseDec)*0.1)

#Select Patients by Status

NurseNotesRP_Alive<-NurseStatus[NurseAliveS,]
NurseNotesRP_Dec<-NurseStatus[NurseDecS,]

NurseNotesBin<-c(NurseAliveS, NurseDecS)
NurseNotesRPBin<-NurseStatus[NurseNotesBin,]

#Corpus Alive Patient

nurse_corpus_alive<-VCorpus(DataframeSource(NurseNotesRP_Alive))

#Corpus Deceased Patients

nurse_corpus_dec<-VCorpus(DataframeSource(NurseNotesRP_Dec))

#Corpus Alive and Deceased Patients

nurse_corpus_bin<-VCorpus(DataframeSource(NurseNotesRPBin))

#Clean Corpus – Alive Patients

nurse_corpus_alive<-tm_map(nurse_corpus_alive, content_transformer(tolower))
nurse_corpus_alive<-tm_map(nurse_corpus_alive, removeWords, stopwords("english"))
nurse_corpus_alive<-tm_map(nurse_corpus_alive, removePunctuation)

toSpace<-content_transformer(function(x, pattern) gsub(pattern," ",x))
nurse_corpus_alive<-tm_map(nurse_corpus_alive, toSpace, c("his","her","this"))
nurse_corpus_alive<-tm_map(nurse_corpus_alive, toSpace, c("\n","but","and"))

nurse_corpus_alive<-tm_map(nurse_corpus_alive, stripWhitespace)

#Clean Corpus – Deceased Patients

nurse_corpus_dec<-tm_map(nurse_corpus_dec, content_transformer(tolower))
nurse_corpus_dec<-tm_map(nurse_corpus_dec, removeWords, stopwords("english"))
nurse_corpus_dec<-tm_map(nurse_corpus_dec, removePunctuation)

toSpace<-content_transformer(function(x, pattern) gsub(pattern," ",x))
nurse_corpus_dec<-tm_map(nurse_corpus_dec, toSpace, c("\n","but","and"))
nurse_corpus_dec<-tm_map(nurse_corpus_dec, toSpace, c("his","her","this"))	
nurse_corpus_dec<-tm_map(nurse_corpus_dec, stripWhitespace)

#Clean Corpus -  Binary

nurse_corpus_bin<-tm_map(nurse_corpus_bin, content_transformer(tolower))
nurse_corpus_bin<-tm_map(nurse_corpus_bin, removeWords, stopwords("english"))
nurse_corpus_bin<-tm_map(nurse_corpus_bin, removePunctuation)

nurse_corpus_bin<-tm_map(nurse_corpus_bin, removeNumbers)

toSpace<-content_transformer(function(x, pattern) gsub(pattern," ",x))
nurse_corpus_bin<-tm_map(nurse_corpus_bin, toSpace, c("\n","but","and"))
nurse_corpus_bin<-tm_map(nurse_corpus_bin, toSpace, c("his","her","this"))	
nurse_corpus_bin<-tm_map(nurse_corpus_bin, stripWhitespace)

#Document Term Matrix-Alive Patient

nurse_alive_dtm<-DocumentTermMatrix(nurse_corpus_alive)
nurse_alive_dtm
nurse_alive_dtm_s<-removeSparseTerms(nurse_alive_dtm, 0.99)
nurse_alive_dtm_s

#Word Cloud -Alive Patient

freqA<-data.frame(sort(colSums(as.matrix(nurse_alive_dtm_s)),decreasing=TRUE))
wordcloud(rownames(freqA),freqA[,1],max.words=50,random.order=FALSE, colors=brewer.pal(8,"Dark2"))

#Document Term Matrix-Alive Patient - TFIDF

nurse_dtm_alive_tfidf<-DocumentTermMatrix(nurse_corpus_alive, control=list(weighting=weightTfIdf))
nurse_dtm_alive_tfidf
nurse_dtm_alive_tfidf_s<-removeSparseTerms(nurse_dtm_alive_tfidf,0.99)
nurse_dtm_alive_tfidf_s

#Word Cloud -Alive Patient - TFIDF

freqAT<-data.frame(sort(colSums(as.matrix(nurse_dtm_alive_tfidf_s)),decreasing=TRUE))
wordcloud(rownames(freqAT),freqAT[,1],max.words=50,random.order=FALSE, colors=brewer.pal(8,"Dark2"))

#Alive Words

AliveWords<-rownames(freqAT)

#DocumentTerm Matrix-Deceased Patients

nurse_dec_dtm<-DocumentTermMatrix(nurse_corpus_dec)
nurse_dec_dtm
nurse_dec_dtm_s<-removeSparseTerms(nurse_dec_dtm, 0.99)
nurse_dec_dtm_s

#Word Cloud-Deceased Patients

freqD<-data.frame(sort(colSums(as.matrix(nurse_dec_dtm_s)),decreasing=TRUE))
wordcloud(rownames(freqD),freqD[,1],max.words=50, random.order=FALSE, colors=brewer.pal(8,"Dark2"))

#Document Term Matrix-Deceased Patient - TFIDF

nurse_dtm_dec_tfidf<-DocumentTermMatrix(nurse_corpus_dec, control=list(weighting=weightTfIdf))
nurse_dtm_dec_tfidf
nurse_dtm_dec_tfidf_s<-removeSparseTerms(nurse_dtm_dec_tfidf,0.99)
nurse_dtm_alive_tfidf_s

#Word Cloud -Deceased Patient - TFIDF

freqDT<-data.frame(sort(colSums(as.matrix(nurse_dtm_dec_tfidf_s)),decreasing=TRUE))
wordcloud(rownames(freqDT),freqDT[,1],max.words=50,random.order=FALSE, colors=brewer.pal(8,"Dark2"))

#Deceased Words 

DecWords<-rownames(freqDT)

#Document Term Matrix-Alive and Deceased Patients

nurse_bin_dtm<-DocumentTermMatrix(nurse_corpus_bin)

#Document Term Matrix-Alive and Deceased Patients - TFIDF

nurse_dtm_bin_tfidf<-DocumentTermMatrix(nurse_corpus_bin, control=list(weighting=weightTfIdf))
nurse_dtm_bin_tfidf_s<-removeSparseTerms(nurse_dtm_bin_tfidf,0.99)

#Combing and counting words and corpus for all patients (10% from Alive and 10% from Deceased ‘subset)

NurseNotesRPBin$AD<-sapply(nurse_corpus_bin, tm_term_score, AliveWords)
NurseNotesRPBin$DD<-sapply(nurse_corpus_bin, tm_term_score, DecWords)

NurseNotesRPBin1 = cbind(NurseNotesRPBin,as.matrix(nurse_dtm_bin_tfidf_s))

NurseNotesRPBin1$text<-NULL
NurseNotesRPBin1$subject_id<-NULL
NurseNotesRPBin1$hadm_id<-NULL
NurseNotesRPBin1$chartdate<-NULL
NurseNotesRPBin1$charttime<-NULL
NurseNotesRPBin1$storetime<-NULL
NurseNotesRPBin1$cgid<-NULL
NurseNotesRPBin1$iserror<-NULL
NurseNotesRPBin1$category<-NULL
NurseNotesRPBin1$description<-NULL
NurseNotesRPBin1$row_id<-NULL

#Training and Test Set

train<-sample(nrow(NurseNotesRPBin1),nrow(NurseNotesRPBin1)*0.8)
NurseNotes_train<-NurseNotesRPBin1[train,]
NurseNotes_test<-NurseNotesRPBin1[-train,]

#Decision Tree

NurseNoteDT<-rpart(Status~.,method="class", data=NurseNotes_train)
prp(NurseNoteDT)

NurseNoteDT2<-rpart(Status~.,method="class", data=NurseNotes_train, cp=0.001)
prp(NurseNoteDT2)

NurseNoteDT3<-rpart(Status~.,method="class", data=NurseNotes_train, cp=0.005)
prp(NurseNoteDT3)

NurseNoteDT4<-rpart(Status~.,method="class", data=NurseNotes_train, minsplit=5, cp=0.005)
prp(NurseNoteDT4)

#Confusion Matrix

predictionTree<-predict(NurseNoteDT, NurseNotes_test, type="class")
table(NurseNotes_test$Status, predictionTree)

predictionTreelowcp<-predict(NurseNoteDT2, NurseNotes_test, type="class")
table(NurseNotes_test$Status, predictionTreelowcp)

predictionTreemidcp<-predict(NurseNoteDT3, NurseNotes_test, type="class")
table(NurseNotes_test$Status, predictionTreemidcp)

#glm regrssion
NurseNoteGLM<-glm(Status~.,family="binomial", data=NurseNotes_train)

PredictGLM<-as.numeric(predict(NurseNoteGLM, NurseNotes_test, type="response")>0.5)
table(NurseNotes_test$Status, PredictGLM)

#Wordcorrelation
freqDW<-sort(colSums(as.matrix(nurse_dec_dtm_s)),decreasing=TRUE)
freqAW<-sort(colSums(as.matrix(nurse_alive_dtm_s)),decreasing=TRUE)
freqAWt<-sort(colSums(as.matrix(nurse_dtm_alive_tfidf_s)),decreasing=TRUE)
freqDWt<-sort(colSums(as.matrix(nurse_dtm_dec_tfidf_s)),decreasing=TRUE)
head(freqDW)
head(freqAW)
head(freqAWt)
head(freqDWt)

findAssocs(nurse_alive_dtm_s,"plan", corlimit=0.3)
findAssocs(nurse_dec_dtm_s,"plan", corlimit=0.3)
findAssocs(nurse_dtm_alive_tfidf_s,"pain", corlimit=0.2)
findAssocs(nurse_dtm_dec_tfidf_s,"pain", corlimit=0.2)

#Plots

plot(nurse_dec_dtm_s,terms=findFreqTerms(nurse_dec_dtm_s,lowfreq=5000)[1:20],corThreshold=0.3)
plot(nurse_alive_dtm_s,terms=findFreqTerms(nurse_alive_dtm_s,lowfreq=5000)[1:20],corThreshold=0.3)
plot(nurse_dtm_alive_tfidf_s,terms=findFreqTerms(nurse_dtm_alive_tfidf_s,lowfreq=50)[1:20],corThreshold=0.2)
plot(nurse_dtm_dec_tfidf_s,terms=findFreqTerms(nurse_dtm_dec_tfidf_s,lowfreq=40)[1:20],corThreshold=0.2)

 
