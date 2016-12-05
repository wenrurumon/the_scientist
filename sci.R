setwd("/Users/hzxsdtcgigi4/Downloads")
raw <- readLines("LUNGCANCER2013_14_parse.xls")
raw <- t(sapply(1:length(raw), function(i){
  strsplit(raw[i],"\t")[[1]]
}))
colnames(raw) <- raw[1,]
rownames(raw) <- raw[,1]
raw <- raw[-1,-1]

colnames(raw)

###########################
# Nonduplicated Author
###########################

#Process Affiliation
aff <- lapply(raw[,6],function(x){
  if(regexpr("(reprint author)",x)>0){x <- substr(x,regexpr("(reprint author)",x)+16,nchar(x))}
  x <- gsub("adress:","",x,ignore.case=T)
  x <- toupper(gsub("[^A-z]","",strsplit(x,",")[[1]]))
  x
  #[[:punct:]]
})
#system.time(aff_matrix <- sapply(aff,function(x){unique(unlist(aff))%in%x}))
#aff_matrix <- lapply(aff,function(x){unique(unlist(aff))%in%x})
jacob <- function(x){
  sapply(aff_matrix,function(x2){sum(x&x2)/sum(x|x2)})
}
#system.time(aff_score <- sapply(aff_matrix[1:10],jacob))
jacob2 <- function(i,j){
  xi <- aff_matrix[[i]]; xj <- aff_matrix[[j]]
  sum(xi&xj)/sum(xi|xj)
}
# system.time(aff_score <- lapply(1:5294,function(k){
#   cat(k);print(Sys.time())
#   sapply(k:5294,function(i){jacob2(i,k)})
# }))
# test <- matrix(NA,5294,5294)
# for ( i in 1:5294 ){
#   test[i:5294,i]<-aff_score[[i]]
# }
# hist(as.vector(test)[!is.na(as.vector(test))])
# quantile(as.vector(test)[!is.na(as.vector(test))],0.975)

#Test Same Name

sort(tapply(raw[,5],raw[,5],length))
raw2 <- raw[grepl("Li, J ",raw[,6]),]
aff2 <- raw2[,6]

aff2 <- lapply(aff2,function(x){
  if(regexpr("(reprint author)",x)>0){x <- substr(x,regexpr("(reprint author)",x)+16,nchar(x))}
  x <- gsub("adress:","",x,ignore.case=T)
  #x <- toupper(gsub("[^A-z]","",strsplit(x,",")[[1]]))
  x <- toupper(gsub("[[:punct:]]","",strsplit(x,",")[[1]]))
  unique(x)
})

jacob_aff2 <- function(i,j,data=aff2){
  temp <- c(unique(data[[i]]),unique(data[[j]]))
  length(temp)/length(unique(temp))-1
}
jacob_aff2_2 <- function(data=aff2){
  sapply(1:length(data),function(j){sapply(1:length(data),function(i){jacob_aff2(i,j)})})
}
aff2_score <- sapply(1:length(aff2),function(j){sapply(1:length(aff2),function(i){jacob_aff2(i,j)})>0.28})

bss=0; tss=1; i=0
while (bss<tss){
  i <- i+1
  aff2_km <- kmeans(aff2_score,i)
  bss <- aff2_km$betweenss; tss <- aff2_km$totss
};i
i = 0
i=i+1; raw2[aff2_km$cluster==i,8]
raw2[aff2_km$cluster==i,6]

#apply LDA on the groups

aff2.voca <- unique(unlist(aff2))
system.time(aff2.docs <- lapply(aff2,function(x){
  temp <- tapply(x,x,length)
  rbind(as.integer(match(names(temp),aff2.voca)-1),as.integer(temp))
}))

K <- i
result <- lda.collapsed.gibbs.sampler(aff2.docs,K,aff2.voca,5,0.1,0.1)
top.words <- top.topic.words(result$topics,5,by.score=T)
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions[is.na(topic.proportions)] <-  1 / K
colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
# topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
#                                    document=factor(1:N)),
#                              variable.name="topic",
#                              id.vars = "document")  
km <- kmeans(topic.proportions,K);km

j=0
j=j+1;raw2[km$cluster==j,8];j

# Weighted Score between words

system.time(aff2.mat <- t(sapply(aff2,function(x){
  unique(unlist(aff2))%in%x
})))
colnames(aff2.mat) <- unique(unlist(aff2))
aff2.mat <- aff2.mat[,!colnames(aff2.mat)%in%c("")]
voca.dist <- 1/(as.matrix(dist(t(aff2.mat)))+1)

##############################
# Abstract by LDA
##############################

aff2.docs <- raw[1:100,9]
aff2.voca <- lapply(aff2.docs,function(x){
  unique(tolower(gsub("[^A-z]","",strsplit(x," ")[[1]])))
})
aff2.voca_filter <- tapply(unlist(aff2.voca),unlist(aff2.voca),length)
aff2.voca_filter <- aff2.voca_filter[aff2.voca_filter<=10]

system.time(aff2.docs <- lapply(raw[1:100,9],function(x){
  x <- tolower(gsub("[^A-z]","",strsplit(x," ")[[1]]))
  x <- tapply(x,x,length)
  x <- x[names(x)%in%names(aff2.voca_filter)]
  rbind(as.integer(match(names(x),names(aff2.voca_filter))-1),as.integer(x))
}))

K<-10
result <- lda.collapsed.gibbs.sampler(aff2.docs,K,names(aff2.voca_filter),25,0.1,0.1)
top.words <- top.topic.words(result$topics,10,by.score=T)
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions[is.na(topic.proportions)] <-  1 / K
colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")

test <- topic.proportions[1:20,]
as.matrix(dist(test))==max(dist(test))

cbind(raw[c(6,19),1])
cbind(raw[c(4,10),1])

##############################
#Model with 2014 chephy
##############################

#Test

####

load("/Users/hzxsdtcgigi4/Documents/Work/sci/2014 fd che phy.RData")
library(XML)
#write(test_phy1[[1]],"/Users/hzxsdtcgigi4/Documents/Work/sci/ph1.txt")

#Parse Trail
test_phy <- c(test_phy1,test_phy2,test_phy3)
test_che <- c(test_che1,test_che2,test_che3,test_che4,test_che5,test_che6,test_che7,test_che8,test_che9)
test_both <- c(test_both1,test_both2,test_both3)
rm(test_phy1,test_phy2,test_phy3,test_che1,test_che2,test_che3,test_che4,test_che5,test_che6,test_che7,test_che8,test_che9,test_both1,test_both2,test_both3,temp,cl,i,url,url0)

system.time(phy <- t(sapply(test_phy,function(x){
  title=sapply(getNodeSet(htmlParse(x),"//div[@class='title']/value"),xmlValue)
  author=try(paste(sapply(getNodeSet(htmlParse(grep("mailto:",x,value=T)),"//a[@href]"),xmlValue),collapse="; "))
  abstract=sapply(getNodeSet(htmlParse(x),"//div[@class='block-record-info']/p[@class='FR_field']"),xmlValue)[[2]]
  source=sapply(getNodeSet(htmlParse(x),"//p[@class='sourceTitle']/value"),xmlValue)
  label=try(grep("Web of Science Categories:",x,value=T))
  label=substr(label,min(nchar(label),57),nchar(label)-4)
  c(title = if(length(title)==0){NA}else{title},
    author = if(class(author)=="try-error"){NA}else{author},
    abstract = if(length(abstract)==0){NA}else{abstract},
    source = if(length(source)==0){NA}else{source},
    category = if(length(label)==0){NA}else{label})
})))
system.time(che <- t(sapply(test_che,function(x){
  title=sapply(getNodeSet(htmlParse(x),"//div[@class='title']/value"),xmlValue)
  author=try(paste(sapply(getNodeSet(htmlParse(grep("mailto:",x,value=T)),"//a[@href]"),xmlValue),collapse="; "))
  abstract=sapply(getNodeSet(htmlParse(x),"//div[@class='block-record-info']/p[@class='FR_field']"),xmlValue)[[2]]
  source=sapply(getNodeSet(htmlParse(x),"//p[@class='sourceTitle']/value"),xmlValue)
  label=try(grep("Web of Science Categories:",x,value=T))
  label=substr(label,min(nchar(label),57),nchar(label)-4)
  c(title = if(length(title)==0){NA}else{title},
    author = if(class(author)=="try-error"){NA}else{author},
    abstract = if(length(abstract)==0){NA}else{abstract},
    source = if(length(source)==0){NA}else{source},
    category = if(length(label)==0){NA}else{label})
})))
system.time(both <- t(sapply(test_both,function(x){
  title=sapply(getNodeSet(htmlParse(x),"//div[@class='title']/value"),xmlValue)
  author=try(paste(sapply(getNodeSet(htmlParse(grep("mailto:",x,value=T)),"//a[@href]"),xmlValue),collapse="; "))
  abstract=sapply(getNodeSet(htmlParse(x),"//div[@class='block-record-info']/p[@class='FR_field']"),xmlValue)[[2]]
  source=sapply(getNodeSet(htmlParse(x),"//p[@class='sourceTitle']/value"),xmlValue)
  label=try(grep("Web of Science Categories:",x,value=T))
  label=substr(label,min(nchar(label),57),nchar(label)-4)
  c(title = if(length(title)==0){NA}else{title},
    author = if(class(author)=="try-error"){NA}else{author},
    abstract = if(length(abstract)==0){NA}else{abstract},
    source = if(length(source)==0){NA}else{source},
    category = if(length(label)==0){NA}else{label})
})))

demodata <- rbind(cbind(che[,],"che"),cbind(phy[,],"phy")); dim(demodata)
demodata <- cbind(demodata,ifboth=demodata[,1]%in%both[,1])
demodata[substr(demodata[,2],1,9)=="Error in ",2]<-NA
demodata <- demodata[rowSums(is.na(demodata))==0,]; dim(demodata)
colnames(demodata) <- c("title","abstract","jour","subject","ifboth")

#LDA Test

docs <- demodata[,2]
docs <- lapply(docs,function(x){
  x <- (gsub("[^a-z]","",strsplit(tolower(x)," ")[[1]]))
  tapply(x,x,length)
})
voca <- tapply(unlist(lapply(docs,names)),unlist(lapply(docs,names)),length)
voca <- names(voca)[voca<quantile(voca,0.95)]
docs <- lapply(docs,function(x){
  x <- x[names(x)%in%voca]
  rbind(as.integer(match(names(x),voca)-1),as.integer(x))
})

#k <- function(K){
K = 10
result <- lda.collapsed.gibbs.sampler(docs,K,voca,25,0.1,0.1)
top.words <- top.topic.words(result$topics,50,by.score=T)
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions[is.na(topic.proportions)] <-  1 / K
#  colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
km <- kmeans(topic.proportions,K+20)$cluster
print(t.test(km-1~demodata[,3]))
(sum(km==2&demodata[,3]==names(tapply(km-1,demodata[,3],mean))[tapply(km-1,demodata[,3],mean)==max(tapply(km-1,demodata[,3],mean))])+sum(km!=2&demodata[,3]!=names(tapply(km-1,demodata[,3],mean))[tapply(km-1,demodata[,3],mean)==max(tapply(km-1,demodata[,3],mean))]))/dim(demodata)[1]
#}
tapply((demodata[,3]=="phy")+0,km,mean)

#Though Way - Jour Name

library(MASS)
library(prabclus)

jour <- lapply(demodata[,3],function(x){
  x <- strsplit(tolower(x),"[^a-z]")[[1]]
  x[x!=""]
})
jour.mat <- t(sapply(jour,function(x){unique(unlist(jour))%in%x}))
system.time(jour.vac <- sapply(unique(unlist(jour)),function(x){grepl(x,jour)}))
system.time(jour.j <- 1-jaccard(jour.mat))
system.time(jour.vac <- t(sapply(1:dim(jour.mat)[1],function(i){rowSums(as.matrix(jour.j[,jour.mat[i,]]))})))
system.time(jour.pc <- princomp(jour.vac))
jour.vac2 <- sapply(0:sum(cumsum(sort(jour.pc$sdev/sum(jour.pc$sdev),T))<0.9)+1,function(i){
  jour.pc$loading[,i]%*%t(jour.vac)
})

system.time(jour.lda <- lda(jour.vac[,],demodata[,4]))
mean(paste(predict(jour.lda)$class)==demodata[,4])
system.time(jour.lda <- lda(jour.vac2[,],demodata[,4]))
mean(paste(predict(jour.lda)$class)==demodata[,4])

result <- data.frame(predict(jour.lda)[[2]],predict(jour.lda)$class,demodata[,4:5])
result <- result[result[,5]=="FALSE"&rowSums(result[,1:2]>0.6)==1,]
dim(result)[1]/sum(demodata[,5]=="FALSE");mean(result[,3]==result[,4])

#What about having rpart as classification

jour.rpart <- rpart(demodata[,4]~jour.vac2)
result <- cbind(predict(jour.rpart),demodata[,4])[rowSums(predict(jour.rpart)>0.6)==1&demodata[,5]=="FALSE",]
dim(result)[1]/sum(demodata[,5]=="FALSE");sum(apply(table(result[,3],kmeans(result[,1:2],2)$cluster),2,max))/dim(result)[1]


#Tough Way - docs

docs <- lapply(demodata[,2],function(x){
  x <- strsplit(tolower(x),"[^a-z]")[[1]]
  unique(x[nchar(x)>1])
})
voca <- tapply(unlist(docs),unlist(docs),length)
voca <- voca[voca < quantile(voca,0.75)]
docs <- lapply(docs,function(x){
  x[x%in%names(voca)]
})
system.time(docs.mat <- t(sapply(docs,function(x){print(Sys.time());names(voca)%in%x})))
system.time(docs.j <- jaccard(docs.mat))
docs.j <- 1-docs.j
system.time(docs.vac <- t(sapply(1:length(docs),function(i){
  rowSums(as.matrix(docs.j[,docs.mat[i,]]))
})))

#to be conti

#Docs LatentDA

#############################
#Abstract Jaccard
#############################

test <- sapply(docs,function(x){sum("ray"%in%names(x))})

t.test(docs2[,1]~demodata[train,4])$p.value
t.test(docs2[,2]~demodata[train,4])$p.value
t.test(docs2[,4]~demodata[train,4])$p.value

mean(tapply(docs2[,1],demodata[train,4],var)/tapply(docs2[,1],demodata[train,4],mean))/
  (var(tapply(docs2[,1],demodata[train,4],var))/mean(tapply(docs2[,1],demodata[train,4],var)))
mean(tapply(docs2[,2],demodata[train,4],var)/tapply(docs2[,2],demodata[train,4],mean))/
  (var(tapply(docs2[,2],demodata[train,4],var))/mean(tapply(docs2[,2],demodata[train,4],var)))
mean(tapply(docs2[,4],demodata[train,4],var)/tapply(docs2[,4],demodata[train,4],mean))/
  (var(tapply(docs2[,4],demodata[train,4],var))/mean(tapply(docs2[,4],demodata[train,4],var)))

mean((predict(rpart(demodata[train,4]~docs2[,1]))[,1]==apply(predict(rpart(demodata[train,4]~docs2[,1])),1,max))==(demodata[train,4]=="che"))
mean((predict(rpart(demodata[train,4]~docs2[,2]))[,1]==apply(predict(rpart(demodata[train,4]~docs2[,2])),1,max))==(demodata[train,4]=="che"))
mean((predict(rpart(demodata[train,4]~docs2[,4]))[,1]==apply(predict(rpart(demodata[train,4]~docs2[,4])),1,max))==(demodata[train,4]=="che"))

#######

setwd("~/Documents/Work/sci")
load("~/Documents/Work/sci/temp.RData")

library(MASS)
library(rpart)
library(prabclus)

demodata <- data.frame(cbind(rbind(cbind(che,ref="che"),cbind(phy[!phy[,1]%in%che[,1],],ref="phy"))))
ref <- paste(demodata$ref); ref[demodata[,1]%in%both[,1]]<-"both"; demodata$ref<-ref; rm(ref)

#Label Decompose

system.time(lab <- lapply(demodata$category,function(x){
  x <- gsub(" &amp","",x)
  x <- strsplit(tolower(x),"[^a-z]")[[1]]
  x[nchar(x)>0]
}))
labs <- tapply(unlist(lab),unlist(lab),length)

# system.time(lab2 <- sapply(demodata$category,function(x){
#   x <- gsub(" &amp","",x)
#   x <- strsplit(tolower(x),"; ")[[1]]
#   x[nchar(x)>0]
# }))
# labs2 <- tapply(unlist(lab2),unlist(lab2),length)
# system.time(lab2 <- t(sapply(lab2,function(x){
#   names(labs2)%in%x
# })))
# system.time(lab2.j <- 1-jaccard(lab2))

system.time(lab.mat <- sapply(names(labs),function(x){grepl(x,tolower(demodata$category))}))
colnames(lab.mat)<-names(labs)
system.time(lab.j <- 1-jaccard(lab.mat))
system.time(lab.mat2 <- t(sapply(1:dim(lab.mat)[1],function(i){
  rowSums(as.matrix(lab.j[,lab.mat[i,]]))
})))



#Key Word Decompose

system.time(docs <- lapply(demodata$abstract,function(x){
  x <- strsplit(tolower(x),"[^a-z]")[[1]]
  x <- x[nchar(x)>1]
  tapply(x,x,length)
}))
voca <- tapply(unlist(lapply(docs,names)),unlist(lapply(docs,names)),length)
voca <- voca[voca <= quantile(voca,1)]
system.time(docs <- sapply(docs,function(x){
  paste(names(x),collapse="; ")
}))

system.time(docs_voca.mat <- sapply(names(voca),function(x){grepl(x,docs)}))
system.time(voca.j <- 1-jaccard(docs_voca.mat))

system.time(docs2 <- lapply(demodata$abstract,function(x){
  x <- (strsplit(tolower(x),"[^a-z]")[[1]])
  system.time(x <- x[x%in%names(voca)])
  #system.time(x <- sapply(names(voca),function(x2){sum(grepl(x2,x))}))
  tapply(x,x,length)
}))
system.time(docs2 <- t(sapply(docs2,function(x){
  as.matrix(voca.j[,names(voca)%in%names(x)])%*%x
})))

#

oneway.test(rep(c(1,2,3),each=5)+rnorm(15)/10~paste(rep(1:3,each=5)))


#phy and che

system.time(voca.var <- t(sapply(1:dim(docs2)[2],function(i){
  x <- tapply(docs2[,i],demodata[train,4],median) 
  c(var(x),sd(x)/mean(x),t.test(docs2[,i]~demodata[train,4])$p.value)
})))

docs3 <- docs2[,order(-voca.var[,3])][,1:sum(voca.var[,3]<0.05)]
# head(voca[order(-voca.var[,1])],30)
# head(voca[order(-voca.var[,2])],30)
# head(voca[order(voca.var[,3])],30)
system.time(docs3.pc <- lm(rnorm(dim(docs3)[1])~docs3-1))
docs3 <- docs3[,(!is.na(coef(docs3.pc)))]

system.time(docs2.lda <- lda(docs3,demodata[train,4]))

mean(predict(docs2.lda)$class==demodata[train,4])
mean(predict(docs2.lda)$class[!demodata[,1]%in%both[,1]]==demodata[!demodata[,1]%in%both[,1],4])

#Test Both Label Result

km <- kmeans(predict(docs2.lda)[[2]],3)
km$center; table(km$cluster,demodata[,4]); table(km$cluster,demodata[,1]%in%both[,1])

#Top Words Review

head([,order(voca.var[,1])])

#Closet Modeling


#Holdout Test

system.time(tdocs <- lapply(demodata[-train,2],function(x){
  x <- (strsplit(tolower(x),"[^a-z]")[[1]])
  x <- x[x%in%names(voca)]
  tapply(x,x,length)
}))
system.time(tdocs <- t(sapply(tdocs,function(x){
  as.matrix(voca.j[,names(voca)%in%names(x)])%*%x
})))
tdocs <- tdocs[,order(-voca.var)][,1:sum(voca.var>0)]

mean(predict(docs2.lda,tdocs)$class==demodata[-train,4])
mean(predict(docs2.lda,tdocs)$class[rowSums(predict(docs2.lda,tdocs)[[2]]>0.7)==1]==demodata[-train,4][rowSums(predict(docs2.lda,tdocs)[[2]]>0.7)==1])

#Both Group Test

system.time(tdocs <- lapply(both[,2],function(x){
  x <- (strsplit(tolower(x),"[^a-z]")[[1]])
  x <- x[x%in%names(voca)]
  tapply(x,x,length)
}))
system.time(tdocs <- t(sapply(tdocs,function(x){
  as.matrix(voca.j[,names(voca)%in%names(x)])%*%x
})))
tdocs <- tdocs[,order(-voca.var)][,1:sum(voca.var>0)]
hist(predict(docs2.lda,tdocs)[[2]])



##############################

#PCA

library(graphics)
library(psych)

(pc.cr <- princomp(USArrests))
summary(pc.cr <- princomp(USArrests, cor = TRUE))
biplot(pc.cr)

principal(jour.vac2)




#LDA

data(iris)
library(MASS)
(irislda<-lda(iris[,c(1:4)],iris$Species))
plot(predict(irislda)$x,type="n")
text(predict(irislda)$x,
     substr(levels(predict(irislda)$class),1,2)[predict(irislda)$class],
     col=1:3,cex=1)

sum(predict(irislda)[[1]]==iris[,5])/length(iris[,5])
mean(predict(irislda)[[1]][rowSums(predict(irislda)[[2]]>0.8)==1]==iris[rowSums(predict(irislda)[[2]]>0.8)==1,5])

#LDA T

library(topicmodels)
data("AssociatedPress", package = "topicmodels")
lda <- LDA(AssociatedPress[1:20,], control = list(alpha = 0.1), k = 2)
lda_inf <- posterior(lda, AssociatedPress[21:30,])

#LDA3

library("lda")
library("ggplot2")
library("reshape2")
data(cora.documents)
data(cora.vocab)
data(cora.cites)

K<-10
result <- lda.collapsed.gibbs.sampler(cora.documents,K,cora.vocab,25,0.1,0.1)
top.words <- top.topic.words(result$topics,5,by.score=T)
N<-10
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions <-
  topic.proportions[sample(1:dim(topic.proportions)[1], N),]
topic.proportions[is.na(topic.proportions)] <-  1 / K
colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                   document=factor(1:N)),
                             variable.name="topic",
                             id.vars = "document")  


#########################################
#Test among kmeans rpart and lda
#########################################

mean(paste(predict(lda(iris[,1:4],iris[,5]))$class)==paste(iris[,5]))

fit <- rpart(iris[,5]~iris[,1]+iris[,2]+iris[,3]+iris[,4])
mean(apply(predict(fit),1,function(x){(colnames(predict(fit)))[max(x)==x]})==iris[,5])

sum(apply(table(iris[,5],kmeans(iris[,-5],3)$cluster),2,max))/dim(iris)[1]

###########################################################################################################################
###########################################################################################################################

#########################################
# Process Data
#########################################

load("~/Documents/Work/sci/2014 fd che phy.RData")
library(XML)

test_phy <- c(test_phy1,test_phy2,test_phy3)
test_che <- c(test_che1,test_che2,test_che3,test_che4,test_che5,test_che6,test_che7,test_che8,test_che9)
test_both <- c(test_both1,test_both2,test_both3)
rm(test_phy1,test_phy2,test_phy3,test_che1,test_che2,test_che3,test_che4,test_che5,test_che6,test_che7,test_che8,test_che9,test_both1,test_both2,test_both3,temp,cl,i,url,url0)

system.time(both <- t(sapply(test_both,function(x){
  title=sapply(getNodeSet(htmlParse(x),"//div[@class='title']/value"),xmlValue)
  #  author=try(paste(sapply(getNodeSet(htmlParse(grep("mailto:",x,value=T)),"//a[@href]"),xmlValue),collapse="; "))
  author=grep("reprint author",x,value=T)
  author=paste(substr(author,regexpr("</span>",author)+7,regexpr("(reprint author)",author)-3),collapse="; ")
  author2=grep(paste0("alt=\"Find more records by this author\">",author),x,value=T)
  author2=substr(author2,regexpr(paste0(">",author),author2)+nchar(author)+7,regexpr(")<sup>",author2)-1)
  author2=paste(author2,collapse="; ")
  abstract=sapply(getNodeSet(htmlParse(x),"//div[@class='block-record-info']/p[@class='FR_field']"),xmlValue)[[2]]
  source=sapply(getNodeSet(htmlParse(x),"//p[@class='sourceTitle']/value"),xmlValue)
  label=try(grep("Web of Science Categories:",x,value=T))
  label=substr(label,min(nchar(label),57),nchar(label)-4)
  select=length(grep("<div class=\"title3\">Abstract</div>",x))>0
  c(title = if(length(title)==0){NA}else{title},
    author = if(length(author)==0){NA}else{author},
    author2 = if(length(author2)==0){NA}else{author2},
    abstract = if(length(abstract)==0){NA}else{abstract},
    source = if(length(source)==0){NA}else{source},
    category = if(length(label)==0){NA}else{label},
    select)
})))

system.time(phy <- t(sapply(test_phy,function(x){
  title=sapply(getNodeSet(htmlParse(x),"//div[@class='title']/value"),xmlValue)
  #  author=try(paste(sapply(getNodeSet(htmlParse(grep("mailto:",x,value=T)),"//a[@href]"),xmlValue),collapse="; "))
  author=grep("reprint author",x,value=T)
  author=paste(substr(author,regexpr("</span>",author)+7,regexpr("(reprint author)",author)-3),collapse="; ")
  author2=grep(paste0("alt=\"Find more records by this author\">",author),x,value=T)
  author2=substr(author2,regexpr(paste0(">",author),author2)+nchar(author)+7,regexpr(")<sup>",author2)-1)
  author2=paste(author2,collapse="; ")
  abstract=sapply(getNodeSet(htmlParse(x),"//div[@class='block-record-info']/p[@class='FR_field']"),xmlValue)[[2]]
  source=sapply(getNodeSet(htmlParse(x),"//p[@class='sourceTitle']/value"),xmlValue)
  label=try(grep("Web of Science Categories:",x,value=T))
  label=substr(label,min(nchar(label),57),nchar(label)-4)
  select=length(grep("<div class=\"title3\">Abstract</div>",x))>0
  c(title = if(length(title)==0){NA}else{title},
    author = if(length(author)==0){NA}else{author},
    author2 = if(length(author2)==0){NA}else{author2},
    abstract = if(length(abstract)==0){NA}else{abstract},
    source = if(length(source)==0){NA}else{source},
    category = if(length(label)==0){NA}else{label},
    select)
})))

system.time(che <- t(sapply(test_che,function(x){
  title=sapply(getNodeSet(htmlParse(x),"//div[@class='title']/value"),xmlValue)
  #  author=try(paste(sapply(getNodeSet(htmlParse(grep("mailto:",x,value=T)),"//a[@href]"),xmlValue),collapse="; "))
  author=grep("reprint author",x,value=T)
  author=paste(substr(author,regexpr("</span>",author)+7,regexpr("(reprint author)",author)-3),collapse="; ")
  author2=grep(paste0("alt=\"Find more records by this author\">",author),x,value=T)
  author2=substr(author2,regexpr(paste0(">",author),author2)+nchar(author)+7,regexpr(")<sup>",author2)-1)
  author2=paste(author2,collapse="; ")
  abstract=sapply(getNodeSet(htmlParse(x),"//div[@class='block-record-info']/p[@class='FR_field']"),xmlValue)[[2]]
  source=sapply(getNodeSet(htmlParse(x),"//p[@class='sourceTitle']/value"),xmlValue)
  label=try(grep("Web of Science Categories:",x,value=T))
  label=substr(label,min(nchar(label),57),nchar(label)-4)
  select=length(grep("<div class=\"title3\">Abstract</div>",x))>0
  c(title = if(length(title)==0){NA}else{title},
    author = if(length(author)==0){NA}else{author},
    author2 = if(length(author2)==0){NA}else{author2},
    abstract = if(length(abstract)==0){NA}else{abstract},
    source = if(length(source)==0){NA}else{source},
    category = if(length(label)==0){NA}else{label},
    select)
})))

tail(sort(tapply(data.frame(che)$author2,data.frame(che)$author2,length)),10)
tail(sort(tapply(data.frame(phy)$author2,data.frame(phy)$author2,length)),10)
sort(tapply(data.frame(both)$author2,data.frame(both)$author2,length))

#########################################
# Demo
#########################################

load("~/Documents/Work/sci/demo.RData")

library(MASS)
library(rpart)
library(prabclus)

############################
# Physics Chemistry Classification
############################

load("~/Documents/Work/sci/tempraw.RData")
demodata <- data.frame(cbind(rbind(cbind(che,ref="che"),cbind(phy,ref="phy"))))

system.time(docs <- lapply(demodata$abstract,function(x){
  x <- strsplit(tolower(x),"[^a-z]")[[1]]
  x <- x[nchar(x)>1]
  tapply(x,x,length)
}))
voca <- tapply(unlist(lapply(docs,names)),unlist(lapply(docs,names)),length)
voca <- voca[voca <= quantile(voca,1)]
system.time(docs <- sapply(docs,function(x){
  paste(names(x),collapse="; ")
}))

system.time(docs_voca.mat <- sapply(names(voca),function(x){grepl(x,docs)}))
system.time(voca.j <- 1-jaccard(docs_voca.mat))

system.time(docs2 <- lapply(demodata$abstract,function(x){
  x <- (strsplit(tolower(x),"[^a-z]")[[1]])
  system.time(x <- x[x%in%names(voca)])
  #system.time(x <- sapply(names(voca),function(x2){sum(grepl(x2,x))}))
  tapply(x,x,length)
}))
system.time(docs2 <- t(sapply(docs2,function(x){
  as.matrix(voca.j[,names(voca)%in%names(x)])%*%x
})))

system.time(voca.var <- t(sapply(1:dim(docs2)[2],function(i){
  x <- tapply(docs2[,i],demodata$ref,median) 
  c(var(x),sd(x)/mean(x),t.test(docs2[,i]~demodata$ref)$p.value,oneway.test(docs2[,i]~demodata$ref)$p.value)
})))

docs3 <- docs2[,order(voca.var[,4])][,1:sum(voca.var[,4]<0.05)]

#PComp

system.time(docs3.pc <- lm(rnorm(dim(docs3)[1])~docs3-1))
docs3 <- docs3[,(!is.na(coef(docs3.pc)))]
voca3 <- names(voca)[order(voca.var[,4])][1:sum(voca.var[,4]<0.05)][(!is.na(coef(docs3.pc)))]

# system.time(docs3.pc2 <- princomp(docs3[,1:dim(docs3)[1]]))
# system.time(docs3 <- sapply(1:sum(cumsum(docs3.pc2[[1]]/sum(docs3.pc2[[1]]))<0.90),function(i){
#   as.numeric(docs3.pc2$scores[,i]%*%docs3[,1:dim(docs3)[1]])
# }))

system.time(docs2.lda <- lda(docs3,demodata$ref))

#Demo

load("/Users/hzxsdtcgigi4/Documents/Work/sci/demo.RData")
library(MASS)
library(rpart)
library(prabclus)
table(predict(docs2.lda)$class,paste(demodata$ref,demodata$title %in% names(tapply(demodata$title,demodata$title,length))[tapply(demodata$title,demodata$title,length)==2]))
table(kmeans(predict(docs2.lda)[[2]],3)$cluster,paste(demodata$ref,
                                                      demodata$title %in% names(tapply(demodata$title,demodata$title,length))[tapply(demodata$title,demodata$title,length)==2]
))

#Keyword

x <- "organic"
x.voca <- voca.j[,colnames(voca.j)==x]
x.voca[x.voca<0.05] <- 0
voca3 <- names(voca)[order(voca.var[,4])][1:sum(voca.var[,4]<0.05)][(!is.na(coef(docs3.pc)))]
x.voca <- x.voca[names(x.voca)%in%voca3]

if(length(x.voca)>0){
  x.docs <- docs3[,voca3%in%names(x.voca)]
  x.docs <- t(t(x.docs)*as.numeric(x.voca[match(voca3,names(x.voca))]))
  x.docs <- rep(rowSums(x.docs),sapply(strsplit(paste(demodata$author),"; "),length))
  x.docs <- data.frame(author=unlist(strsplit(paste(demodata$author),"; "),),score=x.docs)[x.docs>mean(x.docs),]
  x.docs[x.docs[,2]>mean(x.docs[,2]) + 1.6 * sd(x.docs[,2]),2]<-mean(x.docs[,2]) + 1.6 * sd(x.docs[,2])
  x.docs[x.docs[,2]<mean(x.docs[,2]) - 1.6 * sd(x.docs[,2]),2]<-mean(x.docs[,2]) - 1.6 * sd(x.docs[,2])
  head(-sort(-tapply(x.docs[,2],x.docs[,1],sum)),20)
  head(-sort(-tapply(x.docs[,2],x.docs[,1],length)),20)
  
} else {"no record"}

##################################
# Word Cloud
##################################

#load("/Users/hzxsdtcgigi4/Documents/Work/sci/demo.RData")
setwd("C:/Users/WenluluSens/Documents/project/scientists")
load("demo.RData")

rm(demodata,docs2,docs3,docs_voca.mat,voca.j,voca.var,docs,docs2.lda,docs3.pc,voca)
library(MASS)
library(wordcloud)
demodata <- data.frame(rbind(cbind(che[!che[,1]%in%phy[,1],],ref="che"),
                             cbind(phy[!phy[,1]%in%che[,1],],ref="phy"),
                             cbind(phy[phy[,1]%in%che[,1],],ref="both")))

#Keyword
key <- "solgel MATERIAL"
key <- strsplit(toupper(key)," ")[[1]]
score <- sapply(key,function(x){grepl(x,gsub("[^A-Z| ]","",toupper(paste(demodata$title,demodata$abstract))))})
score <- data.frame(author=paste(demodata$author),score)[rowSums(score)>0,]
score <- data.frame(score,score=sapply(1:dim(score)[1],function(x){(sum(as.numeric(score[x,-1]))/(dim(score)[2]-1))^1.5}))
#score <- data.frame(score,score=sapply(1:dim(score)[1],function(i){choose(sum(as.numeric(score[i,-1])),2)/choose(length(as.numeric(score[i,-1])),2)}))
author <- strsplit(paste(score$author),"; ")
score <- sapply(0:length(key)+2,function(i){tapply(rep(score[,i],sapply(author,length)),unlist(author),sum)})
score <- data.frame(score,sapply(1:dim(score)[1],function(i){prod(as.numeric(score[i,-length(key)-1]))})                      )
#score[,length(key)+2] <- score[,length(key)+1]+scale(score[,length(key)+2])
score[,length(key)+2] <- score[,length(key)+2]*mean(score[,length(key)+2])/mean(score[,length(key)+1])
colnames(score) <- c(key,"score","adjscore")
head(score[order(-score[,length(key)+2]),],20)

#Lab keyword
key <- "dyzhao@fudan.edu.cn"
doc <- paste(demodata$title,demodata$abstract)[grepl(key,paste(demodata$author))]
voca <- lapply(doc,function(x){
  unique(unique(gsub("[^A-Z]","",strsplit(toupper(x)," ")[[1]])))
})
voca <- tapply(unlist(voca),unlist(voca),length)/length(voca)
system.time(voca_bk <- t(sapply(names(voca),function(x){grepl(x,gsub("[^A-Z| ]","",toupper(paste(demodata$title,demodata$abstract)[!grepl(key,paste(demodata$author))])))})))
voca2 <- voca/rowSums(voca_bk)*dim(voca_bk)[2]
wordcloud(names(voca[voca2>1&voca2!=Inf]),voca[voca2>1&voca2!=Inf],colors=brewer.pal(8,"Dark2"))
wordcloud(names(voca[voca2>1&voca2!=Inf]),(voca/voca2)[voca2>1&voca2!=Inf],colors=brewer.pal(8,"Dark2"))

#Lab Label
cats <- tapply(unlist(strsplit(paste(demodata$category[grepl(key,paste(demodata$author))]),"; ")),
               unlist(strsplit(paste(demodata$category[grepl(key,paste(demodata$author))]),"; ")),
               length)
cats
pie(cats)
