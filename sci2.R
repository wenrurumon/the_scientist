
library(XML)
library(wordcloud)

############################
#Data Process
############################

# load("D:/Projects/sci/old/2014 fd che phy.RData")
# che <- c(test_che1,test_che2,test_che3,test_che4,test_che5,test_che6,test_che7,test_che8,test_che9)
# phy <- c(test_phy1,test_phy2,test_phy3)
# rm(i,temp,test_both1,test_both2,test_both3,test_che1,test_che2,test_che3,test_che4,test_che5,test_che6,test_che7,test_che8,test_che9,
#    test_phy1,test_phy2,test_phy3,url,cl,url0)
# 
# system.time(demodata <- t(sapply(c(phy,che),function(x){
#   xp <- sapply(getNodeSet(htmlParse(x),"//p[@class='FR_field']"),xmlValue)
#   dinfo <- sapply(getNodeSet(htmlParse(x),"//div[@class='block-record-info']/p[@class='FR_field']"),xmlValue)
#   title <- sapply(getNodeSet(htmlParse(x),"//div[@class='title']/value"),xmlValue)
#   if (length(title)==0) {title="missing"}
#   rpa <- gsub("\nReprint Address:\n| |\\(reprint author\\)","",grep("reprint address",xp,ignore.case=T,value=T))
#   if (length(rpa)==0) {rpa="missing"}
#   mail <- gsub("\nE-mail Addresses:|\n","",grep("E-mail",xp,ignore.case=T,value=T))
#   if (length(mail)==0) {mail="missing"}
#   org <- sapply(getNodeSet(htmlParse(x),"//td[@class='fr_address_row2']"),xmlValue)[1:length(rpa)]
#   if (length(org)==0) {org="missing"}
#   keyw <- paste(gsub("\nKeyWords Plus:|\n|Author Keywords:","",grep("KeyWords",xp,ignore.case=F,value=T)),collapse="; ")
#   if (length(keyw)==0) {keyw="missing"}
#   abstract <- dinfo[2]
#   stitle <- sapply(getNodeSet(htmlParse(x),"//p[@class='sourceTitle']/value"),xmlValue)
#   if (length(stitle)==0) {stitle="missing"}
#   cats <- gsub("\n|Research Domain ","",grep("Research Domain",xp,ignore.case=T,value=T))
#   if (length(cats)==0) {cats="missing"}
#   wcats <- gsub("Web of Science Categories:|\n","",grep("Web of Science Categories:",xp,ignore.case=T,value=T))
#   if (length(wcats)==0) {wcats="missing"}
#   pubdate <- gsub("\n|Published:","",grep("\nPublished:\n",xp,ignore.case=T,value=T))
#   if (length(pubdate)==0) {pubdate="missing"}
#   mails <- substr(strsplit(mail,"; ")[[1]],rep(1,length(strsplit(mail,"; ")[[1]])),regexpr("@",strsplit(mail,"; ")[[1]])-1)
#   if (length(mails)<=1) {mail2 <- mail} else {
#     mail2 <- sapply(rpa,function(xrpa){
#       strsplit(mail,"; ")[[1]][order(-rowSums(sapply(sapply(2:nchar(xrpa)-1,function(i){substr(xrpa,i,i+1)}),function(x){grepl(x,mails,ignore.case=T)})))[1]]
#     })
#     mail2 <- paste(mail2,collapse="; ")
#   }
#   gsub("\n"," ",c(title,keyw,abstract,stitle,cats,wcats,rpa,mail2,org,pubdate))
# })))
# colnames(demodata) <- c("title","keyw","abstract","stitle","cats","wcats","author","mail","affiliation","pubdate")
# demodata <- data.frame(demodata)

############################
#Load
############################

setwd("C:/Users/WenluluSens/Documents/Project/Scientists")
load("sci2.Rdata")


############################
#Lab
############################

key <- grepl("dyzhao@fudan.edu.cn",demodata$mail)

#Lab keyword

all_keyw <- lapply(paste(demodata$keyw),function(x){
  x1 <- gsub("[^a-z]"," ",tolower(strsplit(x," |; ")[[1]]))
  x2 <- gsub("[^a-z]"," ",tolower(strsplit(x,"; ")[[1]]))
  c(x1[nchar(x1)>1],x2[nchar(x2)>1])
})

keyw1 <- tapply(unlist(all_keyw[key]),unlist(all_keyw[key]),length)
keyw2 <- tapply(unlist(all_keyw),unlist(all_keyw),length)
keyw1 <- -sort((-keyw1/keyw2[match(names(keyw1),names(keyw2))]*100-keyw1/length(all_keyw))[nchar(names(keyw1))>1])

head(keyw1,20)
wordcloud(names(keyw1),keyw1,colors=brewer.pal(8,"Dark2"))

#Lab Category

all_catw <- lapply(paste(demodata$wcat),function(x){
  x <- strsplit(x,"; ")[[1]]
  x[(x)!="missing"]
})

catw1 <- -sort(-tapply(unlist(all_catw[key]),unlist(all_catw[key]),length))
catw1 <- catw1 / sum(catw1)
names(catw1)[-1:-(sum(cumsum(catw1)<0.9)+1)] <- "Others"
catw1 <- -sort(-tapply(catw1,names(catw1),sum))
pie(catw1)

############################
#Keyword
############################

key <- "anode carbon"

#Key Category

all_catw <- lapply(paste(demodata$wcat),function(x){
  #x <- strsplit(x,"; |, ")[[1]]
  x <- strsplit(x,"; ")[[1]]
  x[x!="missing"]
})

catw2 <- lapply(strsplit(tolower(key)," ")[[1]],function(key){
  all_catw[grepl(key,gsub("[^a-z]| ","",tolower(paste(demodata$abstract,demodata$title,demodata$keyw))))]
})
catw2 <- -sort(-tapply(unlist(catw2),unlist(catw2),length)/length(unlist(catw2)))
names(catw2)[-1:-(sum(cumsum(catw2)<0.8)+1)] <- "Others"
catw2 <- -sort(-tapply(catw2,names(catw2),sum))
pie(catw2)

#Key Lab

score <- sapply(strsplit(tolower(key)," ")[[1]],function(x){
  grepl(x,gsub("[^a-z]| ","",tolower(paste(demodata$abstract,demodata$title,demodata$keyw))))
})
score1 <- choose(rowSums(score),2)
score <- c(lapply(strsplit(tolower(key)," ")[[1]],function(x){
  grepl(x,tolower(paste(demodata$abstract,demodata$title,demodata$keyw)))
}),list(score1))

score <- sapply(score,function(x){
  tapply(x,toupper(paste(demodata$mail)),sum)
})
score <- cbind(score,exp(rowSums(cbind(log(score+1)[,-dim(score)[2]+1])))-1)
score <- score[order(-exp(rowSums(log(score+1))-1)),]
#score <- score[rowSums(score)>0,]

head(score,20)

