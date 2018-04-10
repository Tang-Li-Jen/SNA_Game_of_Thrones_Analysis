install.packages('igraph')
library(dplyr)
library(igraph)
#df<-read.csv("https://www.macalester.edu/%7Eabeverid/data/stormofswords.csv")
book<-read.csv("https://raw.githubusercontent.com/pombredanne/GoT-SNA/master/data/chapters.csv")
role<-read.csv("https://raw.githubusercontent.com/pombredanne/GoT-SNA/master/data/characters.csv")
event<-read.csv("https://raw.githubusercontent.com/pombredanne/GoT-SNA/master/data/events.csv")
main<-unique(chapt_index$povID)
chapt_index<-merge(book,role,by.x = 'povID',by.y = 'characterID')[,c(1,2,3,4,7)]
role= role[role$Name!='Paxter Redwyne',]
View(table(chapt_index$Name))


#此function之目的為：每個章節產生undirected之from-to資料集
ft<-function(human,index){
  x<-data.frame(table(human[[index]]))
  x<-x[order(x[,1]),]
  out<-data.frame()
  t<-0
  for(i in 1:(length(x$Var1)-1)){
    for(j in (i+1):length(x$Var1)){
      t<-sum(t,1)
      out[t,1]<-x$Var1[i] #from
      out[t,2]<-x$Var1[j] #to
      out[t,3]<-x$Freq[i]
    }
  }
  return(out)
}

sna<-function(i,final){
  #第i個主要角色的全部POV章節以及被提及的所有角色
  test<-merge(event,book[book$povID==i,],by.x = 'chapterID',by.y = 'ID') %>% filter(.,event=="mentioned")
  #該主要角色屬於哪幾章節
  l<-unique(test$chapterID)
  human<-list()
  for(i in 1:length(l)){
    #每一個章節分別提到哪些人
    human[[i]]<-as.numeric(test[test$chapterID==l[i],2])
  }
  final<-data.frame()
  for (i in 1:length(human)) {
    #產生資料集ai，並將他們垂直合併
    #final<-rbind(final,ft(i))
    final<-rbind(final,assign(paste0("a", i), ft(human,i)))
  }
  return(final)
}
#產生全部的from-to資料集
sna_final<-data.frame()
for(i in main){
  sna_final<-rbind(sna_final,sna(i,t))
}

#將重複的關係加總
total_sna<-aggregate(V3~.,data=sna_final,sum)

#產生Gephi Edge資料集
total_sna2<-merge(role[,c(1,2)],total_sna,by.x = 'characterID',by.y = 'V1')
total_sna3<-merge(role[,c(1,2)],total_sna2,by.x = 'characterID',by.y = 'V2')
sna_output<-total_sna3[,c(2,4,5)]
colnames(sna_output)<-c('Source','Target','Weight')
sna_output$Type<-"Undirected"
write.csv(sna_output,'/Users/charlie/Desktop/GOT.csv')

#Gephi Node資料集
node<-role
node$Id<-node$Name
names(node)[2]<-'Label'
names(node)
write.csv(node,'/Users/charlie/Desktop/node.csv')

#產生距離矩陣
g <- graph.data.frame(sna_output, directed=FALSE)
g_matrix<-get.adjacency(g, type="both", attr="Weight")
write.csv(as.matrix(g_matrix),'/Users/charlie/Desktop/g_matrix.csv')

table()




#pov test
#setwd("C:\\Users\\Tom\\Desktop\\SNA\\final\\jon")
library(igraph)
library(data.table)
library(dplyr)
library(reshape2)
chapt<-fread('chapters.csv')
View(chapt[,c(2,3,5,6)])
f<-c('1716','851')
chapt2<-filter(chapt,povID %in% f)
jon<-chapt2[chapt2$povID=='851',c(1,2,3,6)]
tyrion<-chapt2[chapt2$povID=='1716',c(2,3,6)]
evnt<-fread('events.csv')  

j<-merge(evnt,jon,by.x='chapterID',by.y = 'ID') %>% filter(.,event=="mentioned")
ls<-unique(j$chapterID)
#j1<-j[j$chapterID=='3',]
#snow<-list(j[j$chapterID=='3',],j[j$chapterID=='6',])
snow<-list()
for(i in 1:length(ls)){
  snow[[i]]<-as.numeric(j[j$chapterID==ls[i],2])
}
length(snow)
#snow[[27]]
#j35<-j[j$chapterID=='79',]

#²£¥Ífrom-to ¸ê®Æ¶°


#for(i in 1:length(snow)){
#  
#}
#a<-table(j1$characterID)
#a<-data.frame(a)
ft<-function(index){
  x<-data.frame(table(snow[[index]]))
  out<-data.frame()
  t<-0
  for(i in 1:(length(x$Var1)-1)){
    for(j in (i+1):length(x$Var1)){
      t<-sum(t,1)
      out[t,1]<-x$Var1[i] #from
      out[t,2]<-x$Var1[j] #to
      out[t,3]<-x$Freq[i]
    }
  }
  return(out)
}
final<-data.frame()
for (i in 1:42) {
  final<-rbind(final,assign(paste0("a", i), ft(i)))
}

jon_snow<-aggregate(V3~.,data=final,sum)

role<-read.csv('characters.csv')

#role2<-read.csv('role.csv')
#role2<-cbind(role2,role[,c(4,5,6)])

role<-role[,c(1:6)]
role$Label<-role$Name


#colnames(role)<-c('Id','Team','Label')
write.csv(role,'role.csv')

jon_snow2<-merge(role[,c(1,2)],jon_snow,by.x = 'characterID',by.y = 'V1')
jon_snow3<-merge(role[,c(1,2)],jon_snow2,by.x = 'characterID',by.y = 'V2')
jon_snow<-jon_snow3[,c(2,4,5)]
colnames(jon_snow)<-c('From','To','Weight')
jon_snow$Label<-jon_snow$Source
colnames(jon_snow)<-c('Source','Target','Weight')
write.csv(jon_snow,'jon_snow.csv')

#a<-ft(35)
#²£¥Í¶ZÂ÷¯x°}

jon_snow<-read.csv('jon_snow.csv')
jon_snow<-jon_snow[,c(2,3,4)]
g <- graph.data.frame(jon_snow, directed=FALSE)
g_matrix<-get.adjacency(g, type="both", attr="Weight")
write.csv(as.matrix(g_matrix),'g_matrix.csv')


#¦P·ù°ÝÃD
alian<-read.csv('Alian.csv')
role<-read.csv(("role.csv"))
test<-merge(alian,role,by = 'Team')
write.csv(test,'role.csv')



#transform to ucinet
role<-read.csv('role.csv')
match<-read.csv('match.csv')
edge<-read.csv('jon_snow.csv')
test<-merge(match,role,by.x='Id',by.y = 'Name')
write.csv(test,'role_uci.csv')

#E-I
a<-read.csv('role_uci.csv')
for(i in 1: nrow(a)){
  if(a[i,'Alian'] == 0){a[i,'Alian'] =1}else{a[i,'Alian']=2}
}
write.csv(a,'role_uci.csv')

a<-read.csv('role_uci.csv')
b<-read.csv('Coreness2.csv')
c<-read.csv('CorenessPart2.csv')
a2<-merge(a,b,by='Id')
a3<-merge(a2,c,by='Id')
write.csv(a3,'role_final.csv')
a<-read.csv('role_final.csv')
b<-read.csv('EI.csv')
a2<-merge(a,b,by='Id')

for(i in 1: nrow(a2)){
  if(a2[i,'E.I'] > 0){a2[i,'E'] =1}else{a2[i,'E']=0}
}

write.csv(a2,'role_final.csv')


#Is Jon snow dead god?
evnt<-fread('events.csv')  
evnt2<-evnt[evnt$event=='killed',c(2,3)]
evnt2$dead<-1
role<-read.csv('role_final.csv')
cha<-read.csv('characters.csv')
cha<-cha[,c(1,2)]
cha2<-merge(cha,evnt2,by='characterID')
cha2<-cha2[,c(2,4)]
role2<-merge(role,cha2,by.x='Id',by.y='Name',all.x = TRUE)
role2[is.na(role2$dead),'dead']<-0
unique(role2$Id)
write.csv(role2,'role_final2.csv')
jon<-read.csv('jon_snow.csv')

#glm
jon<-read.csv('glm.csv')
jon$alian<-as.factor(jon$alian)
t<-model.matrix(~alian,data=jon)
t<-data.frame(t)
jon<-cbind(jon,t)

jon$part<-as.factor(jon$part)
jon$dead<-as.factor(jon$dead)
t<-model.matrix(~part,data=jon)
t<-data.frame(t)
jon<-cbind(jon,t)
write.csv(jon,'glm.csv')
jon<-jon[-40,]
model<-glm(dead~e.i+alian2+part+Freq,data=jon,family=binomial(link='logit'))
summary(model)
model$fitted.values
test<-data.frame()
for(i in 1:length(snow)) {
  test<-rbind(test,data.frame(snow[[i]]))  
}
test<-data.frame(table(test))
test2<-merge(test,cha,by.x='test',by.y = 'characterID')
test2<-test2[,c(2,3)]
jon<-merge(jon,test2,by.x='id',by.y='Name',all.x = TRUE)

#glm2
jon<-read.csv('glm2.csv')
model<-glm(dead~e.i+alian2+part+Freq,data=jon,family=binomial(link='logit'))
