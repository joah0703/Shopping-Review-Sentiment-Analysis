# R code
library(readxl)
library(tm)
library(stringr)

ko.words=function(doc){
  d=str_split(doc,';')[[1]]
  
  extracted=tolower(str_match(d,'([가-힣a-zA-Z]+/[NVO]'))
  keyword=extracted[,2]
  
  keyword[!is.na(keyword)]
}

category=list("패션잡화", "화장미용", "디지털가전", "가구인테리어", "출산육아", "스포츠레저", "식품", "생활건강")
neg_dic_all=c()
pos_dic_all=c()


#######첫번째 카테고리 사전 구축#######
####형태소 분석####
neg_fashion=read_excel("C:\\navershopping\\negative_패션의류.xlsx",col_names=F)
pos_fashion=read_excel("C:\\navershopping\\positive_패션의류.xlsx",col_names=F)

options(mc.cores=1)
neg_cps=Corpus(VectorSource(neg_fashion))
pos_cps=Corpus(VectorSource(pos_fashion))
neg_tdm=TermDocumentMatrix(neg_cps,control = list(tokenize=ko.words,
                                                  removePunctuation=T,
                                                  removeNumbers=T,
                                                  wordLengths=c(2,Inf)))
pos_tdm=TermDocumentMatrix(pos_cps,control = list(tokenize=ko.words,
                                                  removePunctuation=T,
                                                  removeNumbers=T,
                                                  wordLengths=c(2,Inf)))
Encoding(neg_tdm$dimnames$Terms) = 'EUC-KR'
Encoding(pos_tdm$dimnames$Terms) = 'EUC-KR'
neg_tdm.matrix=as.matrix(neg_tdm)                       
pos_tdm.matrix=as.matrix(pos_tdm)

neg_word.count=rowSums(neg_tdm.matrix)
pos_word.count=rowSums(pos_tdm.matrix)
neg_word.order=order(neg_word.count,decreasing = T)
pos_word.order=order(pos_word.count,decreasing = T)
neg_freq.word=neg_tdm.matrix[neg_word.order[1:100],]
pos_freq.word=pos_tdm.matrix[pos_word.order[1:100],]
neg_words=rownames(neg_tdm.matrix)[neg_word.order[1:100]]
pos_words=rownames(pos_tdm.matrix)[pos_word.order[1:100]]


####사전 구축####
neg_samefactor=c()
pos_samefactor=c()
for(i in 1:100){
  for(j in 1:100){
    if(neg_words[i]==pos_words[j]){
      neg_samefactor=append(neg_samefactor,i)
      pos_samefactor=append(pos_samefactor,j)
    }
  }
}
neg_dic=neg_words[-neg_samefactor]
pos_dic=pos_words[-pos_samefactor]

neg_dic_all=append(neg_dic_all,neg_dic)
pos_dic_all=append(pos_dic_all,pos_dic)



#######나머지 카테고리 사전 추가 구축#######
for(k in 1:length(category)){
  ####형태소 분석####
  neg_fashion=read_excel(paste0("C:\\navershopping\\negative_",category[k],".xlsx"),col_names=F)
  pos_fashion=read_excel(paste0("C:\\navershopping\\positive_",category[k],".xlsx"),col_names=F)
  
  options(mc.cores=1)
  neg_cps=Corpus(VectorSource(neg_fashion))
  pos_cps=Corpus(VectorSource(pos_fashion))
  neg_tdm=TermDocumentMatrix(neg_cps,control = list(tokenize=ko.words,
                                                    removePunctuation=T,
                                                    removeNumbers=T,
                                                    wordLengths=c(2,Inf)))
  pos_tdm=TermDocumentMatrix(pos_cps,control = list(tokenize=ko.words,
                                                    removePunctuation=T,
                                                    removeNumbers=T,
                                                    wordLengths=c(2,Inf)))
  Encoding(neg_tdm$dimnames$Terms) = 'EUC-KR'
  Encoding(pos_tdm$dimnames$Terms) = 'EUC-KR'
  neg_tdm.matrix=as.matrix(neg_tdm)                       
  pos_tdm.matrix=as.matrix(pos_tdm)
  
  neg_word.count=rowSums(neg_tdm.matrix)
  pos_word.count=rowSums(pos_tdm.matrix)
  neg_word.order=order(neg_word.count,decreasing = T)
  pos_word.order=order(pos_word.count,decreasing = T)
  neg_freq.word=neg_tdm.matrix[neg_word.order[1:100],]
  pos_freq.word=pos_tdm.matrix[pos_word.order[1:100],]
  neg_words=rownames(neg_tdm.matrix)[neg_word.order[1:100]]
  pos_words=rownames(pos_tdm.matrix)[pos_word.order[1:100]]
  
  
  ####사전 추가 구축####
  neg_samefactor=c()
  pos_samefactor=c()
  for(i in 1:100){
    for(j in 1:100){
      if(neg_words[i]==pos_words[j]){
        neg_samefactor=append(neg_samefactor,i)
        pos_samefactor=append(pos_samefactor,j)
      }
    }
  }
  neg_dic=neg_words[-neg_samefactor]
  pos_dic=pos_words[-pos_samefactor]
  
  neg_dic_all=append(neg_dic_all,neg_dic)
  pos_dic_all=append(pos_dic_all,pos_dic)
  
  neg_samefactor=c()
  pos_samefactor=c()
  for(i in 1:length(neg_dic_all)){
    for(j in 1:length(neg_dic)){
      if(neg_dic_all[i]==neg_dic[j])
        neg_samefactor=append(neg_samefactor,j)
    }
  }
  if(is.null(neg_samefactor)!=TRUE){
    neg_dic=neg_dic[-neg_samefactor]
    neg_dic_all=append(neg_dic_all,neg_dic)
  }
  
  for(i in 1:length(pos_dic_all)){
    for(j in 1:length(pos_dic)){
      if(pos_dic_all[i]==pos_dic[j])
        pos_samefactor=append(pos_samefactor,j)
    }
  }
  if(is.null(pos_samefactor)!=TRUE){
    pos_dic=pos_dic[-pos_samefactor]
    pos_dic_all=append(pos_dic_all,pos_dic)
  }
  
}



#####최종 사전 저장##### 
write.table(neg_dic_all,"C:\\Users\\user\\Desktop\\분산처리\\dictionary\\neg_dictionary.txt",row.names = F,quote = F,append = T)
write.table(pos_dic_all,"C:\\Users\\user\\Desktop\\분산처리\\dictionary\\pos_dictionary.txt",row.names = F,quote = F,append = T)
##기존 사전과 합치기##
positive=readLines("C:\\Users\\user\\Desktop\\분산처리\\positive.txt",encoding = 'UTF-8')
write.table(positive,"C:\\Users\\user\\Desktop\\분산처리\\dictionary\\pos_dictionary.txt",row.names = F,quote = F,append = T)
negative=readLines("C:\\Users\\user\\Desktop\\분산처리\\negative.txt",encoding = 'UTF-8')
write.table(negative,"C:\\Users\\user\\Desktop\\분산처리\\dictionary\\neg_dictionary.txt",row.names = F,quote = F,append = T)
