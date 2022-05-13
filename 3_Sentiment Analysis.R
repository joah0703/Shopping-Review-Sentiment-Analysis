# R code
library(readxl)
library(tm)
library(stringr)
library(plyr)

ko.words=function(doc){
  d=str_split(doc,';')[[1]]
  
  extracted=tolower(str_match(d,'([가-힣a-zA-Z]+/[NVO]'))
  keyword=extracted[,2]
  
  keyword[!is.na(keyword)]
}
#만든 사전 가져오기
posdic=scan("C:/Users/user/Desktop/분산처리/dictionary/raw_pos_dictionary.txt",skip=1,what="chracter",sep = "\n")
negdic=scan("C:/Users/user/Desktop/분산처리/dictionary/raw_neg_dictionary.txt",skip=1,what="chracter",sep = "\n")



###test해보기
pos_score=array()
neg_score=array()
score=array()
category=list("패션의류","패션잡화", "화장미용", "디지털가전", "가구인테리어", "출산육아", "스포츠레저", "식품", "생활건강")

for(i in 1:length(category)){
  test=read_xlsx(paste0("C:\\navershopping\\positive_",category[i],".xlsx"))
  test=as.character(test)
  
  test_cps=Corpus(VectorSource(test))
  test_tdm=TermDocumentMatrix(test_cps,control = list(tokenize=ko.words,
                                            removePunctuation=T,
                                            removeNumbers=T,
                                            wordLengths=c(2,Inf)))
  Encoding(test_tdm$dimnames$Terms) = 'EUC-KR'
  tdm.unlist=unlist(test_tdm$dimnames$Terms)
  
  #감성분석 결과 점수
  pos_score[i]=sum(!is.na(match(tdm.unlist,posdic)))
  neg_score[i]=sum(!is.na(match(tdm.unlist,negdic)))
  score[i] = pos_score[i] - neg_score[i]
}
pos_score.fin=score
neg_score.fin=score

score.fin=c(pos_score.fin,neg_score.fin)
scale(score.fin)

sentence="가볍고 편하고 저렴해서 너무 좋아요. ^^올 겨울 따뜻하게 잘 신을수 있을거 같아요^^*??"
####################분석할 문장 입력#########################
{sentence <-readline('insert sentence : ')
print(sentence)

options(mc.cores=1)
cps=Corpus(VectorSource(sentence))
tdm=TermDocumentMatrix(cps,control = list(tokenize=ko.words,
                                          removePunctuation=T,
                                          removeNumbers=T,
                                          wordLengths=c(2,Inf)))
Encoding(tdm$dimnames$Terms) = 'EUC-KR'
tdm.unlist=unlist(tdm$dimnames$Terms)

#감성분석 결과 점수
pos_score=sum(!is.na(match(tdm.unlist,posdic)))
neg_score=sum(!is.na(match(tdm.unlist,negdic)))

if(pos_score>neg_score){
  percent_num=round(pos_score/(pos_score + neg_score)*100,2)
  percent=paste0(percent_num,"%확률로 긍정입니다.")
  
  #점수를 별점화
  {if(percent_num>60)
    star=5
  else if(percent_num<=60&&percent_num>20)
    star=4
  else
    star=3}
  
  }else{
  percent_num=round(neg_score/(pos_score + neg_score)*100,2)
  percent=paste0(percent_num,"%확률로 부정입니다.")
  
  #점수를 별점화
  {if(percent_num<=20)
    star=3
    else if(percent_num<=60&&percent_num>20)
      star=2
    else
      star=1}
  
}


result=data.frame(pos_score,neg_score,percent,star,sentence)
View(t(result))
}
