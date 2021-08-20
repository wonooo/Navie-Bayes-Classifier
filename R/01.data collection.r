# 네이버의 영화리뷰에서 리뷰 수집
library(rvest)
library(stringr)
library(tm)
library(KoNLP)
library(dplyr)
library(gmodels)
library(e1071)

tenet_moive <- NULL
for(i in 1:500){
  url <- paste0('https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=190010&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=', i)
  html <- read_html(url)
  
  point <- html_nodes(html, '.star_score') %>% html_node('em') %>% html_text()
  
  comment <- html_nodes(html, '.score_reple>p') %>% html_text()
  comment <- gsub('관람객', '', comment)
  comment <- gsub('\n', '', comment)
  comment <- gsub('\t', '', comment)
  comment <- gsub('\r', '', comment)
  
  temp_df <- data.frame(point=point, comment=comment)
  tenet_moive <- rbind(tenet_moive, temp_df)
  Sys.sleep(2)
}
nrow(tenet_moive)
View(tenet_moive)

tenet_moive[tenet_moive$comment==" ",]     # 321개 조회됨 -> 삭제
nrow(tenet_moive[tenet_moive$comment==" ",])
tenet_moive <- tenet_moive[-which(tenet_moive$comment==" "),]
nrow(tenet_moive) # 4679개
rownames(tenet_moive) <- NULL
