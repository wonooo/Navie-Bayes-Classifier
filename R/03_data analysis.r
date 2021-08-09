# 테스트와 샘플데이터 나누기
set.seed(1234)
idx <- sample(2, nrow(tenet_dtm), replace = T, prob = c(0.8,0.2))
table(idx)

tenet_dtm_train <- tenet_dtm[idx==1,]
tenet_dtm_test <- tenet_dtm[idx==2,]
nrow(tenet_dtm_train)

# 데이터에 대한 정답이 들어있음
tenet_dtm_train_labels <- factor(tenet_moive[idx==1,2])
tenet_dtm_test_labels <- factor(tenet_moive[idx==2,2])
length(tenet_dtm_train_labels)               # nrow(sms_dtm_train)과 동일(3731개)

# 필요한 단어들에 대한 사전 만들기
tenet_freq_words <- findFreqTerms(tenet_dtm_train, lowfreq = 3)
length(tenet_freq_words)
View(tenet_freq_words)

# train데이터에 필요단어 사전단어가 얼마나 들어있는지 확인: 508개
tenet_dtm_train[,tenet_freq_words]

tenet_dtm_freq_train <- tenet_dtm_train[,tenet_freq_words]
tenet_dtm_freq_test <- tenet_dtm_test[,tenet_freq_words]
nrow(tenet_dtm_freq_train)
nrow(tenet_dtm_freq_test)
tenet_dtm_freq_train$dimnames

# 1 이상의 값은 YES로 0은 NO로 바꿔줌
convert_counts <- function(x){
  x <- ifelse(x>0, 'YES', 'NO')
}


#############################################
# 나이브베이즈 이론을 적용
tenet_train <- apply(tenet_dtm_freq_train, MARGIN = 2, convert_counts)
str(tenet_train)
tenet_test <- apply(tenet_dtm_freq_test, MARGIN = 2, convert_counts)
str(tenet_test)

#나이브베이즈
tenet_classifier <- naiveBayes(tenet_train, tenet_dtm_train_labels)
str(tenet_classifier)

# test데이터가 예측으로 나온 결과
tenet_test_predict <- predict(tenet_classifier, tenet_test)
table(tenet_test_predict)
warnings()
# 실제 정답
tenet_dtm_test_labels
table(tenet_dtm_test_labels)

# 결과 확인
CrossTable(tenet_test_predict, tenet_dtm_test_labels)

# ---------------------결과
# 최소빈도 3개로 했을때 예측률
(584+71+17)/948
0.7088608

# 최소빈도 5개로 했을때 예측률
(599+65+17)/948
0.7183544

# 최소빈도 2개로 했을때 예측률
(572+67+17)/948
0.6919831

