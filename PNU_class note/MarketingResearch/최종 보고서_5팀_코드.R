### 필요한 라이브러리 ###
library(psych)


### 데이터 불러오기 ###
data <- read.csv('R/response.csv')
# 20대의 설문 결과만 사용
data <- data[data$age < 30, ]
attach(data)


### 사전 변수 정의 - 평균값 계산 ###
# value : 경험가치
val <- (value1 + value2 + value3)/3
# process_order + process_info : 과정품질
proq <- (process_order1 + process_order2 + process_order3 + process_order4 +
           process_inf1 + process_inf2 + process_inf3 + process_inf4)/8
# outcome : 결과품질
outcomes <- (outcome1 + outcome2 + outcome3 + outcome4 + outcome5)/5
# environment : 서비스 환경 품질
envq <- (environment1 + environment2 + environment3 + environment4 
         + environment5 + environment6 + environment7)/7
# satisfaction : 만족도
satis <- (satisfaction1 + satisfaction2 + satisfaction3 + satisfaction4)/4
# continuous : 지속 사용 의도
contin <- (continuous1 + continuous2 + continuous3)/3


### 신뢰도 분석 ###
# 경험 가치
alpha(data[, 24:26])
# 과정 품질
alpha(data[, 4:11])
# 결과 품질
alpha(data[, 12:16])
# 서비스 환경 품질
alpha(data[, 17:23])
# 만족도
alpha(data[, 27:30])
# 지속 사용 의도
alpha(data[, 31:33])


### 가설 검증 ###
# H1 - 1) 키오스크의 과정 품질은 경험 가치에 정의 영향을 미칠 것이다.
# 단순 선형 회귀
verf1 <- lm(val ~ proq)
summary(verf1)


## H1 - 2) 키오스크의 결과 품질은 경험 가치에 정의 영향을 미칠 것이다.
# 단순 선형 회귀
verf2 <- lm(val ~ outcomes)
summary(verf2)


## H1 - 3) 키오스크의 서비스 환경품질은 경험가치에 정의 영향을 미칠 것이다.
# 단순 선형 회귀
verf3 <- lm(val ~ envq)
summary(verf3)


# H1) 키오스크의 품질 특성은 경험가치에 유의미한 영향을 미칠 것이다.
verf <- lm(val ~ proq + outcomes + envq)
summary(verf)


## H2) 음식점 키오스크 이용에 대한 경험가치는 만족에 유의한 정의 영향을 미칠 것이다.
# 단순 선형 회귀
verf4 <- lm(satis ~ val)
summary(verf4)


## H3) 음식점 키오스크에 대한 만족은 지속 사용에 유의한 정의 영향을 미칠 것이다.
# 단순 선형 회귀
verf5 <- lm(contin ~ satis)
summary(verf5)



## H4) 성별에 따른 만족도 차이는 유의할 것이다.
# 성별과 만족도가 존재하는 데이터 프레임 생성
df <- data.frame(gender, satis)
# 여성 집단(f_df)과 남성 집단(m_df) 구분
f_df <- df[df$gender=='female', 2]
m_df <- df[df$gender=='male', 2]

# t-test 실시
t.test(f_df, m_df)