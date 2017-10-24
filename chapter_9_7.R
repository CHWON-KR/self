---
title: "chapter9_7"
author: "원치환"
date: "2017.10.23"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### 0. 성별 직업빈도 알아보기

1. library 설정 
2. 전처리 : 성별, 직업
3. 성별 직업 빈도표, 그래프 작성

### 1. library 설정

1. foreign, readxl 패키지 설정

```{r, echo=TRUE, eval=FALSE}
#패키지 설치 및 설정
install.packages("foreign")
install.packages("readxl")

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
```

2. 작업디렉토리 확인 및 설정
```{r, echo=TRUE, eval=FALSE}
#working directory 확인
getwd()
setwd("C:/Users/user/Desktop/r_sas/self/self") #작업경로
```

## 2. 데이터 전처리
1.데이터 불러오기 및 구조파악 
```{r, echo=TRUE, eval=FALSE}
#데이터불러오기
raw_welfare <- read.spss(file="Koweps_hpc10_2015_beta1.sav",to.data.frame=T)

#복사본 만들기
welfare <- raw_welfare

#데이터 구조파악
head(welfare)
tail(welfare)
view(welfare)
dim(welfare)
str(welfare)
summary(welfare)
``` 

2. 변수명 바꾸기 및 성별 코드 부여
```{r, echo=TRUE, eval=FALSE}
#번수명 바꾸기
welfare <- rename(welfare
                 ,sex = h10_g3 #성별
                 ,birth = h10_g4 #출생년
                 ,marrige = h10_g10 #혼인여부
                 ,religion = h10_g11 #종교
                 ,income = p1002_8aq1 #월급
                 ,code_job = h10_eco9 #직업코드
                 ,code_region = h10_reg7) #지역코드

#성별 이상치 확인하기
table(welfare$sex) #1이면 남자, 2이면 여자, 그외 오류
table(welfare$sex) #1이면 남자, 2이면 여자, 그외 오류
table(welfare$birth) #1907~2014까지
table(welfare$marrige) 
#0.비해당(18세 미만) 1.유배우 2.사별 3.이혼 4.별거 5.미혼(18세이상, 미혼모 포함) 6.기타(사망 등)
table(welfare$religion) #1.있음 2.없음
min(welfare %>% select(income) %>% filter(!is.na(income))) 
#연속형 income이 NA가 아닌것들 중에서 최소값 = 0
max(welfare %>% select(income) %>% filter(!is.na(income))) 
#연속형 income이 NA가 아닌것들 중에서 최대값 =2400
table(welfare$code_job) #CODE북 참조
table(welfare$code_region) 
# 1. 서울  2. 수도권(인천/경기) 3. 부산/경남/울산 4.대구/경북 5. 대전/충남 6. 강원/충북 7.광주/전남/전북/제주도

#성별 코드에 이름부여
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
```

3. 직업코드 붙이기
```{r, echo=TRUE, eval=FALSE}
#직업코드 붙이기
list_job <- read_excel("Koweps_Codebook.xlsx", col_names=T, sheet=2)

head(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, id="code_job") #code_job 기준으로 left join

welfare1 <- welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10 #welfare1 테이블로 확인 
```

3.1.결과

![그림1](pic_chap9_7_1.PNG)

### 3.성별 직업 빈도표, 그래프 작성
1. 성별 직업 상위 빈도 10개 추출

```{r, echo=TRUE, eval=FALSE}
#남성 직업 상위 빈도 10개 추출
job_male <- 
  welfare %>%
   filter(!is.na(job) & sex == "male") %>% 
   group_by(job) %>% 
   summarise(n=n()) %>% 
   arrange(desc(n)) %>% 
   head(10)

#여성 직업 상위 빈도 10개 추출
job_female <- 
  welfare %>%
  filter(!is.na(job) & sex == "female") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
```

1.1.결과(남성)

![그림2](pic_chap9_7_2.PNG)

1.2.결과(여성)

![그림3](pic_chap9_7_3.PNG)

2. 그래프 그리기
```{r, echo=TRUE, eval=FALSE}
#남성 직업 상위 빈도 그래프
ggplot(data = job_male, aes(x=reorder(job, n), y=n)) +
  geom_col() +
  coord_flip()

#여성 직업 상위 빈도 그래프
ggplot(data = job_female, aes(x=reorder(job, n), y=n)) +
  geom_col() +
  coord_flip()
```

2.1.결과(남성)

![그림4](pic_chap9_7_4.PNG)

2.2.결과(여성)

![그림5](pic_chap9_7_5.PNG)

### - EOD -
