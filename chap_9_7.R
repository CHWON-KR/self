
#패키지 설치 및 설정
install.packages("foreign")
install.packages("readxl")

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

#working directory 확인
getwd()
setwd("C:/Users/user/Desktop/r_sas/self/self")

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

#설별 코드에 이름부여
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)

#직업코드 붙이기
list_job <- read_excel("Koweps_Codebook.xlsx", col_names=T, sheet=2)

head(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, id="code_job") #code_job 기준으로 left join

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

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

#남성 직업 상위 빈도 그래프
ggplot(data = job_male, aes(x=reorder(job, n), y=n)) +
  geom_col() +
  coord_flip()

#여성 직업 상위 빈도 그래프
ggplot(data = job_female, aes(x=reorder(job, n), y=n)) +
  geom_col() +
  coord_flip()

        