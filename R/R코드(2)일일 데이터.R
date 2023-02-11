setwd('covid-19')
library(dplyr)
temp = list.files(pattern = "*2021.csv") #전체 데이터 파일 불러오기
sample <- read.csv('2012-31-2020.csv') #2020년 12월31일 데이터 값 (시작값)
be_minus <- sample %>% group_by(Country_Region) %>% summarise(sum_confirmed=sum(Confirmed),sum_Deaths=sum(Deaths)) #저장공간 나라별로 합치기 및 총 확진자 수와 총 사망자 수 데이터 정리
samplee2 <- be_minus[,-c(2,3)] #일일 확진자 수 저장공간
samplee3 <- be_minus[,-c(2,3)] #일일 사망자 수 저장공간
for(i in 1:365){
  sample <- read.csv(temp[i]) # 일별CSV파일 불러오기
  sample2 <- sample %>% group_by(Country_Region) %>% summarise(sum_confirmed=sum(Confirmed),sum_Deaths=sum(Deaths)) #나라별로 합치기 및 총 확진자 수와 총 사망자 수 데이터 정리
  hab <-merge(sample2,be_minus,by='Country_Region',all=TRUE) #머지로 합쳐 계산할 수 있는 상태로 만들기
  hab$daily <- hab$sum_confirmed.x - hab$sum_confirmed.y #일일 확진자 데이터 추출
  hab$daily2 <- hab$sum_Deaths.x - hab$sum_Deaths.y #일일 사망자 데이터 추출
  samplee2 <- merge(samplee2,hab[c(1,6)],by='Country_Region',all=TRUE) 
  # samplee2에 일일 확진자 수 저장
  samplee3 <- merge(samplee3,hab[c(1,7)],by='Country_Region',all=TRUE) 
  # samplee3에 일일 사망자 수 저장
  be_minus <- sample2 # be_mines에 다음날 데이터 저장
}
samplee4 <- be_minus[,-c(2,3)] #최종값 저장공간
a <- rowSums(samplee2[,-1]) %>% as.data.frame() #일일 데이터 누적값 데이터 프레임으로 변환
b <- rowSums(samplee3[,-1]) %>% as.data.frame()
samplee4$tot_con2 <- a[,1]  # 일일 확진자와 일일 사망자 데이터들을 하나의 데이터프레임으로 결합
samplee4$tot_dea2 <- b[,1]
samplee4 # 1-2번 문제 최종 값 

samplee4$compare_con <- samplee4$tot_con2 - acc_final$tot_con #1-1번과 1-2번 데이터 결과값 비교 및 확인
samplee4$compare_dea <- samplee4$tot_dea2 - acc_final$tot_dea
samplee4[c(1,4,5)]