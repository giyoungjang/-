install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)

getwd()
read.csv("rank1.csv")

rank1 <- read.csv("rank1.csv")

# 맨앞 열 year 제거
rank1 <- rank1[ , -1]


# 20억이상 당첨 중에서 내림차준으로 정렬하고 상위 10개
rank1 %>% filter(money > 2000000000) %>% 
      arrange(desc(money)) %>% 
      head(10)



#최대 최소 평균 당첨금액 확인
summary(rank1)


#당첨자 수 10명 이상인 회차 중에서 내림차순 상위 10개
rank1 %>% filter(human > 10) %>% 
  arrange(desc(human)) %>% 
  head(10)


# 10명 이상 20억 이상


rank1 %>% filter(human > 10) %>%
  filter(money > 2000000000)
  arrange(desc(human)) %>% 
  head(10)

  
# 10명 이상중에서 1인당 당첨금 많이 한 순서
rank1 %>% mutate()
  
  
  
  








