
#데이터 프레임

english <-  c(90, 80, 60, 70)
math <-  c(50, 60, 100, 20)

rm(str1)
rm(str, str2)

rm(list = ls())


df_midterm<- data.frame(english, math)
class <- c(1, 1, 2, 2)
df_midterm <- data.frame(english, math, class)
df_midterm


#접근하기

df_midterm[2, 3]
df_midterm[3, 2]
df_midterm[3, ]
df_midterm[3, c(1,2,3)]
df_midterm[c(2,3), c(1,2)]
# 이런 방법도 있다
df_midterm[2:3, 1:2]


 # '-'는 뺸다
df_midterm[3, -1]
df_midterm[3, -2]
df_midterm[3, -3]

# 3개다 다른 방법으로 같은 결과
df_midterm[ , 1]
df_midterm[ , 'english']
df_midterm$english

#영어성적의 평균은?
mean(english)
mean(df_midterm$english)



제품 <- c("사과", "딸기", "수박")
가격 <- c(1800,1500, 3000)
판매량 <- c(24, 38, 13)

df=data.frame(제품 = c("사과", "딸기", "수박"), 
           가격 = c(1800,1500, 3000), 
           판매량 = c(24, 38, 13)
           )
df
mean(df$판매량)




excel
csv


# 함수 : 입력,,, 출력 ????
# 파일 위치, 함수이름

read_excel()
library(readxl)
install.packages("readxl")

# working directory
getwd()
setwd("c:/Rdata")

df <- read_excel("excel_exam.xlsx")

#write = 저장하기 (뭐할지, 이름)
write.csv(df, "exam.csv")

#영어평균
mean(df$english)

＃ｃｓｖ파일읽기



read.csv("csv_exam.txt")
exam <- read.csv("exam.csv")
exam[ , 2:6]
exam <- exam[ , -1]
exam


#csv 파일이란
comma seperated value


df

saveRDS(df, file = "df.rds")

readRDS("df.rds")

exam

head(exam)


view(exam)
dim(exam)

str(exam)  #structure

#obs : observations (관측지)
#variavles : 변수

exam 

20 X 5
행
관측지


int : integer(정수)
length(exam$id)

summary(exam)

mpg

library(ggplot2)
diamonds

# 몇행 몇열입니까?
53940 10
#carat 의 최댓값은?
31.8
#price 의 중앙값은?
3.530
#price 의 평균은?
3.539

dim(diamonds)
summary(diamonds)



































