library(readxl)
exam <- read_excel("excel_exam.xlsx")
mean(exam$math)
exam


seq(1, 100, 3)
sd(seq(1, 100, 3))


getwd()


#행(row) : obs , 로우, 레코드 recore
#열 : 변수, 속성,attributes 





# 데이터를 빠르게 쉽게 다룰 수 있는 기능을제공하고
# 데이터 페리임을 대상으로 하는 다양한 연산을 지원
dplyr
install.packages("dplyr")
library(dplyr)
exam <- read.csv("exam.csv")
exam
#행
filter(exam, class == 1)
filter(exam, class == 2)
filter(exam, class != 1) #not equal
#영어가 90이상인 학생은 몇 명?
filter(exam, english >= 90)

#영어가 90 이상이고 1반 학생은?
filter(exam, english >= 90 & class = 1)


exam %>% filter(class == 1)
exam %>%  
  filter(english >90) %>% 
  filter(class ==1)

#수학보다 90 보다 작은 학생들 중에서
#영어는 70보다 크고,
#1반이 아닌 학생들?
  
  
exam %>%  
  filter(math < 90 ) %>% 
  filter (english > 70)  %>% 
  filter(class != 1)

# 1반, 2반 학생들?

exam %>% filter(class ==1 | class == 2)
exam %>% filter(class ==1 | class == 2)
exam %>% filter(class %in%  c(1,2,3))



exam %>% 
  select(id, math) %>% 
  head(10)

# 1반 학생의 수학평균
exam %>%  filter(class == 1)
exam_1 <- exam %>%  filter(class == 1)
mean(exam_1$math)

mean((exam %>% filter(class == 1))$math) 

summary(exam %>%  filter(class == 1))


# 열 뽑기 
exam
head(exam , 3)
exam %>%  head(2)

select(exam, class, math)
exam %>%  select(class, math)
exam %>%  select(-math)
exam %>%  select(-(math), -(english))


exam %>%  filter(class ==1 ) %>% 
  select(math, english)


exam %>%  select(math, english, class) %>% 
  filter(class ==1)


# 정렬 arrange()

exam %>%  arrange(math)
exam %>%  arrange(desc(math))

exam %>%  arrange(class, desc(math))



# 파생변수 추가하기 (새로운 칼럼 추가)

exam %>% mutate(total = math + english + science)

# 총점과 평균 칼럼을 추가해 주세요

exam %>% 
  mutate(total = math + english + science, 
         avg = total/3) %>% 
  select(total, avg)


# ifelse(조건, 참일때, 거짓일때)

exam %>% 
  mutate(test = ifelse(science > 60, "pass", "fail"))

#수학, 영어, 과학의 평균을 구하고,
# 평균이 60 이상이면 "pass", 이하 "fail"
# 내림차순으로 정렬을 하고,

exam %>% 
  mutate(total = math + english + science, avg = total/3) %>% 
  mutate(test = ifelse(avg > 60, "pass", "fail")) %>% 
  arrange(desc(avg))






# filter 행
# select 열
# arrange 정리
# mutate 새로운 열 만들기 

--------------------------------------
  
# summarise
# group_by
  
  

library(dplyr)
library(readxl)
exam <-  read_excel("excel_exam.xlsx")
head(exam)  
mean(exam$math)  

exam %>% select(math) %>% mean() #안됌 mean은 baseR
exam %>% select(math) %>% summarise(avg = mean(math))


class1 <- exam %>%  filter(class==1)
mean(class1$math)

class2 <- exam %>%  filter(class==2)
mean(class2$math)

class3 <- exam %>%  filter(class==3)
mean(class3$math)

class4 <- exam %>%  filter(class==4)
mean(class4$math)


exam %>%  group_by(class) %>% 
  summarise(최고점 = max(math))

#요약함수

sum, mean, sd, median,min, max, n()


exam %>%  group_by(class) %>% 
  summarise(n())


# 영어의 반별 평균, 최고점, 최하점, 중앙값, 빈도 수

exam %>%group_by(class) %>% 
  summarise(영어평균 = mean(english),
            max = max(english),
            min = min(english),
            median = median(english),
            빈도 = n())




diamonds %>% head
mpg

table( c("A", "A", "B"))
table(diamonds$cut)
unique(diamonds$cut)
# cut 별로 가격의 최댓값과 최솟값은?

diamonds %>%
  group_by(cut) %>%
  summarize(max_price = max(price), 
            min_price = min(price))


str(diamonds)
diamonds


diamonds %>%
  group_by(carat) %>% 
  summarize(max_price = max(price), 
            min_price = min(price))


colnames(mpg)
rownames(mpg)
names(mpg)

mpg$class

# mpg 데이터의 class 

mpg %>% group_by(class) %>% 
  summarise(avg = mean(cty))

#내림차순으로 정렬

mpg %>% group_by(class) %>% 
  summarise(avg = mean(cty)) %>% 
  arrange(desc(avg))

# hwy 가 가장 높은 6개 회사

mpg %>% group_by(manufacturer) %>% 
  summarise(avg = mean(hwy)) %>% 
  arrange(desc(avg)) %>% 
  head(6)

# 어떤 회사 compact 차종을 가장많이 생산하는지

mpg %>% filter(class == "compact") %>% 
  group_by(manufacturer) %>%
  summarise(생산차 = n()) %>% 
  arrange(desc(생산차))
  
   

p152
test1 <- data.frame(id = c(1, 2, 3, 4, 5, 6),
                     midterm = c(60, 80, 70, 90, 85, 100))

test2 <- data.frame(id = c(1, 2, 3, 4, 5, 7),
                    final = c(70, 83, 65, 95, 80, 0))
test1
test2

left_join(test1, test2, by="id")

left_join(test1, test2)
right_join(test1, test2)
full_join(test1, test2)

bind_cols(test1, test2) %>% select(-id...3)
bind_rows(test1, test2)

exam

name <-  data.frame(class = c(1,2,3,4,5),
                    teacher = c("lee", "kim", "park", "choi", "jung"))
left_join(exam, name, by='class')

#칼럼 이름 바꾸기
dplyr : filter, select, rename, arrange, mutate, group_by, summarise
        left_join, bind_row

exam %>% rename( 반 = class )


#mpg에서 cty > city  , hwy>highway로 칼럼 바꾸기

mpg %>% rename(city = cty ,highway = hwy) %>% 
        select(city, highway)


mpg %>%  names()
colnames(mpg) <-  c(  "manufacturer", "model"   ,     "displ",       
                      "year"   ,      "cyl"    ,      "trans"  ,     
                     "drv"    ,      "city"    ,      "highway"    ,     
                      "fl"     ,      "class" )

colnames(mpg)
mpg <- mpg %>% select(manufacturer, model) %>%  head(10)
mpg

# manufacturer 를 mf 로 바꾸기

colnames(mpg) <-  c("mf", "model", "ity")
mpg$mf <- mpg$model
mpg

exam <- exam %>% filter(id < 11)
exam <- exam %>% select(id, class, math)
exam$classes <- exam$math #없는 칼럼은 새로 생겨서 들어간다
exam

exam <- exam %>% rename(english = classes) 
exam

#sum 칼럼을 만들어 주세요.

exam$SUM <- exam$math + exam$english



rm(exam)










