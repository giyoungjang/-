


############# 복 습 하 기 #############





install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

exam <- read.csv("csv_exam.txt")
exam





#1. 영어가 70이하이고, 4반, 5반인 학생?

exam %>% 
  filter(english <= 70 & class %in% c(4, 5))

#2. 반별 class 의 영어, 수학, 과학 평균점수의 총점은?


exam %>% 
  group_by(class) %>% 
  summarise(mean(math+english+science))


# 영어가 90 점 이상이고 1반인 학생은?
   
 exam %>% filter(math < 90, english > 70, class != "1")


# 수학이 90점보다 작은 학생들 중에서 영어는 70보다 크로, 1반이 아닌 학생들은?
   
  exam %>% filter(math <= 90 ) 
  
  
# 총점과 평균 칼럼을 추가해 주세요

exam <- exam %>%
    mutate(total= english + math + science, 
           avg = (english + math + science)/3)

#통과여부 칼럼추가 ( 세과목의 평규이 60점 이상이면 "pass", 미안이면 "fail)

exam %>% 
  mutate(통과여부 = ifelse(avg >=60, "pass","fail"))
   


#mpg 에서 도심연비가 가장 좋은 차종 상위 6개는?

mpg %>% 
  group_by(model) %>% 
  summarise(mean_md = mean(cty) %>%
  arrange((mean_md))
  head(6)

  

# 차트 만들기  
mpg

ggplot(mpg, aes(x=class , fill = class)) + geom_bar()



#
mean_mpg<- mpg %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>% 
  head(5)
  
#  어떤 회사에서 생산한 평균 cty가 높은지를 구해
# 가장 높은 회사 다섯 곳을 막대 그래프로 표현하시오.

ggplot(mean_mpg, aes(x= reorder(manufacturer, -mean_cty), 
                     y= mean_cty)) + geom_col() +
  labs(x = "Manufacturer", y = "City MPG (Mean)")



exam <- read.csv("csv_exam.txt")
exam[ , c("class" , "english")]
exam[ , c(2,3)]
exam[c(2, 3), ]
exam[3:5, ]


exam[exam$english >90, ]
exam[exam$class ==1, ]

exam$class ==1
exam$english >90


#A,B,C,D,E,F,G,H
#학점 : A, B
#범주, 카테고리, factor :금, 은, 동


var1 <- c(1,2,3,4,5)
mode(var1)
var2 <- as.factor(var1)
mode(var2)


str(diamonds)

var3 <- c("a","b", "b", "c","c","c")
var3 <-  factor(var3)
class(var3)


as.numeric(var3)
as.factor(var3)
as.character(var3)
as.Date(var3)
as.data.frame(var3)

# matrix ( 숫자만 들어있음)
# data frame (숫자 글자 둘다)
#array : 배열 1차원, 2차원,  3차원 ....

#list : 파이썬에선 1차원 r에선 다차원

mpg
class(mpg)
class(mpg[1:3 , 1:3])
class(mpg[ , 3])
class(mpg[3 , ])

class(mpg[ , 1])
class(mpg$manufacturer)

mpg
ggplot(mpg, aes(x= manufacturer, y=class, color = drv)) + geom_point(size = 3)






######################################







######################################

ggplot(mpg, aes(x=displ, y= hwy, color=cyl)) + geom_point(size=2)


ggplot(mpg, aes(x=displ, y= hwy, color=class)) + geom_point(size=2)


ggplot(mpg, aes(x=displ, y= hwy, color=cty, shape = drv)) + geom_point(size=2)

ggplot(mpg, aes(x=displ, y= hwy)) + 
      geom_point(size=2) +
      geom_smooth(method = "lm")

ggplot(mpg, aes(x=displ, y= hwy, color=drv)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm")


g <- ggplot(mpg, aes(x=displ, y= hwy, color=drv, shape = drv)) + 
  geom_point(size=2) 
  
g1 <- ggplot(mpg, aes(x=displ, y= hwy, color=drv, shape = drv)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm")


#smooth 는 곡전이지만, method 를 lm으로 하면 직전으로 보여줌





#테마

g1 + theme_economist()
g1 + theme_wsj()

# 타이틀 달기

g1 + labs(title = "< 배기량에 따른 고속도로 연비 비교 >", x = "배기량", y = "연비")

#묶어서 보기

g1 + facet_wrap(~ drv)
g1 + facet_wrap(~ class)



ggplot(economics,aes(date, unemploy)) + geom_line()

ggplot(mpg, aes(cty)) +
  geom_histogram(bins =10) +
  geom_freqpoly(bins = 20, color = 'red') 
 #   facet_wrap(~ manufacturer)




ggplot(mpg, aes(cty)) + geom_histogram(bins=50)


ggplot(mpg, aes(x=displ)) +
  geom_histogram(binwidth = 0.5) +
  geom_freqpoly(binwidth = 0.5)

ggplot(mpg, aes(displ, fill = drv)) +
  geom_histogram(binwidth = 0.5, position = "dodge")


ggplot(mpg, aes(manufacturer) + geom_bar()


drugs <- data.frame(drug = c("a", "b", "c"),
                    effect = c(4,9,6))
drugs


ggplot(economics, aes(date, unemploy/pop)) + 
  geom_line()




############


실사용 데이터 분석


############



library(foreign)
raw_welfare <- read.spss("Koweps_hpc10_2015_beta1.sav")
welfare <- as.data.frame(raw_welfare)

dim(welfare)
str(welfare)
summary(welfare)
head()
glimpse(welfare)

length(names(welfare))
dim(welfare) [2]

welfare <-  welfare %>% 
  select(  gender = h10_g3, birth = h10_g4,
           marriage = h10_g10, religion = h10_g11,
           income = p1002_8aq1, job = h10_eco9,
           region = h10_reg7)

head(welfare)

class(welfare)
welfare <- as_tibble(welfare)
# air <-  as_tibble(airquality) # tibble 시키면 요약해서 위에 나옴

plot(welfare)
boxplot(welfare)
sum(is.na(welfare)) #결측지 찾기
summary(welfare$income)

mean(welfare$income)
mean(welfare$income, na.rm = T)
range(welfare$income)
range(welfare$income, na.rm = T)

welfare$income <- ifelse(welfare$income == 0, NA, welfare$income)
summary(welfare$income)
range(welfare$income, na.rm=T)

ggplot(data= welfare, aes(x=income, color = factor(gender))) + 
  geom_density()

<- as.factor(welfare$gender)


# 위에 color로 남자 여자 구분을 할때 num 이기 때문에
# 바꿔줘야 한다.

welfare$gender <- ifelse(welfare$gender == 1, "male", "female")

table(welfare$gender)
ggplot(data=welfare, aes(x=gender, fill = gender)) + geom_bar()

# 여자, 남자 평균 소득 (bar 로 했지만 작동하지 않음)
# geom_bar()는 y축이 count를 나타내는 경우에 사용하는 것이고, 
# y축이 평균값 등 다른 값일 때는 geom_col()을 사용하는 것이 좋다

mean_income <- welfare %>% 
  group_by(gender) %>%
  summarise(avg_income = mean(income, na.rm = T))
ggplot(mean_income, aes(x = gender, y= avg_income, fill = gender)) +
  geom_col()


max(welfare$birth)

welfare$age <-2015 - welfare$birth
welfare %>% 
  select(birth, age)

welfare %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income, na.rm=T)) %>% 
  ggplot( aes(x = age, y= mean_income)) + geom_line() + 
  xlim(20, 80)

ifelse(welfare$birth == 2014, 1, 2014 - welfare$birth + 1)



welfare





################# 문제 #################

#다이아몬드 가격(price)의 분포를 구하고 시각화 하세요. 이 때, 
#가격이 10,000 이상인 데이터는 제외하세요.

diamonds_filter <- diamonds %>% 
  filter(price < 10000)


ggplot(diamonds_filter, aes(x= price)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Diamond Price Distribution", x = "Price", y = "Frequency")


# 다이아몬드 가격(price)이 높은 순으로 10개의 데이터를 출력하세요.

diamonds %>% 
  arrange(desc(price)) %>% 
  head(10)

# 각 색상(color)별, 가격(price)의 평균과 중앙값을 구하기

diamonds_defg <- diamonds %>% 
  filter(color %in% c("D", "E", "F", "G"))

diamonds_defg %>% 
  group_by(color) %>% 
  summarise(mean_pirce = mean(price) , median_pirce = median(price))




# airquality 데이터셋을 이용하여 tibble 형태로 저장하여 월 평균 온도를 구합니다
# 각 월별로 평균 온도를 구한 후, 이 결과를 x 축은 월, y축은 평균온도로 하여,
# 평균 온도가 80도를 넘으면 빨간색, 그렇지 않으면 파란색으로 표시하세요.


airquality <- as_tibble(airquality)

avg_temp <- airquality %>%
            group_by(Month) %>% 
            summarise(avg_temp = mean(Temp))

ggplot(avg_temp , aes(x = Month, y = avg_temp)) + 
  geom_bar(aes(fill = ifelse(avg_temp > 80, "blue", "red" )), 
           stat = "identity")

###바로 위 아래는 같은 결과 값

ggplot(avg_temp , aes(x = Month, y = avg_temp, fill = ifelse(avg_temp > 80, "blue", "red" ))) + 
  geom_col()


↑↑↑↑↑↑↑↑↑↑↑오른쪽에 뜨는 blue ,red 를 해결 못했습니다...




# mtcars 데이터 셋을 이용하여, disp와 hp간의 상관관계 분석하기
# mtcars을 tibble 형태로 저장하여 disp와 hp의 산점도를 그립니다.
# x축은 disp, y축은 hp로 한다음, smooth의 직선 그래프를 추가하는 추세선을 그립니다.


mtcars_tbl <- as_tibble(mtcars)

ggplot(mtcars_tbl, aes(x = disp, y = hp)) + 
  geom_point() + 
  geom_smooth(method = "lm")

welfare <- welfare %>% 
  mutate(age = welfare$age)

welfare <-  welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                        ifelse(age <= 59, "middle", "old")))

table(welfare$ageg)
qplot(welfare$ageg)

ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income=mean(income))





