
############복 습 #################

#      filter 와 select 차이
#       행        열

#diamonds에서 clarity 추출
#   diamonds$clarity    or   diamonds %>% clarity

#plot (diamonds$cut)
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

diamonds
plot(diamonds$cut)

# diamonds의 칼럼


# 칼럼 합치기
exam <- data.frame(class= c(1,2,3,4,5),
                   subject = c("math", "english", "science","society","korean"))
name <-data.frame(class= c(1,2,3,4,5),
                  teacher = c("kim","lee","park","choi","jung")) 

left_join(exam, name)
left_join(exam, name, by = "class")
full_join(exam, name)



# 1. mutate() 
#             새로운 열 만들기

# summarise() 항상 어떤 함수랑 같이 쓰이는가  
#             group_by


# mpg에서 회사별 cty 평균 구하기

mpg %>% group_by(manufacturer) %>% 
        summarise(avg = mean(cty))

# mpg 에서 회사별  suv  차종 수를 내림차순으로

mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise( suv_num  = n()) %>% 
  arrange(desc(suv_num))



# 

mpg_new <-mpg

mpg_new %>% 
  rename(displacement = displ , fuel = fl)


install.packages("readxl")
library(readxl)
getwd()
exam <- read.csv("exam.csv")

#1 math 성적을 기준으로 
80 이상이면 a, 
60이상이면b, 
40 이상이면 c,
그 미만은 f



exam %>% 
  mutate(math2 = ifelse(math>=80,"a", 
                ifelse(math>=60,"b", 
                ifelse(math>=40,"c","f"))))
 


df <- data.frame(gender = c("M", "F", NA, "M", "F"),
                 score = c(5,4,3,4,NA))
df

# 1. 겹측지가 있는지
is.na(df)
table(is.na(df))
sum(is.na(df))

# 모두몇개    44
# Ozone 칼럼에는 몇개? 37

airquality
sum(is.na(airquality))
sum(is.na(airquality$Ozone))
summary(airquality)    #NA 몇개 있는지 칼럼별로

# 2. 결측지 처리하기
# 삭제하기
na.omit(df)
#채워넣기
df$gender <- ifelse(is.na(df$gender), "M", df$gender)

df$score <- ifelse(is.na(df$score), "5", df$score)


df [3,1] <- "F"
df [3,1] <- NA
df [5,2] <- NA

mean(df$score)
sum(df$sum)
mean(df$score, na.rm = T)

airquality %>%  head
mean(airquality$Ozone, na.rm = T)

is.na(df$score)
df %>%  filter(is.na(df$score))
df %>%  filter(!is.na(df$score)) %>% 
        summarise(mean(score))

airquality %>% head

# solar.r 을 solar 로 바꾸기
# ozone과 solar 만 행을 뽑아서,
# 두 칼럼 각각의 평균을 구해주세요



#됌 ( 하나씩 차근차근해도됌)

airquality<-airquality%>%
  rename(Solar=Solar.R)
colnames(airquality)  

airquality<-airquality%>%select(Ozone,Solar)
airquality
mean(airquality$Ozone,na.rm=T)
mean(airquality$Solar,na.rm=T) 


airquality <- airquality %>% rename(Solar = Solar.R)
mean(airquality$Ozone,na.rm = T)
mean(airquality$Solar,na.rm = T)






# 이상치 outlier

outlier <- data.frame(gender = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,10))
outlier

# 1. 이상치가 있는지?

boxplot(outlier$score)
ggplot(data = diamonds, aes(x=cut, y=price)) + geom_boxplot()

boxplot(outlier$score)

# 2. 이상치 처리하기

#6번은 제외하고 평균 score을 구해주세요

outlier %>% filter(score < 10) %>% 
            summarise(mean(score))

mean(outlier$score[-6])
mean(outlier[-6, "score"])

#10을 결측지로 바꾸어주세요.
outlier
outlier[6, 2] <- NA

mean(outlier$score, na.rm=T)

  
# vector    : 한줄, 1차원
# dataframe : 표, 2차원









######################################
######################################
######################################
######################################

install.packages("ggplot2")
library(ggplot2)
library(dplyr)

ggplot



mpg %>% select(displ, hwy) %>% 
    data.frame()

mpg
plot(mpg$displ, mpg$hwy)

ggplot(data = mpg, mapping = aes(x= displ, y=hwy)) +
  geom_point(color = "blue", size=1) +
  geom_line() +
  xlim(3, 6) + ylim(10, 30)

  
ggplot(mpg, aes(x = disp  , y=hwy)) + geom_point()

aes : aesthetic
geom: geometry

str(mpg)
glimpse(mpg)


ggplot(mpg, aes(x= cty , y=hwy)) + geom_point(color = "red")
ggplot(mpg, aes(x= displ , y=hwy)) + geom_point(color = "red")
ggplot(mpg, aes(x= drv , y=hwy)) + geom_point(color = "red")
mpg$drv
unique(mpg$drv)



ggplot(mpg, aes(x= displ , y=hwy)) + geom_point()
ggplot(mpg, aes(x= displ , y=hwy, color=drv )) + geom_point()
ggplot(mpg, aes(x= displ , y=hwy, color=cty )) + geom_point(size =3)
ggplot(mpg, aes(x= displ , y=hwy, color=class)) + geom_point(size =3)


# 막대그래프 : geom_bar()

ggplot(data = mpg, aes(x= class, fill=class )) + geom_bar()
ggplot(data = mpg, aes(x= drv, fill=drv )) + geom_bar()
ggplot(data = mpg, aes(x= drv, fill=class )) + geom_bar()
ggplot(data = mpg, aes(x= class, fill=drv )) + geom_bar()


ggplot(mpg, aes(x= class,)) + geom_bar(aes (fill=class) )




df <- table(mpg$class) %>%  as.data.frame()
df <- df %>%  rename(class = Var1)

ggplot(df, aes(x= class, y=Freq)) + geom_bar(stat="identity")
ggplot(df, aes(x= class, y=Freq)) + geom_col()
ggplot(df, aes(x= class, y=Freq, fill = class)) + geom_col()


df %>% 
  arrange(desc(freq)) %>% 
  ggplot(aes(x = reorder(class, -Freq), y = freq, fill = class)) + 
  geom_col()
         
#drv : 구동방식별로 고속도로 평균연비를 구해서
#      그래프로 그려라.
         
df1 <- mpg %>% group_by(drv) %>%
              summarise(평균연비 = mean(hwy))
ggplot(df1, aes(x= drv, y= 평균연비, fill=drv)) + geom_col()        
ggplot(df1, aes(x= reorder(drv, -평균연비), y=평균연비, fill=drv)) + geom_col()        


economics

ggplot(economics, aes(x=date, y=pop)) + geom_line()
ggplot(economics, aes(x=date, y=unemploy)) + geom_line()
ggplot(economics, aes(x=date, y=psavert)) + geom_line()

economics$date <- as.Date(economics$date)
ggplot(economics, aes(x=date, y=psavert)) + geom_line()

mpg
ggplot(data=mpg, aes(x=drv, y=hwy, fill= drv)) + geom_boxplot()
ggplot(data=mpg, ) + geom_boxplot(aes(x=drv, y=hwy, fill= drv))



boxplot(diamonds)










         
         
         
         
         
         
       

##################################################











############### 문제 만들고 연습하기 #############
  

# 다음과 같은 데이터프레임이 있을 때, gender가 1인 행만 추출하고, score 열의 평균을 구하세요.



problem1 <- data.frame(gender = c(1, 2, 1, 3, 2, 1),
                 score = c(5, 4, 3, 4, 2, 10))
problem1

problem1[1, ]
mean(problem1$score)


# 제조사별로 생산한 suv 차량의 수를 내림차순으로 정렬하여 출력하는 코드를 작성하세요.

mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(suv_num = n()) %>% 
  arrange(desc(suv_num))

# airquality 데이터프레임 전체에서 결측치가 몇 개인지 계산하세요.
# Ozone 열에서 결측치가 몇 개인지 계산하세요.

airquality()
sum(is.na(airquality))
sum(is.na(airquality$Ozone))



# 대표적인 도시별 연비 평균이 높은 차량 제조사는 어디인가?


mpg %>% 
  group_by(manufacturer, class) %>% 
  summarise(mean_cty = mean(cty)) %>%
  group_by(class) %>% 
  arrange(desc(mean_cty)) %>% 
  group_by(manufacturer) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
  

# 2. SUV 차종 중 가장 연비가 좋은 차량은 어떤 차종인가?

mpg %>%
  filter(class == "suv") %>% 
  group_by(manufacturer, model) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  slice(1)


# class가 1인 학생들의 math 점수의 평균을 구하세요.

exam <- data.frame(
  id = 1:20,
  class = rep(1:5, each = 4),
  math = c(50, 60, 45, 30, 25, 50, 80, 90, 20, 50, 65, 45, 46, 48, 75, 58, 65, 80, 89, 78),
  english = c(98, 97, 86, 98, 80, 89, 90, 78, 98, 98, 65, 85, 98, 87, 56, 98, 68, 78, 68, 83),
  science = c(50, 60, 78, 58, 65, 98, 45, 25, 15, 45, 65, 32, 65, 12, 78, 65, 98, 90, 87, 58)
)


exam
exam %>% 
  filter(class == 1) %>%
  summarise(mathavg = mean(exam$math))



#데이터 프레임 df가 주어졌을 때, 다음 문제를 해결해보세요.

#df에서 "A" 칼럼의 평균과 중앙값을 구하세요.
#df에서 "B" 칼럼에서 값이 3 이상인 데이터만 추출한 후, "C" 칼럼의 평균을 구하세요.
#df에서 "D" 칼럼에서 최댓값과 최솟값의 차이를 구하세요.


df <- data.frame(A = c(1, 2, 3, 4, 5),
                 B = c(2, 3, 4, 1, 5),
                 C = c(3, 2, 5, 4, 1),
                 D = c(10, 5, 7, 3, 9),
                 E = c(4, 8, 3, 2, 6),
                 F = c(2, 4, 3, 5, 1))

df
#1
summary(df)

#2
df %>% 
  select(B) %>% 
  filter(B >=3)
df %>%
  select(C) %>% 
  summarise(C_avg = mean(C))
#3

cnum <- df$C
cnum
max(cnum)-min(cnum)



#데이터프레임 df가 주어졌을 때, 결측치가 있는지 확인하고 
#결측치를 평균값으로 대체하는 코드를 작성해보세요.


df <- data.frame(x = c(1, 2, 3, NA, 5), 
                 y = c(6, 7, NA, 9, 10))

sum(is.na(df))


df[is.na(df)] <- mean(df, na.rm = TRUE)



######################################
####################################
###################################
####################################3


# mtcars 데이터셋에서 wt 변수와 mpg 변수 사이의 산점도를 그리세요.
mtcars

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

# diamonds 데이터셋에서 cut 변수의 빈도를 막대그래프로 나타내세요.
diamonds
ggplot(diamonds, aes(x=cut)) + geom_bar()


#economics 데이터셋에서 연도별 개인 저축률(psavert)의 변화를 선그래프로 나타내세요.

ggplot(data=economics, aes(x = date, y=psavert)) + geom_line()



# mpg 데이터셋에서, 자동차 종류(class)와 배기량(displ)간의 관계를 나타내는 상자그래프를 그려보세요.
# 단, 상자그래프는 자동차 종류(class)별로 각각 그려주세요.

mpg
ggplot(mpg, aes(x=class, y=displ, fill=class)) + geom_boxplot()

# gpt가 답안 제시
ggplot(mpg, aes(x=class, y=displ, fill=class)) + 
  geom_boxplot() +
  labs(title="자동차 종류별 배기량 분포", x="자동차 종류", y="배기량") +
  theme_bw()


# mpg 데이터셋에서, 자동차 종류(class)별로 고속도로 연비(hwy)의 분포를 
# 나타내는 상자그래프를 나타내고, 평균 연비를 빨간 점으로 표시하라.


mpg
class_hwy <- mpg %>% group_by(class) %>% 
  summarise(class_hwy = mean(hwy))

ggplot(mpg, aes(x=class, y=hwy, fill = class)) + geom_boxplot() +
  geom_point(data=class_hwy, aes(x=class, y=class_hwy), 
             color="red")



#p178


mpg <- ggplot2::mpg
mpg[c(10, 14, 58, 93), "drv"] <-  "k"
mpg[c(29, 43, 129,203), "cty"] <-  c(3, 4, 39, 42)


table(mpg$drv)
mpg$drv <- ifelse(mvpg$dr %in% c("4", "f", "r"), mpg$drv, NA )

mpg$drv <- ifelse(mpg$drv %in% c("k"), NA, mpg$drv)


boxplot(mpg$cty)$stats
mpg$cty <-  ifelse(mpg$cty < 9 | mpg$cty > 26, NA , mpg$cty)


boxplot(mpg$cty)
mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))
  
  
  
#p188

ggplot(mpg, aes(x = cty, y = hwy)) + geom_point()  

midwest
ggplot(midwest, aes(x=poptotal, y= popasian)) + geom_point() + xlim(0, 500000) + ylim(0 , 10000)
  
  
  
  









