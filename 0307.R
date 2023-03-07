##################복습하기


ggplot(mpg, aes(x=displ, y=hwy)) + geom_point(size=2) +
  geom_smooth() +
  labs(title = "배기량에 따른 도시연비 비교")

ggplot(economics, aes(x=date, y=unemploy))+geom_line()+theme_economist()

mpg %>% filter(manufacturer == "audi")
mpg[mpg$manufacturer == "audi", ]

mpg %>% filter( displ < 2)
mpg[mpg$displ < 2 , ]

library(foreign)
welfare <- read.spss("Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

# 남자, 여자 소득 평균

welfare <-  welfare %>% 
  select(  gender = h10_g3, birth = h10_g4,
           marriage = h10_g10, religion = h10_g11,
           income = p1002_8aq1, job = h10_eco9,
           region = h10_reg7)

# 위에를 이용해 그래프 그리기

welfare %>%
  group_by(gender) %>%
  summarise(income_gender = mean(income, na.rm=T)) %>% 
  ggplot(aes(gender, income_gender, fill = gender)) + geom_col()


welfare$gender <- ifelse(welfare$gender == 1 , 'male', 'female')


as_tibble()

mode(welfare$marriage)
welfare$marriage <- ifelse(welfare$marriage == 1 , '기혼', '미혼')

20대의 혼인상태를 막대그래프를 보이시오

welfare$age <- 2015 - welfare$birth

welfare %>% 
  filter(age >=20 $ age < 30) %>% 
 ggplot(aes(x=age, fill=marriage)) + 
  geom_bar(position = 'dodge')
 

welfare$age <- 2015 - welfare$birth
welfare %>% 
  filter(age >= 20 & age < 30) %>% 
  ggplot(aes(x = age, fill = marriage)) +
  geom_bar(position = "dodge") 

#################수업시작

age_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

ggplot(data = age_income, aes(x = age, y = mean_income)) +
    geom_line()

welfare <- welfare %>% 
  mutate(age_gen = ifelse(age < 30, 'young', 
                          ifelse(age <= 50, 'middle', 'old')))

table(welfare$age_gen)

welfare %>% 
  group_by(age_gen) %>% 
  summarise(mean_income = mean(income, na.rm=T)) %>% 
  ggplot(aes(x=age_gen, y=mean_income, fill = age_gen)) + 
  geom_col() +
  scale_x_discrete(limits = c('young', 'middle', 'old')) #young, middle, old 순서로 나타내고 싶을때


welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age_gen, gender) %>% 
  summarise(mean_income = mean(income)) %>% 
  ggplot(aes(x=age_gen, y=mean_income, fill=gender)) + geom_col(position = "dodge") +
  scale_x_discrete(limits = c('young', 'middle', 'old'))

  
welfare %>% filter(!is.na(income)) %>% 
  group_by(age, gender) %>% 
  summarise(mean_income = mean(income)) %>% 
  ggplot(aes(x = age, y = mean_income, color = gender)) +
  geom_line()











library(readxl)

welfare$code_job <- welfare$job


class(welfare$code_job)
table(welfare$code_job)
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet =2)
head(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, by = "code_job")
welfare$job.x <- welfare$job.y
welfare %>% rename()

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job.y) %>% 
  head(10)


job_income <- welfare %>% 
  filter(!is.na(job.x) & !is.na(income)) %>% 
  group_by(job.x) %>% 
  summarise(mean_income = mean(income))

head(job_income)

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10

ggplot(data= top10, aes(x=reorder(job.x, mean_income), y= mean_income)) + geom_col()+coord_flip()

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)

bottom10

ggplot(data = bottom10, aes(x=reorder(job.x, -mean_income), y= mean_income)) +
  geom_col()+
  coord_flip()+
  ylim(0, 850)



welfare 

job_male <- 
  welfare %>% 
  filter(!is.na(job) & gender == 'male') %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_female <- 
  welfare %>% 
  filter(!is.na(job) & gender == 'female') %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)  

ggplot(job_male, aes(x=reorder(job, n), y=n)) + geom_col()+coord_flip()
ggplot(job_female, aes(x=reorder(job, n), y=n)) + geom_col()+coord_flip()





######################################
####################################



        오후 수업


####################################
####################################




install.packages("multilinguer")
library(multilinguer)
multilinguer::install_jdk()

## 의존성 패키지 설치

install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")

install.packages("remotes")



## 깃허브 통해 KoNLP 다운로드

remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

​

또는 

remotes::install_github('haven-jeon/KoNLP', upgrade = "never", force = TRUE, INSTALL_opts=c("--no-multiarch"))

​

## 패키지 불러오기


library(KoNLP)
useNIADic()

text <- readLines("ahn.txt")
nouns <-extractNoun(text)

words <- unlist(nouns) #한줄로 보여줌
words <- words[nchar(words)>=2]   #nchar 글자 살리기   

df <- as.data.frame(table(words))
head(df)
df <- df %>% arrange(desc(Freq)) 

#  install.packages("wordcloud2")

wordcloud2(df)
ggplot(df, aes(x= reorder(words, Freq), y=Freq, fill=words)) + geom_col() + coord_flip()




#인터랙티브
install.packages("plotly")
library(plotly)
g <- ggplot(mpg, aes(x=displ, y=hwy, color=drv)) + geom_point()
ggplotly(ggplot(mpg, aes(x=displ, y=hwy, color=drv)) + geom_point())
ggplotly(g)

install.packages("dygraphs")
library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)
dygraph(eco)

dygraph(eco) %>% dyRangeSelector()

#여러 값 표현하기
저출산
eco_a <- xts(economics$psavert, order.by = economics$date)
실업자 수
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)

eco2 <- cbind(eco_a, eco_b)
colnames(eco2) <- c("psavert", "unemloy")
head(eco2)

dygraph(eco2) %>% dyRangeSelector()








#################### 예시 #######################

receipts <- data.frame(
  name = c("apple", "banana", "chocolate", "water", "coffee"),
  price = c(1000, 2000, 3000, 500, 4000),
  date = c("2022-02-01", "2022-02-02", "2022-02-03", "2022-02-03", "2022-02-04"),
  location = c("supermarket", "convenience store", "department store", "convenience store", "cafe")
)


#명사 추출하기
library(KoNLP)
useNIADic()

words <- unlist(extractNoun(receipts$name))
words <- words[nchar(words)>=2]
df_words <- as.data.frame(table(words))
head(df_words)


#워드클라우드 시각화하기
library(wordcloud2)
wordcloud2(df_words)


#시계열 그래프 그리기
install.packages("lubridate")
library(lubridate)
receipts$date <- ymd(receipts$date)
library(dygraphs)
library(xts)
receipts_xts <- xts(receipts$price, order.by = receipts$date)
dygraph(receipts_xts)

#히스토그램 그리기

library(ggplot2)
ggplot(receipts, aes(x=price)) + geom_histogram(binwidth=500) + labs(title="Histogram of Price")






