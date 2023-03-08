############### 복습하기 #################


#원하는 단어의 순서

scale_x_discrete(limits=c('young', 'middle', 'old'))


a <- data.frame(class = c(1,2,3,4,5), math=c(10,20,30,40,50))
b <- data.frame(class = c(1,2,3,4,5), english=c(15,25,35,45,55))

#합치기
ab <- left_join(a, b, by='class')
library(foreign)

welfare <- read.spss('Koweps_hpc10_2015_beta1.sav', to.data.frame = T)

welfare <-  welfare %>% select(  gender = h10_g3, birth = h10_g4,
           marriage = h10_g10, religion = h10_g11,
           income = p1002_8aq1, job = h10_eco9,
           region = h10_reg7)

head(welfare)

welfare <-  as_tibble(welfare)

#marriage 열이 1일 경우 '기혼' 3 미혼, 나머지는 결측지
# 새로운 칼럼 
welfare$group_marriage <- ifelse(welfare$marriage == 1 , 
                   '기혼', ifelse(welfare$marriage ==3, '미혼', NA))


welfare$group_marriage <- ifelse(welfare$group_marriage == "미혼", "이혼", welfare$group_marriage) 


#직업과 결혼여부가 결측치가 아닌 행만을 추출

welfare <- welfare %>% 
  filter(!is.na(job) & !is.na(group_marriage))

# 직업에 따른 이혼율 빈도 상위 10개
welfare %>% filter(!is.na(job)) %>% 
  group_by(job) %>% 
  filter(marriage == 3)

#직업에 따른 이혼율 빈도 상위 10개의 그래프


welfare %>% 
  group_by(job, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(total = sum(n), ratio = round(n/total*100, 1)) %>% 
  filter(group_marriage == "이혼") %>% select(job, ratio) %>% head(10) %>% 
  ggplot(aes(x=reorder(job, ratio), y=ratio, fill = job)) + 
  geom_col() + coord_flip()+  labs(title = '직업에 따른 이혼율', x='직업' , y='이혼유')



################################



       오전수업



################################



install.packages("rvest")
install.packages('stringr')


title = c()
body = c()





hdoc <- read_html('https://search.daum.net/search?w=news&nil_search=btn&DA=NTB&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4')
temp <- html_nodes(hdoc, ".tit_main.fn_tit_u")

paste(temp[1])
html_text(temp[1])
# hdoc %>% html_nodes(".tit_main.fn_tit_") %>% html_text()

temp <- html_nodes(hdoc, ".desc")
paste(temp[1])
html_text(temp[1])


title <- c(title, "a")
title <- c(title, "b")
title <- c(title, "c")
title 






title = c()
body = c()

url_1 <- 'https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4&p='

f
for(i in 1:3){
  
  url <- paste(url_1, i, sep = "")
  url_1 <- 'https://search.daum.net/search?nil_suggest=btn&w=news&DA=PGD&q=%EB%89%B4%EC%A7%84%EC%8A%A4&p='
  
  
  hdoc <- read_html(url)
  title_part <- hdoc %>%  html_nodes(".tit_main.fn_tit_u") %>% html_text()
  title <- c(title, title_part)
 
  body_part <- hdoc %>% html_nodes(".desc") %>% html_text()
  body <- c(body, body_part)
}


#news = data.frame(제목 = title, 본문 = body)
news = cbind(title, body)
write.csv(news, 'twive.csv')



################### 강사님꺼

title = c()
body = c()

url_1 <- "https://search.daum.net/search?nil_suggest=btn&w=news&DA=PGD&q=%EB%89%B4%EC%A7%84%EC%8A%A4&p="


for(i in 1:10){
  url <- paste(url_1, i, sep = "")
  print(url)
  
  hdoc <- read_html(url)
  
  title_part <- hdoc %>% html_nodes(".tit_main.fn_tit_u") %>% html_text()
  body_part <- hdoc %>% html_nodes(".desc") %>% html_text()
  
  title <- c(title, title_part)
  body <- c(body, body_part)
}

#news = data.frame(제목 = title, 본문 = body)

news = cbind(title, body)


news <-  data.frame(제목 = title, 본문 = body)

write.csv(news, "twice.csv") 
write.csv(news, '뉴진스.csv', fileEncoding = 'CP949')#CP949 윈도우에서 읽느 글꼴






library(KoNLP)

nouns <- extractNoun(text) 

words <- unlist(nouns) 
words <- words[nchar(words) >= 2]
df <- as.data.frame(table(words))
df <- df %>% arrange(desc(Freq)) %>% head(100)
wordcloud2(df) 







##################### 연습하기 #########################

url_base <- 'https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%EC%9D%B8%EB%8F%84%20%EC%93%B0%EB%A0%88%EA%B8%B0%EC%82%B0%20%ED%99%94%EC%9E%AC&p='

title <- c()
body <- c()

for (i in 1:3) {

  url <- paste0(url_base, i)
  
  hdoc <- read_html(url)
  
  title_part <- hdoc %>% html_nodes(".tit_main.fn_tit_u") %>% html_text()
  body_part <- hdoc %>% html_nodes(".desc") %>% html_text()
  
  title <- c(title, title_part)
  body <- c(body, body_part)
}

news <- data.frame(title, body)
write.csv(news, '인도.csv', fileEncoding = 'CP949', row.names = FALSE)



# 텍스트 데이터 생성
text <- paste(news$title, collapse = " ")

# 불용어(stopwords) 제거
text <- removeWords(text, stopwords("ko"))

# 빈도수 계산
word_count <- table(strsplit(text, "\\s+"))

# 빈도수 기준으로 내림차순 정렬
word_count <- sort(word_count, decreasing = TRUE)

# 빈도수 상위 20개 추출
word_top20 <- head(word_count, 20)

# 워드클라우드 생성
wordcloud2(data = word_top20, size = 1.5)









##########################




            오후수업





###########################









x <- 1:5
sample(x, 3)
sample(x, 3, replace = TRUE)



exam <-  read.csv("csv_exam.txt")
exam <-  exam[ , 2:ncol(exam) ]
exam <- exam[ , -1]

ncol(exam)
dim(exam)[2]
length(names(exam))

set.seed(1234)
sample(1:10,5)

exam[1:10, ]

exam[seq(1,20,2), ]
exam[-seq(1,20,2), ]






set.seed(1234)
exam[ sample(1:20, 10), ]


#5394를 뽑습니다.
diamonds[sample(1:53940, 5394), ]
num <- sample(nrow(diamonds), nrow(diamonds)*.1)
dia <- diamonds[num , ]
dia_2 <- diamonds[-num, ]


#diamonds 를5:3:2 비율로 서로 다른 세트
num <- sample(nrow(diamonds), nrow(diamonds)*5)]
num1 <- sample(nrow(diamonds), nrow(diamonds)*3)]

train <-diamonds[num , ]
temp <- diamonds[-num , ]
val <- temp[num1, ]
test <- temp[-num1, ]

sample_n(diamonds, nrow(diamonds)*0.5)
sample_frac(diamonds, 0.5)



sample(c("A","B"), 10, replace = T, prob = c(5,2))









n <- nrow(diamonds)

# 첫 번째 분할
set.seed(123)
num_1 <- sample(n, round(n * 0.5))
dia_1 <- diamonds[num_1, ]

# 두 번째 분할
set.seed(456)
num_2 <- sample(setdiff(1:n, num_1), round(n * 0.3))
dia_2 <- diamonds[num_2, ]

# 세 번째 분할
set.seed(789)
dia_3 <- diamonds[-c(num_1, num_2), ]








ggplot(dia, aes(x=carat, y=price, color = color))+geom_point()






ggplot(mpg, aes(x=drv, fill=drv)) + geom_bar() + 
  #scale_fill_hue(c=50)
  scale_fill_brewer(palette = 'set3')









str(USArrests)
library(tibble)


crime <-  rownames_to_column(USArrests, var = "state")
crime$state <-  tolower(crime$state)

str(crime)

install.packages("maps")

states_map <-  map_data("state")
str(states_map)


ggChoropleth(data = crime,
             aes(fill= Murder,
                 map_id = state),
                 map= states_map,
             interactive= T)










#p303
economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)


car_cor <- cor(mtcars) 
round(car_cor, 2)

install.packages("corrplot")
library(corrplot)

corrplot(car_cor, method = "number")

col <- colorRampPalette(c('#BB4444', '#EE9988', '#FFFFFF', '#77AADD', '#4477AA'))

corrplot(car_cor,
         method = "color",
         col = col(200),
         type = 'lower',
         order = 'hclust',
         addCoef.col = 'black',
         tl.col = "black",
         tl.srt = 45,
         diag = F)










