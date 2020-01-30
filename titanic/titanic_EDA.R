install.packages('readxl')
install.packages('tidyverse')
library(readxl)
library(tidyverse)

data_t <- read.csv("/Users/miyoungyoon/kaggle/titanic/titanic_data/test.csv")
data_tr <- read.csv("/Users/miyoungyoon/kaggle/titanic/titanic_data/train.csv")

str(data_t)
head(data_t)
View(data_t)

# int값의 종류 확인, 관계된 가족의 명수로 int로 남겨둬야 함. Factor화 할 수 있는 것인지 확인했으나 숫자 타입이어야 함 
#sibsp: The dataset defines family relations in this way...
# Sibling = brother, sister, stepbrother, stepsister
# Spouse = husband, wife (mistresses and fiancés were ignored)
data_t$Parch

# parch: The dataset defines family relations in this way...
# Parent = mother, father
# Child = daughter, son, stepdaughter, stepson
# Some children travelled only with a nanny, therefore parch=0 for them.
data_t$SibSp

#자리의 등급이므로 Factor화 해도 될 것 같음. 1,2,3ㅇ으로 3가지 종류만 있음
data_t$Pclass

#이름에 Mr. Miss. Mrs. 3가지를 분리해 낼 수 있을 듯, 그러나 이름 가운데에 있음.  
data_t$Name

# 나이 분포를 히스토그램으로 확인
hist(data_t$Age)

# 티켓 번호로 무엇을 추측할 수 있는지 모르겠음. pclass와도 어떤 연관이 있는지 데이터만 봐서는 모르겠음. 
data_t$Ticket

#전체 데이터의 max, min, mean, median 값 확인 
summary(data_t)
summary(data_tr)
# Fare 표 값의 최소값과 최대값 차이가 엄청 큼. 생존 변수로 쓸 만하다는 추측이 듬. 

# train data
str(data_tr)
ggplot(data_tr, aes(x=Sex, y=Survived))+geom_jitter()

ggplot(data_tr, aes(Survived, fill=Sex))+geom_bar()
# Error: stat_count() must not be used with a y aesthetic.
# col=Sex로 했을 때 제대로 fill=Sex처럼 색이 표현 안됨. 

ggplot(data_tr, aes(x=Sex, y=Age))+geom_boxplot()

ggplot(data_tr, aes(x=Sex, y=Survived))+geom_boxplot()

ggplot(data_tr, aes(x=Pclass, fill=factor(Survived)))+geom_bar()
# stat='identity', position='dodge'안 먹음. 



#각 칼럼의 고유한 값 개수 확인 
lapply(data_tr, function(x) length(unique(x))) 
# PassengerId와 Name 891로 매칭 됨. 
# 그런데 나이는 89개로 사람 이름과 매칭이 안됨.
# 나이값이 없는 사람도 없음을 추측 

# na값 확인
is.na(data_tr$Age)

missing_values <- data_tr %>% summarize_all(funs(sum(is.na(.))/n()))
data_tr %>% summarize_all(funs(sum(is.na(.))))
missing_values
missing_values <- gather(missing_values, key="feature", value="missing_pct")
class(missing_values)

sum(is.na(data_tr$Cabin))
#Cabin도 빈 값이 있는데, 왜 안 잡히는거지? 

length(data_tr$Cabin)
colSums(is.na(data_tr))
colSums(is.na(data_t))
levels(data_tr$Cabin) #cabin이 factor임. 그래서 빈칸은 1개의 펙터로 취급하나? 


sapply(data_tr, function(x) sum(is.na(x)))

missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

# ?? . Missing Value: Age, Survived, Cabin 어떻게 처리해야 하나?
# Age는 평균값으로 채워넣기 
data_tr<-data_tr %>%
  mutate(
    Age = ifelse(is.na(Age), mean(data_tr$Age, na.rm=TRUE), Age),
    'Age Group' = case_when(Age < 13 ~ "Age.0012",
                            Age >= 13 & Age < 18 ~ "Age.1317",
                            Age >= 18 & Age <60 ~ "Age.1859",
                            Age >= 60 ~ "Age.60Ov"))

View(data_tr)


