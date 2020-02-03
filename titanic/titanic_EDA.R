install.packages('readxl')
install.packages('tidyverse')
install.packages('corrplot')
install.packages('vcd')
install.packages('alluvial')
install.packages('caret')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('caret')
install.packages('e1071')
library(readxl)
library(tidyverse)
library(corrplot)
library(vcd)
library(alluvial)
library(caret)
# model
library('rpart')
library(rpart.plot)
library(caret)
library(ggplot2)
library('e1071')


data_t <- read.csv("/Users/miyoungyoon/kaggle/titanic/titanic_data/test.csv")
data_tr <- read.csv("/Users/miyoungyoon/kaggle/titanic/titanic_data/train.csv")

train <- read.csv("/Users/miyoungyoon/kaggle/titanic/titanic_data/train.csv")


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
# (Caution: Sometimes a missing value might be read into R as a blank or empty string.)https://rpubs.com/spring16/184493
# 빈칸일 때 NA로 취급하게 만들어야 하나?
is.null(data_tr$Cabin)
is_empty(data_tr$Cabin[3])

data_tr$Cabin[3]
sum(data_tr$Cabin == "")
sum(data_tr$Embarked == "")


length(data_tr$Embarked)
data_tr$Embarked
is.na(data_tr$Embarked) 
is.null(data_tr$Embarked)


sapply(data_tr, function(x) sum(is.na(x)))
# na로 못 찾는 것 "" 으로 찾음 
sapply(data_tr, function(x) sum(x == ""))
#is.na로 못 찾은 Cabin, Embarked 의 빈 칸 찾아냄. 
# Cabin    Embarked   
# 687           2 


# Missing values: Embarked - "S"로 변경 
class(data_tr$Embarked) #factor
levels(data_tr$Embarked)
embarked_empty<-data_tr$Embarked==""
sum(is.na(embarked_empty))
data_tr$Embarked[which(is.na(data_tr$Embarked))]<-"S" 
data_tr$Embarked
#factor 타입일 때, factor요소로 안 넣으면 에러 남. 
 

# Missing values: Cabin - "번호 없으면 0, 있으면 1로 변환"
class(data_tr$Cabin)
data_tr$Cabin <- lapply(data_tr$Cabin, as.character)
data_tr$Cabin <- ifelse(data_tr$Cabin=="", 0, 1)
data_tr$Cabin


# Age는 평균값으로 채워넣기 
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

# Data manupulation
# 나이 분류해 그룹화 
data_tr<-data_tr %>%
  mutate(
    Age = ifelse(is.na(Age), mean(data_tr$Age, na.rm=TRUE), Age),
    'Age Group' = case_when(Age < 13 ~ "Age.0012",
                            Age >= 13 & Age < 18 ~ "Age.1317",
                            Age >= 18 & Age <60 ~ "Age.1859",
                            Age >= 60 ~ "Age.60Ov"))

#이름의 타이틀 
names <- data_tr$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)
data_tr$title <- title
table(title)

data_tr$title[data_tr$title %in% c("Mlle","Ms","Lady","Dona")] <-"Miss"
data_tr$title[data_tr$title=="Mme"]<-"Mrs"
data_tr$title[!(data_tr$title %in% c("Miss", "Mr", "Mrs"))] <-"Rare title"
data_tr$title <-as.factor(data_tr$title)
table(data_tr$Sex, data_tr$title)

data_tr %>%
  filter(Sex=="female", title=="Rare title")

data_tr$Survived<-factor(data_tr$Survived)
ggplot(data_tr, aes(title, fill=factor(Survived))) +
  geom_bar(stat="count", position="stack")+
  labs(x="title") + theme_gray()
#데이터가 factor 타입이 아니면 fill 이 안 먹으므로 factor()함수로 카테고리형 데이터로 변환 시켜야 함.

#Family Groups
#형제/배우자, 부모/자식 
data_tr$Fsize <- data_tr$SibSp+data_tr$Parch + 1 #1을 추가하는 것은 형제, 부모 없을 때 signle 로 표현하기 위해서

# 도표를 보면서 가족 수 그룹핑할 것 파악 
ggplot(data_tr, aes(x=Fsize, fill=Survived))+
  geom_bar(stat='count', position='dodge') 

sum(Fsize) #integer 값의 합. 행의 개수가 아님. 
count(Fsize) #integer는 카운트 함수가 안 먹음. 
class(Fsize)

Fsize_df<-as.data.frame(Fsize)
count(Fsize_df)
dim(Fsize_df)

data_tr$FamilySized[data_tr$Fsize == 1] <- 'Single'
data_tr$FamilySized[data_tr$Fsize < 5 & Fsize >= 2] <-'Small'
data_tr$FamilySized[data_tr$Fsize >= 5 ]<-'Big'
data_tr$FamilySized = as.factor(data_tr$FamilySized)


#고유번호를 골라서 재번호 부여하는 방법은? 
# Tickets: 번호의 중복 개수 추출 
ticket.unique <- rep(0, nrow(data_tr))
tickets <- unique(data_tr$Ticket)
tickets
length(tickets) #681개의 고유 티켓 번호 
for(i in 1:length(tickets)){ #4번째 티켓 들어오면, 
  current.ticket <- tickets[i] #current.ticket = ticket 4번째 고유 번호 '113803'들어감 
  party.indexes <- which(data_tr$Ticket == current.ticket) #현재 보고 있는 티켓번호와 고유번호가 매칭되는 행 번호가 저장됨 
  
  for(k in 1:length(party.indexes)){ 
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}

data_tr$ticket.unique <- ticket.unique

data_tr %>%
  filter(Ticket == "349909")


data_tr$ticket.size[data_tr$ticket.unique == 1]<-'Single'
data_tr$ticket.size[data_tr$ticket.unique <5 & data_tr$ticket.unique >=2] <- 'Small'
data_tr$ticket.size[data_tr$ticket.unique >= 5] <-'Big'

ggplot(data_tr, aes(x=ticket.unique, fill=Survived)) +
  geom_bar(stat='count', position='dodge')+
  scale_x_continuous(breaks=c(1:11))+
  labs(x='Ticket Size') + theme_grey()


data_tr<-data_tr %>%
  mutate(Survived = case_when(Survived == 1 ~ 'Yes',
                              Survived == 0 ~ 'No'))

crude_summary <- data_tr %>%
  select(PassengerId, Survived) %>%
  group_by(Survived) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))
crude_summary

crude_survrate <- crude_summary$freq[crude_summary$Survived=="Yes"]
crude_survrate

#EDA
#Pclass
ggplot(data_tr, aes(Pclass, fill=Survived)) +
  geom_bar(position = "fill") +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2)+
  ggtitle("Survival Rate by Class") +
  theme_minimal()+
  scale_y_continuous(labels = scales::percent) 


#Sex
ggplot(data_tr, aes(Sex, fill=Survived)) + 
  geom_bar(position="fill")+
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2)+
  ggtitle("Survival Rate by Sex") +
  theme_minimal()+
  scale_y_continuous(labels=scales::percent)

#Age: 제대로 표현 안됨 
ggplot(data_tr, aes(Age, fill=Survived)) +
  geom_bar(position="fill")+
  ylab("Survival Rate")+
  scale_y_continuous(labels=scales::percent)+
  geom_hline(yintercept=crude_survrate, col="white", lyt=2, size=2)+
  ggtitle("Survival Rate by Age") + 
  theme_minimal()

# Age_v2
tbl_age <- data_tr %>%
  select(Age, Survived) %>%
  group_by(Survived) %>%
  summarise(mean.age = mean(Age, na.rm=TRUE))

tbl_age

ggplot(data_tr, aes(Age, fill=Survived)) + 
  geom_histogram(aes(y=..density..), alpha=0.5)+
  geom_density(alpha=.2, aes(colour=Survived))+
  geom_vline(data=tbl_age, aes(xintercept=mean.age, colour=Survived), lty=2, size=1)+
  scale_y_continuous(labels=scales::percent)+
  ylab("Density")+
  ggtitle("Survival Rate by Age")+
  theme_minimal()

names(data_tr)

data_tr$`Age Group`<-as.factor(data_tr$`Age Group`)
ggplot(data_tr, aes(x='Age Group', fill=Survived)) +
  geom_bar(position="stack") 

class(data_tr$`Age Group`)  
levels(data_tr$`Age Group`)
#나이 그룹이 4가지인데, x축에 안 나오고 있음. 이유를 모르겠음. 
ggplot(data_tr, aes(x='Age Group', fill=Survived)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept = crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Age Group") 


# 안됨
ggplot(data_tr %>% filter('Age Group' == "Age.0012") , aes(x='Age Group', fill=Survived)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept = crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Age Group") 


# 형제/배우자 
class(data_tr$SibSp)
data_tr$SibSp
ggplot(data_tr, aes(factor(SibSp), fill=Survived)) +
  geom_bar(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Survival Rate") + 
  geom_hline(yintercept = crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival rate by SibSp")
#형제가 5,6명 되는가족은 전혀 생존하지 못 한 이유가 뭐지? 
data_tr %>% filter(SibSp>=5)


#자식/부모 - 나의 자식, 부모라는 것이겠지? 
class(data_tr$Parch)
ggplot(data_tr, aes(factor(Parch), fill=Survived)) +
  geom_bar(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Survival Rate") + 
  geom_hline(yintercept = crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival rate by Parch")

#승선한 항구 - ML에 큰 영향을 줄 것 같은 변수가 아닐 것 같음. 
str(data_tr$Embarked)
ggplot(data_tr, aes(Embarked, fill=Survived)) +
  geom_bar(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Survival Rate") + 
  geom_hline(yintercept = crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival rate by Embarked")

#title - Miss, Mrs, Rare title 의 생존이 freq = n/sum(n) 보다 이상임을 확인 
str(data_tr$title)
ggplot(data_tr, aes(title, fill=Survived))+
  geom_bar(position="fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Survial Rate") +
  geom_hline(yintercept = crude_survrate, col="white", lty=2, size=2)+
  ggtitle("Survival rate by title")

# FamilySized
str(data_tr$FamilySized)
ggplot(data_tr, aes(FamilySized, fill=Survived)) +
  geom_bar(position="fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept = crude_survrate, col="white", lty=2, size=2)+
  ggtitle("Survival rate by FamilySized")

str(data_tr$Fsize)
ggplot(data_tr, aes(factor(Fsize), fill=Survived)) +
  geom_bar(position="fill") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept = crude_survrate, col="white", lty=2, size=2)+
  ggtitle("Survival rate by FamilySized")


#Correlation Plot
tbl_corr <- data_tr %>%
  select(-PassengerId, -SibSp, -Parch) %>%
  select_if(is.numeric) %>%
  cor(use="complete.obs") %>%
  corrplot.mixed(tl.cex=0.85)
  
View(tbl_corr)

#Mosaic plot - 카테고리 데이터 탐색에 용이함 
tbl_mosaic <- data_tr %>%
  select(Survived, Pclass, Sex, AgeGroup='Age Group', title, Embarked, 'FamilySized') %>%
  mutate_all(as.factor)
head(tbl_mosaic)

# mosaic 챠트 쓰려면 vcd(Visualizing Categorical Data)패키지 설치 필요 
mosaic(~Pclass+Sex+Survived, data=tbl_mosaic, shade=TRUE, legend=TRUE)


#alluvial diagram
tbl_summary <- data_tr %>%
  group_by(Survived, Sex, Pclass, 'Age Group', title) %>%
  summarise(N=n()) %>%
  ungroup %>%
  na.omit

tbl_summary


alluvial(tbl_summary[, c(1:4)],
  freq=tbl_summary$N, border=NA,
  col=ifelse(tbl_summary$Survived == "Yes", "Red", "Gray"),
  cex=0.68, #cex는 라벨의 폰트 크기 
  ordering = list(
    order(tbl_summary$Survived, tbl_summary$Pclass==1),
    order(tbl_summary$Sex, tbl_summary$Pclass==1),
    NULL,
    NULL))


#Machine learning
# 준비 데이터: “Pclass”, “title”,“Sex”,“Embarked”,“FamilySized”,“ticket.size”

feature1<-data_tr[1:891, c("Pclass", "title", "Sex", "Embarked", "FamilySized", "ticket.size")]
response <- as.factor(data_tr$Survived)
feature1$Survived=as.factor(data_tr$Survived)

set.seed(500)
# createDataPartition 은 학습데이터와 테스트 데이터 분리에 사용하는 함수 
ind=createDataPartition(feature1$Survived, times=1, p=0.8, list=FALSE)
head(ind)
#학습데이터와 테스트 데이터 분리 
train_val=feature1[ind,]
test_val=feature1[-ind]

#비율 확인 
round(prop.table(table(train$Survived)*100), digits=1)

# Predictive Analysis 
# Decision tree
set.seed(1234)
Model_DT=rpart(Survived~., data=train_val, method='class')

rpart.plot(Model_DT, extra=3, fallen.leaves=F)

PRE_TDT=predict(Model_DT, data=train_val, type="class")
confusionMatrix(PRE_TDT, train_val$Survived)
# confusionMatrix 돌린후 Accuracy is 0.8432 나옴. 3개 변수로 예측

# 
set.seed(1234)
cv.10 <- createMultiFolds(train_val$Survived, k=10, times=10)

ctrl<-trainControl(method="repeatedcv", number=10, repeats=10, index=cv.10)

tain_val <- as.data.frame(train_val)

Model_CDT <- train(x=train_val[,-7], y=train_val[,7], method='rpart', tuneLength=30, 
                   trControl=ctrl)

rpart.plot(Model_CDT$finalModel, extra=3, fallen.leaves = FALSE)

# 
PRE_VDTS=predict(Model_CDT$finalModel,newdata=test_val,type="class")

confusionMatrix(PRE_VDTS,test_val$Survived)

