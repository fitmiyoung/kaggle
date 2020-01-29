install.packages('readxl')
install.packages('tidyverse')
library(readxl)
library(dplyr)

data_t <- read.csv("/Users/miyoungyoon/kaggle/titanic/titanic_data/test.csv")

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




