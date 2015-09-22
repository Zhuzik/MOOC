enrollment<-read.csv("enrollment_train.csv")
users<-data.frame(table(enrollment[,2]))
summary(users)
course<-data.frame(table(enrollment[,3]))
summary(course)
drop<-read.csv("truth_train.csv")
enrollment<-merge(enrollment, drop, by.x = "enrollment_id",by.y = "X1" )

library(data.table)
event<-fread("log_train.csv")
table(event$source)
table(event$event)
object<-as.data.frame(table(event$object))
train<-merge(event, enrollment, by = "enrollment_id")
View(head(train))
table(train$X0)

enrollment_test<-read.csv("enrollment_test.csv")
event_test<-fread("log_test.csv")
test<-merge(event_test, enrollment_test, by = "enrollment_id")
View(head(test))

enID<-as.data.frame(table(train$enrollment_id))
cor(enID[,2],drop[,2])

library(reshape2)
sour<-dcast(train, enrollment_id~source)
ev<-dcast(train, enrollment_id~event)

train<-merge(sour, enrollment, by = "enrollment_id")
train<-merge(enID, train, by.y = "enrollment_id",by.x = "Var1" )
train<-merge(ev, train, by.x = "enrollment_id",by.y = "Var1" )

library(caret)
set.seed(825)
trainIndex <- createDataPartition(train$X0, p = .7,
                                  list = FALSE,
                                  times = 1)
dataTrain <- train[ trainIndex,]
dataTest  <- train[-trainIndex,]

y<-dataTrain$X0
yTest<-dataTest$X0

X<-dataTrain[,-c(1,14)]
dataTest<-dataTest[,-c(1,14)]

