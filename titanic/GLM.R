#Titanic - learning
# Prediction using GLM

train <- read.csv("input/train.csv")
test  <- read.csv("input/test.csv")

m = glm(Survived ~ Sex + Age + Pclass, train, family=binomial)
#m = glm(Survived ~ Sex, train, family=binomial)

summary(m)

predict(m, data.frame(Sex='female', Age=22, Pclass=3))
predict(m, data.frame(Sex='male', Age=22, Pclass=3))
#predict(m, data.frame(Sex='female'))
#predict(m, data.frame(Sex='male'))

submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(m, test)
write.csv(submission, file = "GLM_r_submission.csv", row.names=FALSE)

