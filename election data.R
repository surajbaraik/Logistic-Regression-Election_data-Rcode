

library(data.table)

election_data <- read.csv(file.choose())
election_data<- na.omit(election_data)
View(election_data)
attach(election_data)
summary(election_data)

colnames(election_data)
plot(election_data)
elec_res <- glm(Result ~ Year+`Amount.Spent`+`Popularity.Rank`, data = election_data)
summary(elec_res)

library(MASS)
stepAIC(elec_res)

library(car)
vif(elec_res)

exp(coef(elec_res))



prob <- as.data.frame(predict(elec_res, type = c("response"), election_data))
final <- cbind(election_data,prob)
confusion <- table(prob>0.5, election_data$Result)
table(prob>0.5)

confusion

Accuracy <- sum(diag(confusion)/sum(confusion))
Accuracy
