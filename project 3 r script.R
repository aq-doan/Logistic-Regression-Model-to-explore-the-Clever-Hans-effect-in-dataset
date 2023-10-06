install.packages("Sleuth3")
install.packages("ggplot")
library("Sleuth3")
library("ggplot2")

ex2120
head(ex2120)
str(ex2120)

#1 Using techniques from Week 10 calculate the relevant 
#odds ratio and create a confidence interval for it. 
#The following command may be useful. 
#tapply(ex2120$Success, ex2120$Treatment, sum) 
#It sums up the number of successful maze runs by 
#both the “bright” and “dull” rats.

tapply(ex2120$Success, ex2120$Treatment, sum)#bright 678 dull 440
ex2120$Failures <- 50 - ex2120$Success
failures <- tapply(ex2120$Failures, ex2120$Treatment, sum)
successes <- tapply(ex2120$Success, ex2120$Treatment, sum)
#2x2 table
rats <- cbind(successes, failures)
fisher.test(rats)
#check
bright_odds <- 678/822
bright_odds

addFailureEx2120 <- ex2120
addFailureEx2120$Failures <-50 - addFailureEx2120$Success
addFailureEx2120

#setup binary response 
binaryResponse <- cbind(ex2120$Success, ex2120$Failures)
binaryResponse

#logistic model for Treatment
fit_treatment <-glm(binaryResponse ~ Treatment,data=ex2120, family = binomial (link=logit))
summary(fit_treatment)
#logistic model for PriorExp
fit_priorExp <-glm(binaryResponse ~ PriorExp,data=ex2120, family = binomial (link=logit))
summary(fit_priorExp)
#logistic model for Day 
fit_Day <-glm(binaryResponse ~ Day ,data=ex2120, family = binomial (link=logit))
summary(fit_Day)
#logistic model for all variable
fit_all <-glm(binaryResponse ~ Day + PriorExp + Treatment,data=ex2120, family = binomial (link=logit))
summary(fit_all)
#logistic model for Day and Treatment
fit_DayTreament <-glm(binaryResponse ~ Day + Treatment,data=ex2120, family = binomial (link=logit))
summary(fit_DayTreament)
# variable selection fit_all
drop1(fit_all, test="Chisq")
fit_all.reduced <- step(fit_all, test="Chisq")
summary(fit_all.reduced)

fit_all.int <-glm(binaryResponse ~ Day*PriorExp*Treatment,data=ex2120, family = binomial (link=logit))
summary(fit_all.int)
fit_all.int.reduced <- step(fit_all.int, test="Chisq")
summary(fit_all.int.reduced)
anova(fit_all.reduced, fit_all.int.reduced, test="Chisq")


#variable selection for fit_Day and Treatment
drop1(fit_DayTreament, test="Chisq")
fit_DayTreament.reduced <- step(fit_DayTreament, test="Chisq")
summary(fit_DayTreament.reduced)

fit_DayTreament.int <-glm(binaryResponse ~ ex2120$Day*ex2120$Treatment, family = binomial (link=logit))
summary(fit_DayTreament.int)
fit_DayTreament.int.reduced <- step(fit_DayTreament.int, test="Chisq")
summary(fit_DayTreament.int.reduced)
anova(fit_DayTreament.reduced, fit_DayTreament.int.reduced, test="Chisq")
#goodnestoffit for Day and Treament
fit_DayTreament.residual.deviance <-summary(fit_DayTreament)$deviance #deviance
fit_DayTreament.residual.deviance
fit_DayTreament.dof <-summary(fit_DayTreament)$df.residual#degree of freedom
fit_DayTreament.dof
#p-value
1 - pchisq(fit_DayTreament.residual.deviance,fit_DayTreament.dof)

#goodnestoffit for all
fit_all.reduced.residual.deviance <-summary(fit_all.reduced)$deviance #deviance
fit_all.reduced.residual.deviance
fit_all.reduced.dof <-summary(fit_all.reduced)$df.residual#degree of freedom
fit_all.reduced.dof
#p-value
1 - pchisq(fit_all.reduced.residual.deviance,fit_all.reduced.dof)

#part3
#
exp(confint(fit_all))

model <-glm(binaryResponse ~ Day + PriorExp +Treatment ,data=ex2120, family = binomial (link=logit))
summary(model)
priorexp <- seq(-7,10,1)
day <- seq(1,5,1)
treat <- c("bright","dull")
grid <- expand.grid(PriorExp=priorexp, Treatment=treat, Day=day)

pr <- predict(model, newdata = grid, type="response")
toPlot <- cbind(grid, pr)
head(toPlot)
p <- ggplot(toPlot, aes(x=Day, y=pr, color=as.factor(PriorExp)))
p + geom_line() + facet_grid(~Treatment) +  labs(
  x = "Day",                   # x-axis label
  y = "Number of Success",    # y-axis label
  title = "Predicting Success Probability by Day and Prior Experience"  # Plot title
)
  priorexp <- seq(-7,10,1)
day <- seq(1,50,1)
treat <- c("bright","dull")
grid <- expand.grid(PriorExp=priorexp, Treatment=treat, Day=day)

p <- ggplot(ex2120, aes(x=Day, y= Success, group = Student))

p + labs(
  x = "Day",                   # x-axis label
  y = "Number of Success",    # y-axis label
  title = "Predicting Success Probability by Day and Prior Experience"  # Plot title
)+ geom_line(aes(color=PriorExp)) + facet_wrap(~Treatment) + theme_bw()

