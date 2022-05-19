library("ggplot2")

insurance <- read.csv(file = "C:/Users/nguye/Downloads/insurance.csv",  header=TRUE, sep=",", stringsAsFactors = FALSE)

insurance$sex[which(insurance$sex=="male")] <- "0"
insurance$sex[which(insurance$sex=="female")] <- "1"
insurance$sex <- factor(insurance$sex, levels = c("0", "1"))

insurance$smoker[insurance$smoker=="no"] <- "0"
insurance$smoker[insurance$smoker=="yes"] <- "1"
insurance$smoker <- factor(insurance$smoker, levels = c("0", "1"))

insurance$region[insurance$region=="southwest"] <- "1"
insurance$region[insurance$region=="southeast"] <- "2"
insurance$region[insurance$region=="northwest"] <- "3"
insurance$region[insurance$region=="northeast"] <- "4"
insurance$region <- factor(insurance$region, levels = c("1", "2", "3", "4"))

mydata_1 <- insurance[which(insurance$region == "1"),]
mydata_2 <- insurance[which(insurance$region == "2"),]
mydata_3 <- insurance[which(insurance$region == "3"),]
mydata_4 <- insurance[which(insurance$region == "4"),]

linearmod_1 <- lm(charges ~ age+bmi+children, data = mydata_1, na.action = na.omit)
summary(linearmod_1)

linearmod_2 <- lm(charges ~ age+bmi+children, data = mydata_2, na.action = na.omit)
summary(linearmod_2)

linearmod_3 <- lm(charges ~ age+bmi+children, data = mydata_3, na.action = na.omit)
summary(linearmod_3)

linearmod_4 <- lm(charges ~ age+bmi+children, data = mydata_4, na.action = na.omit)
summary(linearmod_4)

par(mfrow = c(2,2), mar=c(4,4,4,4))
plot(linearmod_1)

