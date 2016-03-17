setwd("G:/SLM EXERCISES/Exercise 2")
myData <- read.table("Education.txt", header = T)

# CLEAN THE DATA
myData1 <- myData[-c(234),]
myData2 <- myData1[-c(107),]
myData3 <- myData2[-c(433),]
# We also get rid of the Id column which is not useful
usefuldata <- myData3[,-1]

attach(usefuldata)

# QUESTION 1
plot(Wage~Education)

plot(Wage~Education, main = "Wage explained by Education", xlab = "Education (year)", ylab = "Wage per month (Chf)")
detach(usefuldata)

# QUESTION 2
men <- usefuldata$Gender == "1"
dataMen <- usefuldata[men,-2]

women <- usefuldata$Gender == "2"
dataWomen <- usefuldata[women,-2]

plot(dataMen$Wage~dataMen$Education, main = "Wage explained by Education (MEN ONLY)", xlab = "Education (year)", ylab = "Wage per month (Chf)")

plot(dataWomen$Wage~dataWomen$Education, main = "Wage explained by Education (WOMEN ONLY)", xlab = "Education (year)", ylab = "Wage per month (Chf)")

plot(dataMen$Wage~dataMen$Education, main = "Wage explained by Education", xlab = "Education (year)", ylab = "Wage per month (Chf)", col = "blue")
lines(dataWomen$Wage~dataWomen$Education , type = "p" ,  col = "pink")
text(8,8000, "Blue for men", col = "blue")
text(8,7000, "Pink for women", col = "pink")

# QUESTION 3
myTime <- read.table("Mean20.txt", header = T)
mean(myTime)

# We ask R which is the row where na value is located
which(is.na(myTime))
times <- myTime[-c(20),]
min(times)

# We ask R which is the row where the min value is located
which(times == min(times))
times <- times[-c(21)]

# So, the mean, the median, the standard deviation, the minimum and maximum value are:
mean(times); median(times); sd(times); min(times); max(times)

# QUESTION 4
t.test(times, mu = 7.05, alternative = "two.sided")

# Now we consider original data
t.test(myTime, mu = 7.05, alternative = "two.sided") 
# Why do the results differ?
getOption("na.action") # So the na value is ignored
min(times)
min(myTime[!is.na(myTime)])

# QUESTION 5
t.test(times, mu = 7.05, alternative = "greater")
