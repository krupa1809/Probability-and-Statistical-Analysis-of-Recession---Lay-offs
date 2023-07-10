main_data<-read.csv("FINAL DATA.csv")
main_data<-na.omit(main_data)
main_data$Percentage<-gsub("%","",as.character(main_data$Percentage))
main_data$Percentage<-as.numeric(main_data$Percentage)
summary(main_data)
main_data<-na.omit(main_data)
sd(main_data$after.layoff)
corTable <- cor(main_data[sapply(main_data, is.numeric)])
corTable
corrplot(corTable)

plot(main_data$after.layoff~main_data$Lay.off,xlim=c(0,5000),ylim=c(0,50000),xlab="layoff",ylab="Employee After Layoff",main="Regression Graph for Layoff and employee After the layoff",cex.main=1.0)
model<-lm(main_data$Lay.off~main_data$Percentage)
model
abline(lm(main_data$Lay.off~main_data$Percentage))

par(mfrow=c(1,1))
barplot(main_data$Percentage~main_data$Company,las=2,ylab="Percentage",xlab="",
        cex.lab=1.0,ylim=c(0,50),cex.names=0.5,col=rainbow(5),
        main="Bar Plot for Companies with layoff Percentage ",
        cex.main=1.0)

#mean
mean(main_data$Lay.off)
mean(main_data$Number.of.Employees)
mean(main_data$Percentage)
mean(main_data$after.layoff)

#variance

var(main_data$Lay.off)
var(main_data$Number.of.Employees)
var(main_data$Percentage)
var(main_data$after.layoff)

#t-test
t.test(main_data$Lay.off,main_data$after.layoff,paired=TRUE,conf.level=0.9)


# Create a data frame with the sample data
df <- data.frame(
  date = as.Date(c("2022-01-01","2022-02-01","2022-03-01","2022-04-01","2022-05-01","2022-06-01","2022-07-01","2022-08-01","2022-09-01","2022-10-01","2022-11-01","2022-12-01","2023-01-01","2023-02-01","2023-03-01","2023-04-01","2023-05-01","2023-06-01","2023-07-01","2023-08-01","2023-09-01","2023-10-01","2023-11-01","2023-12-01")),
  value1 = c(77, 8, 17, 32, 28, 39, 33, 56, 29, 45, 39, 54, 61, 47, 80, 72, 38, 51, 42, 20, 58, 40, 38, 25),
  value2 = c(136, 70, 94, 102, 122, 131, 115, 125, 108, 123, 106, 63, 86, 79, 20, 15, 25, 22, 20, 15, 35, 30, 20, 60)
)

# Set the plot size
options(repr.plot.width=10, repr.plot.height=5)

# Create a line chart with two lines
plot(df$date, df$value1, type="l", col="red", xlab="Date", ylab="Value", ylim=c(0,150), lwd=2)
lines(df$date, df$value2, col="blue", lwd=2)

# Add a legend
legend("topright", legend=c("Value1","Value2"), col=c("red","blue"), lwd=2)
