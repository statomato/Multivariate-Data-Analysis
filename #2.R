#다변량과제 4장
data <- read.table("C:/대학원/2019-1/1.전공/1.다변량분석/CD (updated) of JW/table4_1.txt")
colnames(data) <- c("indep", "supp", "benev", "conform", "leader", "gender", "socio")

#a
par(mfrow=c(1,3))
qqnorm(data$indep, pch=1, frame=F, main="Independence"); qqline(data$indep, col="steelblue", lwd=2)
qqnorm(data$supp, pch=1, frame=F, main="Support"); qqline(data$supp, col="steelblue", lwd=2)
qqnorm(data$benev, pch=1, frame=F, main="Benevolence"); qqline(data$benev, col="steelblue", lwd=2)
par(mfrow=c(1,2))
qqnorm(data$conform, pch=1, frame=F, main="Conform"); qqline(data$conform, col="steelblue", lwd=2)
qqnorm(data$leader, pch=1, frame=F, main="Leader"); qqline(data$leader, col="steelblue", lwd=2)

#b
data <- data[,1:5]
n <- nrow(data); p <- ncol(data)
xbar <- colMeans(data[,1:5])
S <- cov(data[,1:5])
t <- 0
for(i in 1:n){
  x <- t(data[i, 1:5])
  t <- t + (t(x-xbar) %*% solve(S) %*% (x-xbar) <= qchisq(0.5, p))
}
t/n

result <- mvn(data=data, mvnTest="mardia")
result$multivariateNormality
mvn(data=data, mvnTest="hz")$multivariateNormality
mvn(data=data, mvnTest="royston")$multivariateNormality
mvn(data=data, mvnTest="dh")$multivariateNormality

result1 <- mvn(data=data, mvnTest="royston", univariateTest="SW", desc=TRUE)
result1$univariateNormality

#c
par(mfrow=c(1,3))
logindep <- log(data$indep); rootindep <- sqrt(data$indep)
qqnorm(data$indep, pch=1, frame=F, main="Independence"); qqline(data$indep, col="steelblue", lwd=2)
qqnorm(logindep, pch=1, frame=F, main="log Independence"); qqline(logindep, col="steelblue", lwd=2)
qqnorm(rootindep, pch=1, frame=F, main="root Independence"); qqline(rootindep, col="steelblue", lwd=2)

logsupp <- log(data$supp); rootsupp <- sqrt(data$supp)
qqnorm(data$supp, pch=1, frame=F, main="Support"); qqline(data$supp, col="steelblue", lwd=2)
qqnorm(logsupp, pch=1, frame=F, main="log Support"); qqline(logsupp, col="steelblue", lwd=2)
qqnorm(rootsupp, pch=1, frame=F, main="root Support"); qqline(rootsupp, col="steelblue", lwd=2)

logleader <- log(data$leader); rootleader <- sqrt(data$leader)
qqnorm(data$leader, pch=1, frame=F, main="Leader"); qqline(data$leader, col="steelblue", lwd=2)
qqnorm(logleader, pch=1, frame=F, main="log Leader"); qqline(logleader, col="steelblue", lwd=2)
qqnorm(rootleader, pch=1, frame=F, main="root Leader"); qqline(rootleader, col="steelblue", lwd=2)
