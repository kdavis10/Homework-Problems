

#ry(graphics)
library(ks)
library(triangle)
library(MASS)
library(rgl)
#install.packages("read_excel")
library(readxl)
#read drilling cost dataset
analysis <- read_excel("C:/Users/Kimbe/Documents/OneDrive/Documents/MSA/Simulation and Risk/Analysis_Data.xlsx", 
                       sheet = "Drilling Cost", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

#read price projections dataset
price.proj <- read_csv("C:/Users/Kimbe/Documents/OneDrive/Documents/MSA/Simulation and Risk/Price Projections - 2018-2040.csv")
price.proj = data.frame(price.proj)

################objective 1---simulating 2018 drilling cost
##get geometric annual change of crude oil assuming no change in 1990 and treat 2007 as an outlier
#have to delete the first two rows of the data frame 

return = data.frame(analysis[c(-3,-48),5])
return = return[,1]
return=return[-(1:2)]
return

##should we subset just on 1990 - 2006 (ignoring 2007)? 
#subset of the data from 1990-2006
return2 = data.frame(analysis[34:49,5])
return2 = return2[,1]
return2

#check for normality
qqnorm(return)
qqnorm(return2)

#initiating values
sim = 10000
end_year = 2018
start_year = 2006
init = data.frame(analysis)[47,2] 

#simulate geometric return from normal distribution (probably shouldn't need this )
mean = mean(return)
sd = sd(return)
r.value = matrix(rnorm((end_year-start_year)*sim,mean=mean,sd=sd),ncol=sim)
r.sum = init * exp(apply(r.value,2,sum))
hist(r.sum,breaks=100,main = 'Normality Assumption Simulation',xlab = 'drilling cost')
mean(r.sum)
sd(r.sum)
quantile(r.sum,0.95)
quantile(r.sum,0.05)

#simulate geometric return from kernel estimation - with full numbers 
density.return = density(return, bw="SJ-ste")
density.return   ##bw=0.06332
Est.return=rkde(fhat=kde(return, h=0.06332),n=sim*(end_year-start_year))
hist(Est.return,breaks=100)
r1.value = matrix(as.numeric(Est.return),ncol=sim)
r1.sum = init * exp(apply(r1.value,2,sum))
hist(r1.sum,breaks=100,main = 'Kernel Estimate Simulation',xlab = 'drilling cost')
mean(r1.sum)
sd(r1.sum)
quantile(r1.sum,0.95)
quantile(r1.sum,0.05)

#simulate geometric return from kernel estimation - with 1990-2006 
density.return2 = density(return2, bw="SJ-ste")
density.return2 #bw=0.08908
Est.return2=rkde(fhat=kde(return2, h=0.08908),n=sim*(end_year-start_year))
hist(Est.return2,breaks=100)
r1.value = matrix(as.numeric(Est.return2),ncol=sim)
r1.sum = init * exp(apply(r1.value,2,sum))
hist(r1.sum,breaks=100,main = 'Kernel Estimate Simulation 1990-2006',xlab = 'drilling cost')
mean(r1.sum)
sd(r1.sum)
quantile(r1.sum,0.95)
quantile(r1.sum,0.05)



##############objective 2 --- NPV
##we need the risk-free interest rate to calculate present value, but we don't have it. 
##So I am going to simulate net value for 2018 only
quant_low = 3285
quant_high = 5475
quant_most = 4015
quant = rtriangle(sim,a=quant_low,b=quant_high,c=quant_most)
#ask Jiajing about the hard coded inputs on cost
Var.Cost = 1 + 0.004*quant + rnorm(sim, mean=0, sd=sqrt(0.8))
Price = rtriangle(sim, a=price.proj[1,3], b=price.proj[1,2], c=price.proj[1,4])
#shouldn't we use the r1.sum because that is from the kernel estimation (r.sum is from the normal)
Fixed.Cost = r1.sum
Net.Revenue <- (Price - Var.Cost)*quant - Fixed.Cost
hist(Net.Revenue,breaks=100,main = 'Sampling Distribution of Net Revenue',xlab = 'Net Revenue')

# Kernel Estimate of Net Revenues #
D.Net.Revenue <- density(Net.Revenue)
hist(Net.Revenue, breaks=50, prob=TRUE, main='Sampling Distribution of Net Revenue', xlab='Net Revenue')
lines(D.Net.Revenue, col="blue", lwd=2)