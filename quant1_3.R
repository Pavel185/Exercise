#install.packages("stabledist")
library("stabledist")
#install.packages("fitdistrplus")
library("fitdistrplus")
#install.packages("ggplot2")
library(ggplot2)

#install.packages("actuar")
library(actuar)

# set n-days for returns
n=10 
#define empty values/vectors
q001=c()
Kolmogorov_Smirnov=c()
number_of_trials=c()
ind=0
j=0
#initialize stopping condition
stop=100

while (stop>0.005){
	j=j+1
	#sample Price from distribution
	arr=rstable(750, alpha=1.7, beta=0.0, gamma = 1.0, delta = 1.0)
	#calculate return
	p10 <- sapply(11:length(arr), function(i) {(arr[i]-arr[i-10])/arr[i-10]})
	#p10<-p10/max(p10)
	#get quantile
	q001[j] <- quantile(p10, 0.01)

	#update stopping rule
	if ((j>100)&(j%%100==0)){
		m=mean(q001)
		s=sd(q001)
		stop=abs( (s*1.96/(j^(0.5)))/m )
		print(j)
		print( stop )
		#fit results by normal distribution
		fit1<-fitdist(-q001,"norm")
		ind=ind+1
		number_of_trials[ind]=j
		Kolmogorov_Smirnov[ind]=gofstat(fit1,fitnames = "norm")$ks
		print("-------------")
	}
}

plot(number_of_trials, Kolmogorov_Smirnov)

#fit histogram by distribution
fit1<-fitdist(-q001,"norm")
params <- fit1$estimate
xvals <- seq(0,100,0.5)
hist(-q001,probability=TRUE,breaks=50,xlim=c(0,100))
lines(xvals, dnorm(xvals,mean=params[1],sd=params[2]))

fit2<-fitdist(-q001,"burr", start = list(shape1 = 0.3, shape2 = 1,rate = 1))
params <- fit2$estimate
xvals <- seq(0,100,0.5)
hist(-q001,probability=TRUE,breaks=50,xlim=c(0,100))
lines(xvals, dburr(xvals,shape1=params[1],shape2=params[2],rate=params[3]))

#statistics for fit1 and fit2
gofstat(list(fit1,fit2),fitnames = c("norm","burr"))

#kernel density estimation
hist(-q001,probability=TRUE,breaks=50,xlim=c(0,100))
d<-density(x = -q001, kernel="gaussian", bw = "nrd")
lines(d)







