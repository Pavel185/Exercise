# install.packages("stabledist")
library("stabledist")
# install.packages("fitdistrplus")
library("fitdistrplus")
# install.packages("ggplot2")
library(ggplot2)
# install.packages("actuar")
library(actuar)


# set n-days for returns
n <- 10 
# define empty values/vectors
q001 <- c()
KolmogorovSmirnov <- c()
number.of.trials <- c()
ind <- 0
j <- 0
# initialize stopping condition
stop.condition <- 100
stop.iter <- c()

while (stop.condition > 0.025) {
  j <- j+1
  # sample 1-day returns
  r1 <- rstable(750, alpha = 1.7, beta = 0.0, gamma = 1.0, delta = 1.0)
  # calculate 10-days returns using 1-day returns
  r10 <- sapply( 1:(750-n+1), function(i) {prod(r1[i:(i+9)]+1)-1} )
  q001[j] <- quantile(r10, 0.01)

  # update stopping rule
  if ((j > 100) & (j %% 100 == 0)) {
    m <- mean(q001)
    s <- sd(q001)
    stop.condition <- abs( ( s * 1.96 / (j^(0.5)) )/m )
    print(j)
    print( stop.condition )

    # fit results by normal distribution
    fit1 <- fitdist(-q001, "norm")
    ind <- ind+1
    stop.iter[ind] <- stop.condition
    number.of.trials[ind] <- j
    KolmogorovSmirnov[ind] <- gofstat(fit1, fitnames = "norm")$ks
    print("-------------")
  }
}

plot(number.of.trials, KolmogorovSmirnov)
plot(number.of.trials, stop.iter)

# fit histogram by normal distribution
q001.norm <- q001/1023
fit1 <- fitdist(-q001.norm, "norm")
params <- fit1$estimate
xvals <- seq(0,100,1)
hist(-q001.norm, probability = TRUE, breaks = 500, xlim = c(0,100), xlab = "-q001/1023")
lines(xvals, dnorm(xvals, mean = params[1], sd = params[2]))

# fit histogram by burr distribution
fit2 <- fitdist(-q001.norm, "burr", method = "mle", start = list(shape1 = 0.5, shape2 = 3, rate = 0.0018))
params <- fit2$estimate
xvals <- seq(0,100,0.1)
hist(-q001.norm, probability = TRUE, breaks = 500, xlim = c(0,100), xlab = "-q001/1023")
lines(xvals, dburr(xvals, shape1 = params[1], shape2 = params[2], rate = params[3]))

# statistics for fit1 and fit2
gofstat(list(fit1, fit2), fitnames = c("norm", "burr"))

# kernel density estimation
hist(-q001.norm, probability = TRUE, breaks = 500, xlim = c(0,100), xlab = "-q001/1023")
d <- density(x = -q001.norm, kernel = "gaussian", bw = "nrd")
lines(d)

# kernel density estimation
hist(-q001.norm, probability = TRUE, breaks = 500, xlim = c(0,100), xlab = "-q001/1023")
d <- density(x = -q001.norm, kernel = "cosine", bw = "nrd")
lines(d)





