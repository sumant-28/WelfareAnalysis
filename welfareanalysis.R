# Motivations for writing this script

# 1. Provides an intuitive way to calculate welfare effects from the logit model using matrix manipulation
# 2. Exploits a data set where income is measured which provides a sanity check on estimates of how much a consumer would value a hypothetical change


# my first setup
rm(list = ls())
setwd("J:/R")

install.packages("mlogit")
library(mlogit)

data("Fishing", package = "mlogit")
head(Fishing, 3)

Fish <- mlogit.data(Fishing, shape = "wide", varying = 2:9, choice = "mode")

# Choose model with the best fit

m <- mlogit(mode ~ price | income | catch, Fish, reflevel = "charter")
summary(m)

# first find the consumer surplus before the change this should be easier than the process of imposing a counter factual

X <- model.matrix(m)
alt <- index(Fish)$alt
chid <- index(Fish)$chid
eXb <- as.numeric(exp(X %*% coef(m)))
SeXb <- tapply(eXb, chid, sum)
logSeXb <- log(SeXb)

addifnotzero <- function(vector, thesum) {
for (i in seq_along(vector)) {
  if (vector[i] != 0) {
    vector[i] <- vector[i] + thesum
  }
}
output <- vector
}

beachincome <- X[,4]
boatincome <- X[,5]
pierincome <- X[,6]

beachincome <- addifnotzero(beachincome, 1000)
boatincome <- addifnotzero(boatincome, 1000)
pierincome <- addifnotzero(pierincome, 1000)

nincome <- cbind(beachincome, boatincome, pierincome)

nX <- X
nX[,4:6] <- nincome
neXb <- as.numeric(exp(nX %*% coef(m)))
nSeXb <- tapply(neXb, chid, sum)
nlogSeXb <- log(nSeXb)

testing123 <- -(nlogSeXb - logSeXb)/coef(m)["price"]
