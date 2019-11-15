# Motivations for writing this script

# 1. Provides an intuitive way to calculate welfare effects from the logit model using matrix manipulation
# 2. Exploits a data set where income is measured which provides a sanity check on estimates of how much a consumer would value a hypothetical change


# my first setup
rm(list = ls())
setwd("J:/R")

install.packages("mlogit")
library(mlogit)
install.packages("dplyr")
library(dplyr)

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

consumersurplus <- -(nlogSeXb - logSeXb)/coef(m)["price"]

# Instead now let's calculate the welfare effects of reducing the price of all the alternatives by the same amount

# In theory the welfare benefit should be at least the cost of the uniform reduction in the market place

# First we need to restrict the sample to people who only have options that are relatively pricy

# It makes no sense to make a uniform reduction in price of say $10 when the lowest options to begin with are as little as $1.29

newFish <- Fish %>%
  filter(price > 10)

# with subset data it is now time to run regression again

m1 <- mlogit(mode ~ price | income | catch, newFish, reflevel = "charter")
summary(m1)

# this creates a problem with the way information is cut off from the original Fish data frame that is needed to interface with dplyr

# instead try to filter at the Fishing level then apply same transformation

newFishing <- Fishing %>%
  filter(price.beach > 10 & price.pier > 10 & price.boat > 10 & price.charter > 10)

# can also use the subset base R function newFishing <- subset(Fishing, price.beach > 10 & ...)

newFish <- mlogit.data(newFishing, shape = "wide", varying = 2:9, choice = "mode")

m2 <- mlogit(mode ~ price | income | catch, newFish, reflevel = "charter")
summary(m2)

# results not "as significant" as that calculated previously but sufficient for further analysis

X <- model.matrix(m2)
alt <- index(newFish)$alt
chid <- index(newFish)$chid
eXb <- as.numeric(exp(X %*% coef(m2)))
SeXb <- tapply(eXb, chid, sum)
logSeXb <- log(SeXb)

newprice <- X[,4] - 5
nX <- X
nX[,4] <- newprice
neXb <- as.numeric(exp(nX %*% coef(m2)))
nSeXb <- tapply(neXb, chid, sum)
nlogSeXb <- log(nSeXb)

consumersurplus <- -(nlogSeXb - logSeXb)/coef(m2)["price"]

m3 <- mlogit(mode ~ price + catch, newFish, reflevel = "charter")
summary(m3)

# the willingness to pay calculations

# for this go back to original model

m <- mlogit(mode ~ price | income | catch, Fish, reflevel = "charter")
summary(m)

# the willingness to pay for increased catch rate is the price coefficient divided by the catch coefficient

wtpchartercatch <- coef(m)["charter:catch"]/coef(m)["price"]
wtpbeachcatch <- coef(m)["beach:catch"]/coef(m)["price"]
wtpboatcatch <- coef(m)["boat:catch"]/coef(m)["price"]
wtppiercatch <- coef(m)["pier:catch"]/coef(m)["price"]

# alternatively simplify the model by calculating only one willingness to pay

m4 <- mlogit(mode ~ price + catch, Fish, reflevel = "charter")
summary(m4)

wtp <- coef(m4)["catch"]/coef(m4)["price"]

# consumers are only willing to pay ~$15 for a one unit increase in the catch rate (keep in mind this coefficient exceeds 1)
# this appears to be slightly on the smaller side

# following code compares how much consumers benefit from a one unit increase in the catch rate

X <- model.matrix(m4)
alt <- index(Fish)$alt
chid <- index(Fish)$chid
eXb <- as.numeric(exp(X %*% coef(m4)))
SeXb <- tapply(eXb, chid, sum)
logSeXb <- log(SeXb)

newcatch <- X[,5]
newcatch <- newcatch + 1
nX <- X
nX[,5] <- newcatch
neXb <- as.numeric(exp(nX %*% coef(m4)))
nSeXb <- tapply(neXb, chid, sum)
nlogSeXb <- log(nSeXb)

consumersurplus <- -(nlogSeXb - logSeXb)/coef(m4)["price"]

# consumers benefit the exact amount that they are willing to pay
