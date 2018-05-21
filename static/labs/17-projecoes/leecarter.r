# Defining work directory
setwd("")

# Loading required packages
library("gdata")
library("reshape2")

# Reading Death counts file
# The file/spreadsheet used is nothing more than the death counts corrected by the completeness factors
# Columns are years and rows are age groups
deaths <- read.xls("Corrected_Data.xlsx", sheet = "Origial_Death_Counts", header = TRUE)
colnames(deaths) <- c("Age", seq(1980, 2010))

# Melting data to reshape it
deaths <- melt(deaths, id.vars = "Age", variable.name = "Year", value.name = "Deaths")

# Reading Population file (Exposures)
# The file/spreadsheet used is nothing more than theinterpolated population for each year 
# Columns are years and rows are age groups
population <- read.xls("Corrected_Data.xlsx", sheet = "Original_Population_Estimates", header = TRUE)
colnames(population) <- c("Age", seq(1980, 2010))

# Melting data to reshape it
population <- melt(population, id.vars = "Age", variable.name = "Year", value.name = "Population")

# Reading data into the Model
D.vec <- deaths$Deaths
E.vec <- population$Population

# Loading package
library(MortalitySmooth)

# Preparing the domains and data in matrices
A <- unique(as.numeric(as.character(deaths$Age)))
Y <- unique(deaths$Year)
D.mat <- matrix(D.vec, length(A), length(Y))
E.mat <- matrix(E.vec, length(A), length(Y))

# Select the data
agemax <- 80
x <- A[A <= agemax]
Y <- as.integer(Y)
y <- Y
D <- D.mat[A <= agemax, ]
E <- E.mat[A <= agemax, ]

# Fit with 2D P-splines
# Default: optimal smoothing parameters selected by BIC
fitPS <- Mort2Dsmooth(x = x, y = y, Z = D, offset = log(E))

# Default plot: shaded contour plot
plot(fitPS)

# Plotting with perspective plot
persp(x, y, fitPS$logmortality, theta = -30,
      col = "green", shade=TRUE, xlab = "Ages (0-80)",
      ylab="Years (1980 - 2010)", zlab = "Mortality rate (log)")

# Plotting deviance residuals
# Histogram
hist(residuals(fitPS), breaks=100)

# Interpolating death rates to each individual age
newages <- seq(0, 80, length = 81)
newdata <- list(x = newages)
pre <- predict(fitPS, newdata = newdata, se.fit = TRUE)

persp(newages, y, pre$fit, theta = -28,
      col="green", shade = 1, border = "black",  xlab = "Ages (0-80)",
      ylab="Years (1980-2010)", zlab="Mortality rate (log)")

rates <- exp(pre$fit)
colnames(rates) <- seq(1980, 2010)
rates <- melt(rates, value.name = "nMx")
colnames(rates) <- c("Age", "Year", "nMx")

# write.csv(exp(pre$fit), "Single_Age_Rates.csv")

# Projecting Death Rates for 2011 - 2025

# Forecasting LFPR using the Lee-Carter Method
# Original R code by Bernardo Queiroz

# Function to calculate Life Tables
life.table <- function(x, nMx){
  # simple lifetable using Keyfitz and Flieger separation factors and 
  # exponential tail of death distribution (to close out life table)
  b0 <- 0.07;   b1<- 1.7;      
  nmax <- length(x)
  #nMx = nDx/nKx   
  n <- c(diff(x), 999)          		  # width of the intervals
  nax <- n/2;		            	        # default to .5 of interval
  nax[1] <- b0 + b1 * nMx[1]    		  # from Keyfitz & Flieger(1968)
  nax[nmax] <- 1/nMx[nmax] 	  	      # e_x at open age interval
  nqx <- (n * nMx) / (1 + (n - nax) * nMx)
  nqx<-ifelse(nqx > 1, 1, nqx);		    # necessary for high nMx
  nqx[nmax] <- 1.0
  lx <- c(1, cumprod(1 - nqx));   	  # survivorship lx
  lx <- lx[1:length(nMx)]
  ndx <- lx * nqx;
  nLx <- n * lx - nax * ndx;      	 # equivalent to n*l(x+n) + (n-nax)*ndx
  nLx[nmax] <- lx[nmax] * nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
  ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax], NA);
  lt <- data.frame(Ages = x, nqx = nqx, lx = lx, ndx = ndx, nLx = nLx, Tx = Tx, ex = ex, nMx = nMx)
  return(lt)
}

# Function to get the life expectancy of a newborn

get.e0 <- function(x){
  return(life.table(newages, x)$ex[1])
}

# Function to estimate the parameters of the model
leecarter <- function(nmx){
  log.nmx <- log(nmx)
  ax <- apply(log.nmx, 2, mean)
  swept.mx <- sweep(log.nmx, 2, ax)
  svd.mx <- svd(swept.mx)
  bx <- svd.mx$v[, 1]/sum(svd.mx$v[, 1])
  kt <- svd.mx$d[1] * svd.mx$u[, 1] * sum(svd.mx$v[, 1])
  result <- list(ax = ax, bx = bx, kt = kt)
  return(result)
}

# Functions related to the "jump-off" fix
nmx.from.kt <- function (kt, ax, bx){
  # Derives mortality rates from kt mortality index, 
  #   per Lee-Carter method
  nmx <- exp((bx[1:80] * kt) + ax[1:80])
  nmx[nmx > 1] <- 1
  nmx[nmx == 0] <- 1
  return(nmx)
}

iterative.kt <- function (e0, ax, bx){
  # Given e(0), search for mortality index k(t) per Lee-Carter method
  step.size <- 20
  guess.kt <- 0
  last.guess <- c("high")
  how.close <- 5
  while(abs(how.close)>.001){
    nmx <- nmx.from.kt(guess.kt,ax,bx)
    guess.e0 <- get.e0(nmx)
    how.close <- e0 - guess.e0
    if (how.close>0){
      # our guess is too low and must decrease kt
      if (last.guess==c("low")) {step.size <- step.size/2
      }
      guess.kt <- guess.kt - step.size
      last.guess<- c("high")
    }
    if (how.close<0){
      # our guess is too high and must increase kt
      if (last.guess==c("high")) {step.size <- step.size/2
      }
      guess.kt <- guess.kt + step.size
      last.guess <- c("low")
    }
  }
  return (guess.kt)
}

# Estimate nMx from kt

nmx.from.kt <- function (kt, ax, bx){
  nmx <- exp((bx * kt) + ax)
  nmx[nmx>1] <- 1
  nmx[nmx==0] <- 1
  return(nmx)
}

# The file used as input in the function is the rates data.frame generated above

data <- dcast(rates, Age ~ Year, value.var = "nMx")
data <- data[, -1]
nmx <- t(data)

# Check to see if the get.e0 function works
e0.male <- apply(nmx, 1, get.e0)
e0.male <- unname(e0.male)

# The data.frame data already has the necessary format years x ages
years <- seq(1980, 2010)
f.years <- seq(2011, 2025)

# Adjusting the Lee-Carter Model for forecasting
model <- leecarter(nmx)

# Removing names from numeric object model$ax
model$ax <- unname(model$ax)

# Data Frame with the parameters
parameters <- data.frame(model$ax, model$bx)
kt <- model$kt

# Estimate second stage ("jump-off") kt

end.year <- 2010
start.year <- 1980
len.series <- end.year - start.year + 1
kt.secondstage <- rep(0,len.series)

for (i in 1:len.series){
  kt.secondstage[i] <- iterative.kt(e0.male[i], parameters$model.ax, parameters$model.bx)
}

# Generating new nmx based on the second stage kt

male.nmx.est <- matrix (0, length(years), length(newages))

for (i in 1:length(years)){
  year <- 1979 + i
  male.nmx.est[i, ] <- nmx.from.kt(kt.secondstage[i], parameters$model.ax, parameters$model.bx)
}

dimnames (male.nmx.est) <- list(seq(1980,2010), seq(0, 80))

# Preparing kt for forecasting
# kt.diff <- diff(kt.secondstage)
kt.diff <- diff(kt)
summary.kt <- summary(lm(kt.diff ~ 1))
kt.drift <- summary.kt$coefficients[1,1]
sec <- summary.kt$coefficients[1,2]
see <- summary.kt$sigma

# Actually forecasting kt
# mort.finalyear <- male.nmx.est[31,]
# kt.initial <- iterative.kt(unname(e0.male[31]), log(mort.finalyear), parameters$model.bx)
h <- seq(0, 14)
kt.stderr <- ( (h*see^2) + (h*sec)^2 )^.5
# kt.forecast <- kt.initial + (h * kt.drift)
kt.forecast <- tail(kt, 1) + (h * kt.drift)
kt.lo.forecast <- kt.forecast - (1.96*kt.stderr)
kt.hi.forecast <- kt.forecast + (1.96*kt.stderr)

f.nmx <- matrix(nrow = length(kt.forecast), ncol = length(newages))
for (i in 1:length(kt.forecast)){
  f.nmx[i, ] <- exp((model$bx * kt.forecast[i]) + model$ax)
}

f.nmx <- t(f.nmx)
f.nmx <- as.data.frame(f.nmx)

# Writing 
write.csv(f.nmx, "f.nmx.csv")

# Preparing Plot
library(reshape2) 
library(ggplot2)

colnames(data) <- years
colnames(f.nmx) <- f.years

row.names(data) <- newages
row.names(f.nmx) <- newages

m.data <- melt(data, value.name = "nMx", variable.name = "Year")
mf.nmx <- melt(f.nmx, value.name = "nMx", variable.name = "Year")

plot.data <- rbind(m.data, mf.nmx)
plot.data$age <- rep(seq(0, 80), each = 1, times = 46)

plot.data.select <- subset(plot.data, Year %in% c(1980, 1991, 2000, 2010, 2015, 2020, 2025))

qplot(data = plot.data.select, x = age, y = nMx, colour = factor(Year), geom = "line", 
      main = "Death Rates Brazil - Selected Years", xlab = "Ages", ylab = "Death Rates") + 
  facet_wrap(~Year) + theme(legend.position = "none")

qplot(data = plot.data.select, x = age, y = nMx, colour = factor(Year), geom = "line", 
      main = "Death Rates Brazil - Selected Years", xlab = "Ages", ylab = "Death Rates") + theme(legend.title = 
                                                                                                   element_text(size = 12, face = "bold")) +
  scale_color_discrete(name = "Year")

qplot(data = plot.data, x = age, y = nMx, colour = factor(Year), geom = "line", 
      main = "Death Rates Brazil", xlab = "Ages", ylab = "Death Rates") + 
  theme(legend.title = element_text(size = 12, face = "bold")) +
  scale_color_discrete(name = "Year") + guides(col = guide_legend(ncol = 4))

parameters$age <- seq(0, 80)
qplot(data = parameters, x = age, y = model.ax, geom = "line", main = "LFPR Model ax", ylab = "ax", xlab = "Ages")

qplot(data = parameters, x = age, y = model.bx, geom = "line", main = "LFPR Model bx", ylab = "bx", xlab = "Ages")

plot.kt <- kt
plot.kt <- as.data.frame(plot.kt)
plot.kt$years <- seq(1980, 2010)
qplot(data = plot.kt, x = years, y = kt, main = "LFPR Model kt", geom = "line", ylab = "kt", xlab = "Ages")


plot.f.kt <- c(kt, kt.forecast, kt.lo.forecast, kt.hi.forecast)
plot.f.kt <- as.data.frame(plot.f.kt)
plot.f.kt$Type <- c(rep("Forecast", times = sum(length(kt), length(kt.forecast))),
                    rep("Low", times = length(kt.lo.forecast)),
                    rep("High", times = length(kt.hi.forecast)))
plot.f.kt$Year <- c(rep(seq(1980, 2025), times = 1),
                    rep(seq(2011, 2025), times = 1),
                    rep(seq(2011, 2025), times = 1))
colnames(plot.f.kt) <- c("kt", "Type", "Year")

qplot(data = plot.f.kt, x = Year, y = kt, geom = "line", colour = Type, main = "Death Rates Model kt with Forecasts 
      and Confidence Interval", xlab = "Years") +
  scale_color_manual(values=c("#000000", "#006699", "#006699")) + 
  theme(legend.position = "none")

# Creating Life Tables for all Census years (1980 - 2010)

Census_1 <- subset(rates, Year %in% 1980)
LT1 <- life.table(newages, Census_1$nMx)

Census_2 <- subset(rates, Year %in% 1991)
LT2 <- life.table(newages, Census_2$nMx)

Census_3 <- subset(rates, Year %in% 2000)
LT3 <- life.table(newages, Census_3$nMx)

Census_4 <- subset(rates, Year %in% 2010)
LT4 <- life.table(newages, Census_4$nMx)

Projection_1 <- subset(mf.nmx, Year %in% 2015)
LT5 <- life.table(newages, Projection_1$nMx)

Projection_2 <- subset(mf.nmx, Year %in% 2020)
LT6 <- life.table(newages, Projection_2$nMx)

Projection_3 <- subset(mf.nmx, Year %in% 2025)
LT7 <- life.table(newages, Projection_3$nMx)

names <- list("LT1", "LT2", "LT3", "LT4", "LT5", "LT6", "LT7")

library(WriteXLS)
# WriteXLS(names, "LifeTables.xlsx")
