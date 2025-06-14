library(DAAG)
library(plotly)
library(mgcv)
library(splines)
# install.packages("DAAG", dependencies = TRUE)

data = bomsoi
head(bomsoi)

meanDat = data.frame("Year" = data$Year, "SOI" = data$SOI, "AvgRain" = data$avrain)

plot(meanDat$AvgRain ~ meanDat$Year)

yearLM = lm(AvgRain ~ Year, data = meanDat)

yearSeq = data.frame("Year" = seq(min(meanDat$Year), max(meanDat$Year), length = 200))

yHat = predict(yearLM, newdata = yearSeq, interval = "confidence")

lines(yearSeq$Year, yHat[, 1], col = "blue")

polygon(
  c(yearSeq$Year, rev(yearSeq$Year)), 
  c(yHat[, "lwr"], rev(yHat[, "upr"])), 
  col = rgb(0, 1, 0, 0.3), border = NA
)

plot(meanDat$AvgRain ~ meanDat$SOI)

SOILM = lm(AvgRain ~ SOI, data = meanDat)

SOISeq = data.frame("SOI" = seq(min(meanDat$SOI), max(meanDat$SOI), length = 200))

yHat2 = predict(SOILM, newdata = SOISeq, interval = "confidence")

lines(SOISeq$SOI, yHat2[, "fit"], col = "blue")

polygon(
  c(SOISeq$SOI, rev(SOISeq$SOI)), 
  c(yHat2[, "lwr"], rev(yHat2[, "upr"])), 
  col = rgb(0, 1, 0, 0.3), border = NA)

plot_ly(x = ~meanDat$SOI, y = ~meanDat$Year, z = ~meanDat$AvgRain, type = "scatter3d")

combLM = lm(AvgRain ~ SOI + Year, data = meanDat)

gridC <- expand.grid(SOI = seq(min(meanDat$SOI), max(meanDat$SOI), length = 200), 
                     Year = seq(min(meanDat$Year), max(meanDat$Year), length = 200))

gridC$AvgRain = predict(combLM, newdata = gridC)

SOI_seq <- seq(min(meanDat$SOI), max(meanDat$SOI), length.out = 200)
Year_seq <- seq(min(meanDat$Year), max(meanDat$Year), length.out = 200)

z_matrix <- matrix(gridC$AvgRain, nrow = length(SOI_seq), ncol = length(Year_seq))


plot_ly() %>%
  add_markers(
    x = ~meanDat$SOI, y = ~meanDat$Year, z = ~meanDat$AvgRain,
    marker = list(color = 'black'), name = "Data"
  ) %>%
  add_surface(
    x = ~SOI_seq, y = ~Year_seq, z = ~z_matrix,
    opacity = 0.6, colorscale = "Viridis", name = "Regression Surface"
  ) %>%
  layout(scene = list(
    xaxis = list(title = "SOI"),
    yaxis = list(title = "Year"),
    zaxis = list(title = "AvgRain")
  ))


b1 = gam(AvgRain ~ Year + s(Year, k = 25), data = meanDat, select = TRUE)
b1

plot(meanDat$Year, meanDat$AvgRain, pch = 16, col = rgb(0, 0, 1, 0.4),
     xlab = "Year", ylab = "AvgRain", main = "GAM fit over data")

year_seq <- data.frame(Year = seq(min(meanDat$Year), max(meanDat$Year), length.out = 200))
preds <- predict(b1, newdata = year_seq, se.fit = TRUE)

lines(year_seq$Year, preds$fit, col = "red", lwd = 2)

b2 = gam(AvgRain ~ s(SOI, k = 25), data = meanDat, select = TRUE)
b2

plot(meanDat$SOI, meanDat$AvgRain, pch = 16, col = rgb(0, 0, 1, 0.4),
     xlab = "Year", ylab = "AvgRain", main = "GAM fit over data")

SOI_seq <- data.frame(SOI = seq(min(meanDat$SOI), max(meanDat$SOI), length.out = 200))
preds2 <- predict(b2, newdata = SOI_seq, se.fit = TRUE)

lines(SOI_seq$SOI, preds2$fit, col = "red", lwd = 2)

b3 = gam(AvgRain ~ s(SOI) + s(Year), data = meanDat)

vis.gam(b3, theta = 30, phi = 30)

###
# Back fitting

y = meanDat$AvgRain
x = meanDat$SOI
z = meanDat$Year
n = length(y)

f1 = rep(0, n)
f2 = rep(0, n)

b0 = mean(y)

oldF1 = f1
oldF2 = f2

for (i in 1:50)
{
  r1 = y - b0 - f2 
  f1Fit = smooth.spline(x, r1, all.knots = TRUE)
  f1 = predict(f1Fit, x)$y
  f1 = f1 - mean(f1)
  
  r2 = y - b0 - f1
  f2Fit = smooth.spline(z, r2, all.knots = TRUE)
  f2 = predict(f2Fit, z)$y
  f2 = f2 - mean(f2)

  oldF1 = f1
  oldF2 = f2
  
}

fitted_vals <- b0 + f1 + f2



plot(x, y, pch = 20, col = "grey", main = "Fitted Curve over Actual Data", 
     xlab = "SOI", ylab = "AvgRain")

# Add the fitted smooth curve (b0 + f1)
ord = order(x)
lines(x[ord], (b0 + f1)[ord], col = "blue", lwd = 2)

plot(z, y, pch = 20, col = "grey", main = "Fitted Curve over Actual Data", 
     xlab = "Year", ylab = "AvgRain")

# Add the fitted smooth curve (b0 + f2)
ord = order(z)
lines(z[ord], (b0 + f2)[ord], col = "blue", lwd = 2)


#######
# Question 3/4 

set.seed(120)

x = seq(0, 2*pi, by = 0.1)
y = sin(x) + rnorm(length(x), 0, sd = sd(sin(x) / 2))
plot(y ~ x, las = 1)

B = bs(x, df = 10)


# Plot multiple splines
for (i in 1:20) {
  lines(x, B %*% rnorm(ncol(B)), col = rgb(0, 0, 1, 0.4))
}

gamMod = gam(y ~ s(x))

pred = predict(gamMod, type = "lpmatrix")
betaHat = coef(gamMod)
vB = vcov(gamMod)

betaSims = MASS::mvrnorm(20, mu = betaHat, Sigma = vB)

plot(y ~ x, las = 1)

for(i in 1:20)
{
  lines(x, pred%*%betaSims[i, ], col = "green")
}
length(pred)


# Question 5

cycleDat = MASS::mcycle




