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

b3 = gam(AvgRain ~ s(SOI) + s(year), data = meanDat)

plot(meanDat$SOI, meanDat$AvgRain, pch = 16, col = rgb(0, 0, 1, 0.4),
     xlab = "Year", ylab = "AvgRain", main = "GAM fit over data")

SOI_seq <- data.frame(SOI = seq(min(meanDat$SOI), max(meanDat$SOI), length.out = 200))
preds2 <- predict(b2, newdata = SOI_seq, se.fit = TRUE)

lines(SOI_seq$SOI, preds2$fit, col = "red", lwd = 2)

