# Question 1

x = runif(100, 0, 1)
mu = 5 + sin(3*pi*(x - 0.6))
y = rnorm(100, mu, 0.5^2)

xseq = seq(0, 1, length.out = 500)

data = data.frame("y" = y, "x" = x)
dataSeq = data.frame("y" = xseq, "x" = xseq)

regress = function(designMat, y, x)
{
  betaHat = solve(t(designMat)%*%designMat)%*%t(designMat)%*%y
  print(dim(designMat))
  print(length(y))
  
  yHatTemp = designMat%*%betaHat
  
  splineDatTemp = data.frame(cbind(yHatTemp, x))
  return(splineDatTemp)
}

Splines_Custom = function(type, knots, data)
{
  data = data[order(data$x), ]
  splineDat = data.frame("y" = c(), "x" = c())
  if (type == "Piecewise Constant")
  {
    for (i in 1:(length(knots) + 1))
    {
      if (i == 1)
      {
        
        tempPoints = data[data$x < knots[i], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(rep(1, length(x)), ncol = 1)
        
        splineDatTemp = regress(designMat, y, x)
        splineDat = splineDatTemp
      }
      
      if(1 < i & i < (length(knots) + 1))
      {
        tempPoints = data[knots[i-1] <= data$x & data$x < knots[i], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(rep(1, length(x)), ncol = 1)
        splineDatTemp = regress(designMat, y, x)
        
        splineDat = rbind(splineDat, splineDatTemp)
        
      }
      if(i == (length(knots) + 1))
      {
        tempPoints = data[data$x >= knots[i-1], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(rep(1, length(x)), ncol = 1)
        splineDatTemp = regress(designMat, y, x)
        
        splineDat = rbind(splineDat, splineDatTemp)
      }
    }
    
  }
  
  if (type == "Piecewise Linear")
  {
  
    for (i in 1:(length(knots) + 1))
    {
      if (i == 1)
      {
        
        tempPoints = data[data$x < knots[i], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(c(rep(1, length(x)), x), ncol = 2)
        
        splineDatTemp = regress(designMat, y, x)
        splineDat = splineDatTemp
      }
      
      if(1 < i & i < (length(knots) + 1))
      {
        tempPoints = data[knots[i-1] <= data$x & data$x < knots[i], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(c(rep(1, length(x)), x), ncol = 2)
        splineDatTemp = regress(designMat, y, x)
        
        splineDat = rbind(splineDat, splineDatTemp)
        
      }
      if(i == (length(knots) + 1))
      {
        tempPoints = data[data$x >= knots[i-1], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(c(rep(1, length(x)), x), ncol = 2)
        splineDatTemp = regress(designMat, y, x)
        
        splineDat = rbind(splineDat, splineDatTemp)
      }
    }
  }
  
  if (type == "Broken Stick")
  {
    
  }
  
  if (type == "Piecewise Cubic Polynomial and Continous at Knots")
  {
    x = data$x
    y = data$y
    designMat = matrix(c(rep(1, length(x)), x, x^2, x^3), ncol = 4)
    for (i in 1:(length(knots)))
    {
      
      designMat = cbind(designMat, c(pmax(x - knots[i], 0))^3)
      designMat = cbind(designMat, c(pmax(x - knots[i], 0))^2)
      designMat = cbind(designMat, c(pmax(x - knots[i], 0)))
      
      
    }
    
    beta = solve(t(designMat)%*%designMat)%*%t(designMat)%*%y
    
    info = list("beta" = beta, "design" = designMat)
    return(info)
  }
  
  if (type == "Piecewise Cubic Polynomial and Continous at 1st")
  {
    x = data$x
    y = data$y
    designMat = matrix(c(rep(1, length(x)), x, x^2, x^3), ncol = 4)
    for (i in 1:(length(knots)))
    {
      
      designMat = cbind(designMat, c(pmax(x - knots[i], 0))^3)
      designMat = cbind(designMat, c(pmax(x - knots[i], 0))^2)
      
      
    }
    
    beta = solve(t(designMat)%*%designMat)%*%t(designMat)%*%y
    
    info = list("beta" = beta, "design" = designMat)
    return(info)
  }
  
  if (type == "Piecewise Cubic Polynomial and Continous at 2nd")
  {
    x = data$x
    y = data$y
    designMat = matrix(c(rep(1, length(x)), x, x^2, x^3), ncol = 4)
    for (i in 1:(length(knots)))
    {
      
      designMat = cbind(designMat, c(pmax(x - knots[i], 0))^3)
      
    }
    
    beta = solve(t(designMat)%*%designMat)%*%t(designMat)%*%y
    
    info = list("beta" = beta, "design" = designMat)
    return(info)
  }
  
  if (type == "Cubic Spline")
  {
    
  }
  
  if (type == "Natural Spline")
  {
    x = data$x
    y = data$y
    designMat = matrix(c(rep(1, length(x)), x), ncol = 2)
    
    xiK = knots[length(knots)]
    
    for (i in 1:(length(knots) - 1))
    {
      dKTemp = ((pmax(x - knots[i], 0)^3) - (pmax(x - xiK, 0)^3))/((xiK - knots[i]))
      
      designMat = cbind(designMat, dKTemp)
      
    }
    
    beta = solve(t(designMat)%*%designMat)%*%t(designMat)%*%y
    
    info = list("beta" = beta, "design" = designMat)
    return(info)
    
    
    
  }
  
  if (type == "B Spline Basis")
  {
    
    
  }
  
  return(splineDat)
}

library(splines)

splines = Splines_Custom("Natural Spline", c(0.33, 0.66), data)
splines

splineSeq = Splines_Custom("Natural Spline", c(0.33, 0.66), dataSeq)

plot(data$y ~ data$x)
yHat = splineSeq$design%*%splines$beta
lines(yHat ~ xseq)


