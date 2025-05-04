# Question 1

x = runif(100, 0, 1)
mu = 5 + sin(3*pi*(x - 0.6))
y = rnorm(100, mu, 0.5^2)

data = data.frame("y" = y, "x" = x)

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
    x = data$x
    y = data$y
    designMat = matrix(c(rep(1, length(x)), x), ncol = 2)
    for (i in 1:(length(knots)))
    {
      
      designMat = cbind(designMat, c(pmax(x - knots[i], 0)))
      
    }
    
    splineDatTemp = regress(designMat, y, x)
    splineDat = rbind(splineDat, splineDatTemp)
  }
  
  if (type == "Piecewise Cubic Polynomial")
  {
    
    for (i in 1:(length(knots) + 1))
    {
      if (i == 1)
      {
        
        tempPoints = data[data$x < knots[i], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(c(rep(1, length(x)), x, x^2, x^3), ncol = 4)
        
        splineDatTemp = regress(designMat, y, x)
        splineDat = splineDatTemp
      }
      
      if(1 < i & i < (length(knots) + 1))
      {
        tempPoints = data[knots[i-1] <= data$x & data$x < knots[i], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(c(rep(1, length(x)), x, x^2, x^3), ncol = 4)
        splineDatTemp = regress(designMat, y, x)
        
        splineDat = rbind(splineDat, splineDatTemp)
        
      }
      if(i == (length(knots) + 1))
      {
        tempPoints = data[data$x >= knots[i-1], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(c(rep(1, length(x)), x, x^2, x^3), ncol = 4)
        splineDatTemp = regress(designMat, y, x)
        
        splineDat = rbind(splineDat, splineDatTemp)
      }
    }
    
  }
  
  if (type == "Piecewise Cubic Polynomial and Continous at Knots")
  {
    x = data$x
    y = data$y
    designMat = matrix(c(rep(1, length(x)), x, x^2, x^3), ncol = 4)
    for (i in 1:(length(knots)))
    {
      
      designMat = cbind(designMat, c(pmax(x - knots[i], 0))^3)
      
    }
    
    splineDatTemp = regress(designMat, y, x)
    splineDat = rbind(splineDat, splineDatTemp)
  }
  
  if (type == "Piecewise Cubic Polynomial and Continous at 1st")
  {
    x = data$x
    y = data$y
    designMat = matrix(c(rep(1, length(x)), x, x^2, x^3), ncol = 4)
    for (i in 1:(length(knots)))
    {
      
      designMat = cbind(designMat, c(pmax(x - knots[i], 0)^3))
      designMat = cbind(designMat, c(pmax(x - knots[i], 0)^2))
      
    }
    
    splineDatTemp = regress(designMat, y, x)
    splineDat = rbind(splineDat, splineDatTemp)
  }
  
  if (type == "Piecewise Cubic Polynomial and Continous at 2nd")
  {
    x = data$x
    y = data$y
    designMat = matrix(c(rep(1, length(x)), x, x^2, x^3), ncol = 4)
    for (i in 1:(length(knots)))
    {
      
      designMat = cbind(designMat, c(pmax(x - knots[i], 0)^3))
      designMat = cbind(designMat, c(pmax(x - knots[i], 0)^2)) 
      designMat = cbind(designMat, c(pmax(x - knots[i], 0))) 
      
    }
    
    splineDatTemp = regress(designMat, y, x)
    splineDat = rbind(splineDat, splineDatTemp)
  }
  
  if (type == "Cubic Spline")
  {
    
  }
  
  if (type == "Natural Spline")
  {

  }
  
  if (type == "B Spline Basis")
  {
    
    
  }
  
  return(splineDat)
}

splines = Splines_Custom("Piecewise Cubic Polynomial and Continous at 2nd", c(0.33, 0.66), data)
splines

plot(data$y ~ data$x)
lines(splines[, 1] ~ splines[, 2])



