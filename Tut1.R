# Question 1

x = runif(100, 0, 1)
mu = 5 + sin(3*pi*(x - 0.6))
y = rnorm(100, mu, 0.5^2)

data = data.frame("y" = y, "x" = x)

Splines_Custom = function(type, knots, data)
{
  data = data[order(data$x), ]
  
  if (type == "Piecewise Constant")
  {
    splineDat = data.frame("y" = c(), "x" = c())
    for (i in 1:(length(knots) + 1))
    {
      if (i == 1)
      {
        
        tempPoints = data[data$x < knots[i], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(rep(1, length(x)), ncol = 1)
        betaHat = solve(t(designMat)%*%designMat)%*%t(designMat)%*%y
        
        yHatTemp = designMat%*%betaHat
        
        splineDatTemp = data.frame("y" = yHatTemp, "x" = x)
        splineDat = rbind(splineDat, splineDatTemp)
      }
      
      if(1 < i & i < (length(knots) + 1))
      {
        tempPoints = data[knots[i-1] <= data$x & data$x < knots[i], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(rep(1, length(x)), ncol = 1)
        betaHat = solve(t(designMat)%*%designMat)%*%t(designMat)%*%y
        
        yHatTemp = designMat%*%betaHat
        
        splineDatTemp = data.frame("y" = yHatTemp, "x" = x)
        splineDat = rbind(splineDat, splineDatTemp)
        
      }
      if(i == (length(knots) + 1))
      {
        tempPoints = data[data$x >= knots[i-1], ]
        x = tempPoints$x
        y = tempPoints$y
        designMat = matrix(rep(1, length(x)), ncol = 1)
        betaHat = solve(t(designMat)%*%designMat)%*%t(designMat)%*%y
        
        yHatTemp = designMat%*%betaHat
        
        splineDatTemp = data.frame("y" = yHatTemp, "x" = x)
        splineDat = rbind(splineDat, splineDatTemp)
      }
    }
    
  }
  
  if (type == "Piecewise Linear")
  {
    
  }
  
  if (type == "Broken Stick")
  {
    
  }
  
  if (type == "Piecewise Cubic Polynomial")
  {
    
  }
  
  if (type == "Piecewise Cubic Polynomial and Continous at Knots")
  {
    
  }
  
  if (type == "Piecewise Cubic Polynomial and Continous at 1st")
  {
    
  }
  
  if (type == "Piecewise Cubic Polynomial and Continous at 2nd")
  {
    
  }
  
  return(splineDat)
}

splines = Splines_Custom("Piecewise Constant", c(0.33, 0.66), data)
splines



