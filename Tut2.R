library(mgcv)
library(splines)
n = 5000
eg = gamSim(2, n = n, scale = 0.5)
names(eg)

truef = eg$truth 
persp(truef$x, truef$z, truef$f, col = heat.colors(40), main = "a. truth")

rep((1:8-0.5)/8, rep(8,8))


b1 = gam(y ~ te(x, z, k = 10), data = eg$data)
b1

vis.gam(b1, theta = 30, phi = 30)

ind = sample(1:n, 20, replace = FALSE)
b2 = gam(y ~ te(x, z, k = 20), data = eg$data, knots = list(x = eg$data$x[ind], z = eg$data$z[ind]))
b2

vis.gam(b2, theta = 30, phi = 30)
  
b3 = gam(y ~ te(x, z, fx = TRUE), data = eg$data)
b3

vis.gam(b3, theta = 30, phi = 30)


b4 = gam(y ~ s(x, z, k = 10, bs = "tp"), data = eg$data)
b4

vis.gam(b4, theta = 30, phi = 30)

ind = sample(1:n, 20, replace = FALSE)
b5 = gam(y ~ s(x, z, k = 20, bs = "tp"), data = eg$data, knots = list(x = eg$data$x[ind], z = eg$data$z[ind]))
b5

vis.gam(b5, theta = 30, phi = 30)

b6 = gam(y ~ s(x, z, fx = TRUE, bs = "tp"), data = eg$data)
b6

vis.gam(b6, theta = 30, phi = 30)

x_knots = quantile(eg$data$x, probs = seq(0.1, 0.9, length.out = 5))
z_knots = quantile(eg$data$z, probs = seq(0.1, 0.9, length.out = 5))


bx = bs(eg$data$x, knots = x_knots, degree = 3, intercept = TRUE)
bz = bs(eg$data$z, knots = z_knots, degree = 3, intercept = TRUE)


G = matrix(0, nrow = 5000, ncol = ncol(bx)*ncol(bz))

colIndex = 1

for (i in 1:ncol(bx))
{
  for (j in 1:ncol(bz))
  {
    G[, colIndex] = bx[, i]*bz[, j]
    colIndex = colIndex + 1
  }
}

thetaHat = solve(t(G)%*%G)%*%t(G)%*%eg$data$y

x = eg$data$x
z = eg$data$z
y = eg$data$y

x_seq = seq(min(x), max(x), length.out = 200)
z_seq = seq(min(z), max(z), length.out = 200)

grid = expand.grid(x = x_seq, z = z_seq)

bx_grid = bs(grid$x, knots = x_knots, degree = 3, intercept = TRUE)
bz_grid = bs(grid$z, knots = z_knots, degree = 3, intercept = TRUE)


G_grid = matrix(0, nrow = nrow(grid), ncol = ncol(bx_grid)*ncol(bz_grid))
colIndex = 1

for (i in 1:ncol(bx_grid))
{
  for (j in 1:ncol(bz_grid))
  {
    G_grid[, colIndex] = bx_grid[, i]*bz_grid[, j]
    colIndex = colIndex + 1
  }
}


fittedGrid = G_grid%*%thetaHat

yMat = matrix(fittedGrid, nrow = length(x_seq), ncol = length(z_seq))

persp(x_seq, z_seq, yMat, col = "lightblue")

U = function(r)
{
  r2 = (r^2)*log(r + 1e-10)
}

n = length(y)

Basis = matrix(c(rep(1, n), x, z), nrow = n, byrow = FALSE)

x_index = sample(1:n, 10)
z_index = sample(1:n, 10)

knots = matrix(c(x[x_index], z[z_index]), ncol = 2, byrow = FALSE)

K = matrix(c(x, z), ncol = 2, byrow = FALSE)

for (i in 1:nrow(knots))
{
  temp = c()
  for (j in 1:n)
  {
    r = sqrt((x[j] - knots[i, 1])^2 + (z[j] - knots[i, 2])^2)
    
    r2 = (r^2)*log(r + 1e-10)
    temp = c(temp, r2)
  }
  Basis = cbind(Basis, temp)
    
}

BetaHat = solve(t(Basis)%*%Basis)%*%t(Basis)%*%y

grid = expand.grid(x = x_seq, z = z_seq)

basisGrid = matrix(c(rep(1, nrow(grid)), grid$x, grid$z), byrow = FALSE, ncol = 3)

for (i in 1:nrow(knots))
{
  temp = c()
  for (j in 1:nrow(basisGrid))
  {
    r = sqrt((grid$x[j] - knots[i, 1])^2 + (grid$z[j] - knots[i, 2])^2)
    
    r2 = (r^2)*log(r + 1e-10)
    temp = c(temp, r2)
  }
  basisGrid = cbind(basisGrid, temp)
  
}

yhat = basisGrid%*%BetaHat

zmat = matrix(yhat, nrow = length(x_seq))

persp(x_seq, z_seq, zmat, col = "lightblue")
