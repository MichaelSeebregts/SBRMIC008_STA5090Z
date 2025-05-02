library(mgcv)
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


b4 = gam(y ~ tp(x, z, k = 10), data = eg$data)
b4

vis.gam(b4, theta = 30, phi = 30)

ind = sample(1:n, 20, replace = FALSE)
b5 = gam(y ~ te(x, z, k = 20), data = eg$data, knots = list(x = eg$data$x[ind], z = eg$data$z[ind]))
b5

vis.gam(b5, theta = 30, phi = 30)

b6 = gam(y ~ te(x, z, fx = TRUE), data = eg$data)
b6

vis.gam(b6, theta = 30, phi = 30)
