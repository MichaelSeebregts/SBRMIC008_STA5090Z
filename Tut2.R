library(mgcv)
n = 5000
eg = gamSim(2, n = n, scale = 0.5)
names(eg)

truef = eg$truth 
persp(truef$x, truef$z, truef$f, col = heat.colors(40), main = "a. truth")

