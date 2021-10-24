library(rjdfilters)
kernel = "Henderson"
filters2 <- rjdfilters::lp_filter(degree = 2, endpoints = "DAF",kernel = kernel)$filters.coef
filters3 <- rjdfilters::lp_filter(degree = 3, endpoints = "DAF",kernel = kernel)$filters.coef
filters2 - filters3
p <- 3
h <- 6
q = 0

k = rjdfilters::get_kernel(horizon = h,kernel = kernel)
K <- diag(1/c(rev(k$coef[-1]),k$coef))

C = sapply(0:p, \(d) seq(-h,h)^d)
I = diag(1,nrow = 2*h+1,ncol = 2*h+1)
L1 <- I[,1:(q+h+1)]
L2 <- I[,-(1:(q+h+1))]
E1 <- K
E1_m1 <- solve(E1)
G = E1_m1-E1_m1 %*% C %*% 
    solve(t(C) %*% E1_m1 %*% C, t(C) %*% E1_m1)
u=t(L1)%*%(I-G%*%L2 %*% 
               solve(t(L2) %*% G %*% L2, t(L2))
)
res = u%*%matrix(filters[,"q=6"], ncol = 1)
c(res)-filters2[1:7,"q=0"]
c(res)-filters3[1:7,"q=0"]
