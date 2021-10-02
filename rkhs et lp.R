library(rjdfilters)

daf2 <- lp_filter(kernel = "Biweight", endpoints = "DAF",degree = 2)
daf3 <- lp_filter(kernel = "Biweight", endpoints = "DAF",degree = 3)

rkhs2 <- rkhs_filter(degree = 2, optimalbw = FALSE,passband = 6)
rkhs3 <- rkhs_filter(degree = 3, optimalbw = FALSE,bandwidth = 6)
daf2$filters.coef - daf3$filters.coef
rkhs2$filters.coef - rkhs3$filters.coef
daf2$filters.coef- rkhs3$filters.coef
daf3$filters.coef- rkhs_filter(degree = 3, optimalbw = FALSE,bandwidth = 7)$filters.coef
plot_gain(daf2, q= 1)
plot_gain(rkhs2, q= 0, add = TRUE)
apply(rkhs2$filters.coef,2,diagnostic_matrix,lags = 6)
rkhs_filter(degree = 1, optimalbw = FALSE,passband = 6)$filters.coef - rkhs_filter(degree = 3, optimalbw = FALSE,bandwidth = 13)$filters.coef

rkhs_filter(degree = 3, optimalbw = TRUE,passband = 11)$filters.coef - rkhs_filter(degree = 3, optimalbw = TRUE,passband = 10)$filters.coef

rkhs_filter(degree = 3, optimalbw = TRUE,passband = 11)$filters.coef

rkhs_filter(degree = 3,passband = 7)$filters.coef - rkhs_filter(degree = 3, optimalbw = TRUE,passband = 6)$filters.coef

rkhs_filter(degree = 3,passband = 8)$filters.coef

p = 3
h <- 6
X = sapply(0:p, function(x) seq(-h,h)^x)
K = diag(c(rev(rjdfilters::get_kernel("Biweight", h)[[1]]), rjdfilters::get_kernel("Biweight", h)[[1]][-1]))
q = 0
X_p = X[1:(h+q+1),]
K_p = K[1:(h+q+1),1:(h+q+1)]
e1 = matrix(0,nrow = p+1)
e1[1] = 1
K_p %*% X_p %*% solve(t(X_p) %*% K_p %*% X_p, e1)
daf3$filters.coef[1:7,"q=0"] - K_p %*% X_p %*% solve(t(X_p) %*% K_p %*% X_p, e1)
sum(daf3$filters.coef[1:7,"q=0"])
diagnostic_matrix(daf3$filters.coef[1:7,"q=0"], lags = 6)
K_t <- function(t){
    H_p = t(X_p) %*% K_p %*% X_p
    dH_p = det(H_p)
    H_p[1,] <- c(t^seq(0, p))
    det(H_p)/dH_p
}
K_t(0) * K[7,7]
t = 6
rkhs_filter(degree = 3, optimalbw = FALSE,bandwidth = h+1)$filters.coef[,"q=0"]
plot(Vectorize(K_t),-h,h)
K_p_c = rkhs_kernel(degree = 3)
K_p_c(0)
x <- sapply(seq(-h,0)/(h+1), K_p_c)
x / sum(x)
integrate(K_p_c, -1, 1)
