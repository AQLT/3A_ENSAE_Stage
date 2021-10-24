library(rjdfilters)
trend.coefs = lp_filter()
y <- retailsa$AllOtherGenMerchandiseStores
decomposition_lp <- x11(y, trend.coefs = trend.coefs)
dec_fun <- function(x){
    x11(x,trend.coefs = trend.coefs, period = 12,extreme.lsig = 500,extreme.usig = 500,mul = FALSE)
}

X <- diag(1, 169, 169)
apply(X,2, function(x){
    dec_fun
})