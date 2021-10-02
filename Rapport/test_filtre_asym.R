library(rjdfilters)
library(rJava)
trend.coefs = lp_filter()

X11SeasonalFiltersFactory = J("jdplus.x11plus.X11SeasonalFiltersFactory")
SeasonalFilterOption = J("jdplus.x11plus.SeasonalFilterOption")
finiteFilter = J("jdplus.math.linearfilters.FiniteFilter")
trend_h <- finiteFilter$of(trend.coefs$filters.coef[,"q=6"], -6L)
period = 12
P1 = new( J("java.lang.Integer"), as.character(period))
P = new( J("java.lang.Double"), as.character(period))
seas0 ="S3X3"
seas1 ="S3X5"
seas_filter <- function(seas, period){
  seas_to_glob <- function(x, period){
    seas_weights = x$weightsToArray()
    x$getLowerBound()
    global_w <- c(unlist(lapply(seas_weights[-length(seas_weights)],
                    function(x){
                      c(x, rep(0, period - 1))
                    })),
                  seas_weights[length(seas_weights)])
    finiteFilter$of(global_w, as.integer(x$getLowerBound()*period))
  }
  P = new( J("java.lang.Double"), as.character(period))
  seasFilter <- X11SeasonalFiltersFactory$filter(P,
                                                  SeasonalFilterOption$valueOf(seas))
  cf = seasFilter$centralFilter()
  endf = seasFilter$rightEndPointsFilters()
  endf[[2]]
  weightSeas = seas_to_glob(cf, period)
  asym_weights = lapply(endf, seas_to_glob, period = period)
  list(sym = weightSeas, asym = asym_weights)
}
Id <- finiteFilter$of(.jarray(1), 0L)
B12 <- finiteFilter$of(c(1,rep(0,11)), -12L)
B6 <- finiteFilter$of(c(1,rep(0,11)), -6L)
M2X12 <- J("jdplus.x11plus.X11FilterFactory")$makeSymmetricFilter(P)

s0_filters <- seas_filter(seas0, period)
s1_filters <- seas_filter(seas1, period)

s_coeff0 <- s0_filters$sym
s_coeff1 <- s1_filters$sym

tc_filter <- function(s_coeff0, s_coeff1){
  Id_minus_M2X12 <- M2X12$subtract(1, M2X12)
  si1 <- finiteFilter$subtract(Id, M2X12)
  s1 <- finiteFilter$multiply(Id_minus_M2X12,
                              finiteFilter$multiply(s_coeff0, si1))
  sa1 <- finiteFilter$subtract(Id, s1)
  tc2 <- finiteFilter$multiply(trend_h, sa1)
  si2 <- finiteFilter$subtract(Id, tc2)
  s2 <- finiteFilter$multiply(Id_minus_M2X12,
                              finiteFilter$multiply(s_coeff1, si2))
  sa2 <- finiteFilter$subtract(Id, s2)
  tc_f <- finiteFilter$multiply(trend_h, sa2)
  tc_f
}

s_coeff0 <- s0_filters$asym[[2]]
s_coeff1 <- s1_filters$asym[[3]]
trend_h <- finiteFilter$of(trend.coefs$filters.coef[,"q=0"], -6L)
tc_filter <- function(s_coeff0, s_coeff1){
  Id_minus_M2X12 <- M2X12$subtract(1, M2X12)
  si1 <- finiteFilter$subtract(Id, M2X12)
  # s1 <- finiteFilter$multiply(s_coeff0, si1)
  # snorm1 <- finiteFilter$multiply(s1, M2X12)
  # s1 <- finiteFilter$substract(finiteFilter$multiply(s1,B12),
  #                              finiteFilter$multiply(snorm1,B6))
  s1 <- finiteFilter$multiply(Id_minus_M2X12,
                              finiteFilter$multiply(finiteFilter$multiply(s_coeff0, si1,B6)))
  s1 <- finiteFilter$multiply(s1,B12)
  sa1 <- finiteFilter$subtract(Id, s1)
  tc2 <- finiteFilter$multiply(trend_h, sa1)
  si2 <- finiteFilter$subtract(Id, tc2)
  
  s2 <- finiteFilter$multiply(s_coeff1, si2)
  snorm2 <- finiteFilter$multiply(s1, M2X12)
  s2 <- finiteFilter$subtract(s2,
                               finiteFilter$multiply(snorm2,B6))
  
  sa_f <- finiteFilter$subtract(Id, s2)
  tc_f <- finiteFilter$multiply(trend_h, sa_f)
}

?x11
y <- retailsa$AllOtherGenMerchandiseStores
decomposition_lp <- x11(y, trend.coefs = trend.coefs)

.jmethods()
x = sa_f$weights()

jasym_filter(y, sa_f$weightsToArray(),
             lags = abs(sa_f$getLowerBound()))
jasym_filter(y, tc_f$weightsToArray(),
             lags = abs(tc_f$getLowerBound()))
4394.010
4462.826
tail(decomposition_lp$decomposition)
s_coeff0 <- s0_filters$asym[[2]]
s_coeff1 <- s1_filters$asym[[3]]
tc_f

plot(rjdfilters:::get_gain_function(M3),0,pi)

plot(rjdfilters:::get_gain_function(trend_h),0,pi)
plot(rjdfilters:::get_gain_function(tc_filter(s0_filters$sym, s1_filters$sym)),0,pi, col = "red", add = TRUE)
plot(rjdfilters:::get_gain_function(tc_filter(s0_filters$asym[[2]],
                                              s1_filters$asym[[3]])),
     0,pi, col = "blue", add = TRUE)

plot(rjdfilters:::get_gain_function(tc_f),0,pi, col = "blue", add = F)


####
y = rjdfilters::retailsa$AllOtherGenMerchandiseStores
d1  = J("demetra.data.DoubleSeq")$of(as.numeric(y))
filter = J("jdplus.x11plus.X11FilterFactory")$makeSymmetricFilter(P)
d2drop = filter$length() / 2;
# x = J("jdplus/x11plus/X11Kernel")$table(d1$length() - 2 * d2drop, NA);
x = lapply(rep(0, d1$length() - 2 * d2drop), \(x) new(J("java.lang.Double"),NaN))
x = .jarray(x,
            "java.lang.Double")
new(J("[D"),12)
new(J("java.lang.Double"),1)
.jmethods(J("[D"))
new(J("[I"), 12L)
x$getClass()
x = .jarray(rep(0, d1$length() - 2 * d2drop),
            "java.lang.Double")
for (i in 1:length(x)){
  x[[i]] <- new(J("java.lang.Double"),NaN)
}
out = J("jdplus.data.DataBlock")$of(x, 0L, as.integer(x$length))
filter$apply(d1, out);

d2 = DoubleSeq.of(x);
res = ts(.jevalArray(x), start = start(y), frequency = frequency(y))
ts.union(y,res)

s1$gainFunction()

rjdfilters::x11()
1/12
Id_minus_M2X12
pol = Id_minus_M2X12$coefficientsAsPolynomial()
pol$
s1$subtract(1, s1)
TC_1$weightsToArray()-M2X12$weightsToArray()
.jmethods2(Id,"multiply")
.jmethods2 <- function(x,y){
    grep(y,.jmethods(x),value = TRUE)
}
.jmethods2("jdplus/math/linearfilters/IFiniteFilter", "")

cf$setDefaultDecomposer()
.jmethods(cf)
tmp$getClass()
tmp$symmetricFilter()
tmp$endPointsFilters()
c(1/9, 2/9, 3/9, 2/9, 1/9)

c(1/9, 2/9, 3/9)
.jmethods(J("jdplus.math.linearfilters.BackFilter"))
new(J("jdplus.math.linearfilters.BackFilter"),1)
J("jdplus.math.linearfilters.BackFilter")$ofInternal(.jarray(c(1,2,3)))
t = tmp$endPointsFilters()
ma2x1 = .jarray(c(3.0 / 27, 7.0 / 27, 10.0 / 27, 7.0 / 27))
ma2x0 = .jarray(c(5.0 / 27, 11.0 / 27, 11.0 / 27))
M_2X1 = J("jdplus.math.linearfilters.FiniteFilter")$of(ma2x1, -2L)
M_2X0 = J("jdplus.math.linearfilters.FiniteFilter")$of(ma2x0, -2L)

FC3 = .jarray(list(M_2X1, M_2X0), "jdplus.math.linearfilters.FiniteFilter")
FC3$getClass()
efilter = new(J("jdplus.x11plus.AsymmetricEndPoints"), FC3,0L)
efilter$getClass()
efilter$
.jmethods(("jdplus.x11plus.AsymmetricEndPoints"))
new(J("jdplus.x11plus.X11SeasonalFiltersFactory$DefaultFilter"), 12L,
    J("jdplus.x11plus.X11SeasonalFiltersFactory")$S3X3,
    efilter)
inal FiniteFilter M_2X1 = FiniteFilter.of(ma2x1, -2);

final FiniteFilter M_2X0 = FiniteFilter.of(ma2x0, -2);
t$S3X3
t$
new DefaultFilter(period.intValue(), sfilter, new AsymmetricEndPoints(efilters, 0))

.jcall(tmp,)
.jmethods(tmp)
sym = tmp$symmetricFilter()
sym$coefficientsAsPolynomial()
.jmethods(sym)
M3 <- J("jdplus/math/linearfilters/SymmetricFilter")$of(J("demetra/data/DoubleSeq")$of(c(1/1/3,1/3, 1/3)))
M3$convolutionOf(M3)
M3$multiply(M3)
M3$plus(M3)
sym$multiply()

X11SeasonalFiltersFactory = J("jdplus.x11plus.X11SeasonalFiltersFactory")
SeasonalFilterOption = J("jdplus.x11plus.SeasonalFilterOption")

DecompositionMode = J("demetra.sa.DecompositionMode")
extreme.lsig = 1.5
extreme.usig = 2.5
context = J("jdplus.x11plus.X11Context")$
    builder()$
    mode(DecompositionMode$valueOf("Additive"))$
    period(P)$
    trendFiltering(J("jdplus.filters.LocalPolynomialFilterFactory")$of(new(J("jdplus.filters.LocalPolynomialFilterSpec"))))$
    initialSeasonalFiltering(X11SeasonalFiltersFactory$filter(P, SeasonalFilterOption$valueOf(seas0)))$
    finalSeasonalFiltering(X11SeasonalFiltersFactory$filter(P, SeasonalFilterOption$valueOf(seas1)))$
    lowerSigma(extreme.lsig)$
    upperSigma(extreme.usig)$
    build()
td = context$getTrendFiltering()
td$symmetricFilter()
bd <- context$builder()
bd$build()
X11Kernel = new(J("jdplus.x11plus.X11Kernel"))
X11Kernel$getBstep()
data = J("demetra.data.DoubleSeq")$of(as.numeric(rjdfilters::retailsa$AllOtherGenMerchandiseStores))
X11Kernel$process(data, context)
B = X11Kernel$getBstep()                
B$toString()
E = X11Kernel$getEstep()
E
X11Kernel$getCstep()

tmp = J("demetra.saexperimental.r.X11Decomposition$Results")$builder()
tmp$
