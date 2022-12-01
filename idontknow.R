# Grid of values for 'n'
n = seq(from=3,to=10,by=1)
# The three sequences of coefficients
coeff1 = 1/(n-2)
coeff2 = 1/n
# The plot
allValues = c(coeff1, coeff2)
yLim = c(min(allValues), max(allValues));
x11(); par(mfcol=c(1,3))
plot(n, coeff1, xlim=c(min(n),max(n)), ylim = yLim, xlab=' ', ylab=' ', main='Coefficients 1', type='l')
plot(n, coeff2, xlim=c(min(n),max(n)), ylim = yLim, xlab=' ', ylab=' ', main='Coefficients 2', type='b')
plot(n, coeff1, xlim=c(min(n),max(n)), ylim = yLim, xlab=' ', ylab=' ', main='All coefficients', type='l')
points(n, coeff2, type='b')
