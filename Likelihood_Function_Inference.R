
# Chapter 5 examples:
# Example of statistical inference. Evans & Rosenthal Example 5.2.1
plot(seq(0,5, by=0.01), dexp(seq(0,5, by=0.01), rate=1),type='l',xlab ="x", ylab="f(x)",lwd=3,cex.lab=1.5)
dev.print(device=postscript,"inference.eps",width=7,height=7, horizontal=FALSE)
dev.off()


#Example of Statistical Models

quartz()
par(mfrow=c(2,3))

x = 0:8;  pdf = dpois(x, 1)
plot(x, pdf, type="h", lwd=3, col="blue", main="PMF of Pois(1)",cex.lab=1.5)

x = 0:8;  pdf = dpois(x, 2)
plot(x, pdf, type="h", lwd=3, col="blue", main="PMF of Pois(2)",cex.lab=1.5)

x = 0:8;  pdf = dpois(x, 3)
plot(x, pdf, type="h", lwd=3, col="blue", main="PMF of Pois(3)",cex.lab=1.5)

x = 0:8;  pdf = dpois(x, 4)
plot(x, pdf, type="h", lwd=3, col="blue", main="PMF of Pois(4)",cex.lab=1.5)

x = 0:8;  pdf = dpois(x, 5)
plot(x, pdf, type="h", lwd=3, col="blue", main="PMF of Pois(5)",cex.lab=1.5)

x = 0:8;  pdf = dpois(x, 6)
plot(x, pdf, type="h", lwd=3, col="blue", main="PMF of Pois(6)",cex.lab=1.5)
dev.print(device=postscript,"poisson_theta.eps",width=7,height=7, horizontal=FALSE)
dev.off()

#Example 5.3.2
plot(seq(0,7, by=0.01), dexp(seq(0,7, by=0.01), rate=1),type='l',xlab ="x", ylab="f(x)",lwd=3,cex.lab=1.5,col="skyblue")
lines(seq(0,7, by=0.01),dexp(seq(0,7, by=0.01), rate=2),lty=2, lwd=3, col="skyblue4")
legend(5,0.8, c("exp(1)", "exp(2)"),fill = c("skyblue","skyblue3"),cex=1,bty='n')
dev.print(device=postscript,"exp.eps",width=7,height=7, horizontal=FALSE)
dev.off()


# Histogram
quartz()
par(mfrow=c(3,1))
hist(rnorm(1000),1000,xlim=c(-3,3),main = "k = 1000: Too Noisy",xlab="z-score")
hist(rnorm(1000),11,xlim=c(-3,3), main = "k = 1+log2(n) = 11: Optimal",xlab="z-score")
hist(rnorm(1000),4,xlim=c(-3,3),main = "k = 4: Too Smooth",xlab="z-score")
dev.print(device=postscript,"hist_plot.eps",width=7,height=7, horizontal=FALSE)
dev.off()

# Liklihood Function
L = (1/24)*(theta^4)*exp(-theta)
plot(theta,L,type ='l',lwd=3,col="blue",cex.lab=1.5)
abline(v=4, col="red")
dev.print(device=postscript,"L_plot.eps",width=7,height=7, horizontal=FALSE)
dev.off()


L = (1/48)*(theta^6)*exp(-2*theta)
plot(theta,L,type ='l',lwd=3,col="blue",cex.lab=1.5)
abline(v=3, col="red")
dev.print(device=postscript,"L2_plot.eps",width=7,height=7, horizontal=FALSE)
dev.off()