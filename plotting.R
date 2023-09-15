# -----------------------------------------
# ---- Basic Plotting and command ---------
# -----------------------------------------
setwd("F:/R_LEARNING/Basic")
mydata <- read.csv("regression_auto.csv")

help(curve)
x<-mpg
curve(x^2, -625, 625)

# normal distribution 
curve(dnorm(x),-5,5)

mpg <- mydata$mpg
weight1 <- mydata$weight1
plot(mpg,weight1)

# line charts
x <-c(1,3,5,7,9,11)
y <-c(2,9,6,7,10,4)
plot(x,y,type="l")
plot(x,y,type="l",lty=2,lwd=2, col="red") # lty is the type of the line;lwd is the thickness
plot(x,y,type="b", main="this is a plot") # main is for the title part
plot(x,y,type="b",pch=17) # pch change the shape of the point 
plot(x,y,type="o", xlim=c(0,20),ylim=c(0,20)) # change the length of x-axis and y-axis
plot(x,y,type="s", xlab="working age",ylab="salary") # change the name of x-axis and y-axis

plot(mpg,weight1)
olsreg <-lm(mpg~weight1)
summary(olsreg)

?abline
abline(a=0.31,b=0.97,lty=2,lwd=2) # add a new line on the plot
points(8,4,col="red") # add a new point on the plot
text(25,4,"outlier", pos=3) # put the text "outlier" on (25,4)
arrows(25,4,30,3.5, length=0.15) # add an arrow from (25,4) to (30,3.5)

# put multiple lines on a plot 
year <- c(2008,2009,2010,2011,2012,2013)
product1 <- c(0,3,6,9,7,8)
product2 <- c(1,2,3,5,9,6)
product3 <- c(2,4,4,2,3,2)
sales <- cbind(product1, product2,product3) # to make a matrix 
matplot(year, sales, type="b", lwd=c(1,2,3), col=c("black", "blue","red"))

?curve
?dnorm
curve(dnorm(x,0,1),-10,10,lwd=1, lty=1)
curve(dnorm(x,0,2), add=TRUE, lwd=2, lty=2)
curve(dnorm(x,0,3), add=TRUE, lwd=3,lty=3)
legend("topright",c("sigma=1","sigma=2","sigma=3"),lwd=1:3, lty=1:3) # 1:3 = c(1,2,3)
legend("topleft",expression(sigma==1, sigma==2, sigma==3),lwd=c(1,2,3),lty=c(1,2,3))
text(0,0.3, 
     expression(f(x)==frac(1,sqrt(2*pi)*sigma)*e^{-frac(x^2,2*sigma^2)}))

     