# Regression Models - Coursera
# 

install.packages('UsingR')

library(UsingR)
data("galton")
library(reshape)

long <- melt(galton)

g <-ggplot(long, aes(x = value, fill = variable)) 
g <- g + geom_histogram(colour = 'black', binwidth = 1)
g <- g + facet_grid(. ~ variable)
g


library(manipulate)
myHist <- function (mu) {
  mse <- mean((galton$child - mu)^2)
  g <- ggplot(galton, aes(x=child)) + geom_histogram(fill='salmon', colour='black', binwidth = 1)
  g <- g + geom_vline(xintercept = mu, size=3)
  g <- g + ggtitle(paste("mu = ", mu, " , MSE= ", round(mse,2), sep = ""))
  g
}
manipulate(myHist(mu), mu=slider(62,74, step = 0.5))


######

ggplot(galton, aes(x=parent, y=child)) + geom_point()

lm(I(child - mean(child)) ~ I(parent - mean(parent)) -1, data = galton)
