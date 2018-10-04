# week 1 - General introduction to ML

# install.packages('kernlab')
library(kernlab)
data("spam")
set.seed(333)
smallSpam <- spam[sample(dim(spam)[1], 10) , ]
spamLabel <- (smallSpam$type == "spam") * 1  + 1
plot(smallSpam$capitalAve, col=spamLabel)
