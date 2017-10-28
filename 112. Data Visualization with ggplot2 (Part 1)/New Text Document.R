d <- read.delim("clipboard")
head(d)


i <- read.delim("clipboard")
head(i)
i$diff <- i$Sunrise - i$Fajr
str(i)
i$Sunrise <- as.numeric(i$Sunrise)
i$Fajr <- as.numeric(i$Fajr)
i$diff

white <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep=';')
head(white)                  
red <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=';')
head(red)
