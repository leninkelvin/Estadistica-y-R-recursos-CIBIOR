# Blood pressure data 
dfBloodPressure <- read.csv("dsCardamom.csv")

# Blood pressure at time 1
BPTime01 <- dfBloodPressure$Before 

# Custom function to get mode(s)
getModes <- function(v, na.rm = TRUE) {
     if (na.rm) v <- v[!is.na(v)] # remove NA if TRUE
     tab <- table(v)
     maxFreq <- max(tab)
     modes <- names(tab)[tab == maxFreq]
     return(modes)
}

# Mean, median, mode
mean(BPTime01)
[1] 139.4
median((BPTime01))
[1] 139
getModes(BPTime01)
[1] "142"

