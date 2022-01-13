a<-c(0,1,2,3,4,5,6,7,8,9,10,0,0,0,0,1,1,1,0,1,0,0,0,1,1,1)

b<-rollapply(a, 4, FUN = sum, fill = NA, align = "right")

a
b


QCP4 <- function(x, threshold){
    if(max(x, na.rm = T) >= threshold){
        return(1)
      }
    else{
        return(0)
      }
}



d <- c(0,0,0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,3,0,0,0,0,0,3,3,3,0,0,0,0,0)*20000

e <- c(0,0,0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,3,0,0,0,0,0,3,3,3,0,0,0,0,0)
e
c <- rollapply(d, width = 3, FUN = QCP4, threshold = 20000, fill = NA, align = "center")
c
c <- rollapply(d, width = 7, FUN = QCP4, threshold = 50000, fill = NA, align = "center")
c


summary(data)

data %>%
  count(QCP1,QCP2,QCP3,QCP4)
