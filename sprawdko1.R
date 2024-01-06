library(tidyverse)

gry <- read_csv("gry.csv")
view(gry)

gry_na <- drop_na(gry, any_of(c("NA_Sales", "EU_Sales", "JP_Sales", "Global_Sales", "Other_Sales")))
count(gry)

sapply(gry, function(x) length(unique(x)))
sapply(gry, class)

Q1 <- quantile(gry[2], probs=c(.25, .75), na.rm = TRUE)
iqr1 <- IQR(gry$Global_Sales, na.rm=TRUE)
Q2 <- quantile(gry$NA_Sales, probs=c(.25, .75), na.rm = TRUE)
iqr2 <- IQR(gry$NA_Sales, na.rm=TRUE)
Q3 <- quantile(gry$EU_Sales, probs=c(.25, .75), na.rm = TRUE)
iqr3 <- IQR(gry$EU_Sales, na.rm=TRUE)
Q4 <- quantile(gry$JP_Sales, probs=c(.25, .75), na.rm = TRUE)
iqr4 <- IQR(gry$JP_Sales, na.rm=TRUE)
Q5 <- quantile(gry$Other_Sales, probs=c(.25, .75), na.rm = TRUE)
iqr5 <- IQR(gry$Other_Sales, na.rm=TRUE)

eliminated <- gry %>%
  filter(Global_Sales > 0 & Global_Sales < (Q1[2]+1.5*iqr1) &
           NA_Sales > 0 & NA_Sales < (Q2[2]+1.5*iqr2) &
           EU_Sales > 0 & EU_Sales < (Q3[2]+1.5*iqr3) &
           JP_Sales > 0 & Other_Sales >0)

eliminated %>% ggplot(aes_string(x="JP_Sales"))+geom_histogram()

quantile(gry$"Global_Sales")

eliminated = list(length=5)
names = c("Global_Sales", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
for  (i in 1:5) {
  Q <- quantile(gry[names[i]], probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(gry[names[i]], na.rm=TRUE)
  
  eliminated[[i]] <- gry %>%
    filter(names[[i]] > (Q[1] - 1.5*iqr) & names[[i]] < (Q[2]+1.5*iqr) &
             Global_Sales > 0) 
}
eliminated



h = list(h1, h1, h1, h1, h1)
names = c("Global_Sales", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
for (i in 1:5) {
  h[[i]] <- eliminated %>% ggplot(aes_string(x=names[i]))+geom_histogram()
}
ggarrange(h[[1]], h[[2]], h[[3]], h[[4]], h[[5]])