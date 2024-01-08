library(tidyverse)

gry <- read_csv("gry.csv")
view(gry)

sapply(gry, function(x) length(unique(x)))
sapply(gry, class)

elim <- gry %>% mutate(across(ends_with("Sales"), ~replace(., .==0, NA))) %>% 
  mutate(across(ends_with("Sales"), ~replace(., . < quantile(., probs=0.25, na.rm=TRUE)-1.5*IQR(., na.rm=TRUE) | . > quantile(., probs=0.75, na.rm=TRUE)+1.5*IQR(., na.rm=TRUE), NA)))

normalize <- function(X) {
  X/max(X)
}

normed <- elim %>% mutate(across(ends_with("Sales"), ~ ./max(., na.rm=TRUE)))

sum(is.na(elim$JP_Sales))

ggarrange(d1, d2, nrow=2)

A = ks.test(normed$Global_Sales, normed$EU_Sales)

A$p.value                
A$statistic

normed %>% ggplot()+
  stat_ecdf(aes(Global_Sales, color="Global_Sales"), linewidth=0.6)+
  stat_ecdf(aes(EU_Sales, color="EU_Sales"), linewidth=0.5)+
  labs(y = "Dystrybuanta empiryczna",
       x = "Wartość",
       color = "Kolumna")+
  theme(axis.title=element_text(size=10))
