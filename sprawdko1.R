gry <- read_csv("gry.csv")
view(gry)

sapply(gry, function(x) length(unique(x)))
sapply(gry, class)

unique(gry$User_Score)

length(unique(dane$Name))
length(unique(dane$Year_of_Release))
