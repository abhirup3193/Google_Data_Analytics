install.packages("dplyr")
library(dplyr)

data("ToothGrowth")
View(ToothGrowth)

filtered_tg <- filter(ToothGrowth,dose==0.5)
View(filtered_tg)

arrange(filtered_tg, len)

filtered_toothgrowth <- ToothGrowth %>% 
  filter(dose==0.5) %>%
  arrange(len)
View(filtered_toothgrowth)

filtered_toothgrowth <- ToothGrowth %>% 
  filter(dose==0.5) %>%
  group_by(supp) %>%
  summarise(mean_len = mean(len,na.rm = T),.group="drop")
View(filtered_toothgrowth)

