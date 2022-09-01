install.packages("tidyverse")

library(ggplot2)
library(palmerpenguins)
library(tidyverse)

drop_na(penguins)

ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g))
ggplot(data = penguins) + geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm))