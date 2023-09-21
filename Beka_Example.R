library(tidyverse)

horses <- 20
cows <- 15
dogs <- 12
pigs <- 16
sheep <- 19

#create a pretend dataframe. I will save it as a tibble and work within the tidyverse.
fakedata <- as_tibble(rnorm(100, mean = 67, sd = 4))

#take a quick peek at the first few rows.The column name is "value.“
head(fakedata)

#crate a histogram, assigning it to an object of plot
myplot<-ggplot(fakedata, aes(x = value)) + 
  geom_histogram()

#look at it
myplot

#save it to my "figs" folder
ggsave(plot = myplot,
       filename = "figs/histogram.tiff",
       width = 5, height = 5, units = "in",
       device = "tiff",
       dpi = 400)

#Bailey has entered the Code 