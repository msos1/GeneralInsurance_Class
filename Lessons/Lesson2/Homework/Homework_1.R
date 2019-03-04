# Find out, which __year__ was the __most terrific__ for portfolio you have identified as __most profitable__ during the lesson and 
# show it on the chart using `ggplot2` package. Write an explanation about your findings into the code as comment.
# __Commit__ it to your repository into `Lessons/Lesson2/Homework`.

## Code
library(dplyr)
library(ggplot2)

dt_KPI_raw <- read.csv("C:\\Users\\Matej\\Documents\\github\\GeneralInsurance_Class\\Data\\lesson2_KPI.csv")

dt_KPI_raw$Premium[dt_KPI_raw$Premium < 0] <- 0 # Because 'Premium' can not be negative (probably)
# Maybe we should do more with cleaning our data set - for example clean negative Losses and Expenses etc.
# But, well, I have no idea from where this particular data set comes from and what really those numbers represent
# So, then, I'd rather trust that it's all perfect...

dt_KPI_raw %>% 
  mutate(UWR = Premium - Expenses - Losses) %>%  # Let's suppose this is the 'most profitable portfolio' (which I'm not sure about)
  group_by(Year) %>% 
  summarize(UWR = sum(UWR, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(Year, UWR), y = UWR)) + 
  geom_col()




## Your Explanation about analysis:

# We have slightly adjusted the data set and then we have taken the so called 'UnderWritingResult' from our data, grouped by Years
# Results are shown in histogram and we see that Year 2015 has the lowest value of UWR
# P.S.: I'm not really sure about those analysis