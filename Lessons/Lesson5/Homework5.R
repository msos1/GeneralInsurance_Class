# ------------------Homework 5 -------------------- #

# ----- Data preparation (as in the lesson) ----- #
library(dplyr)
dt_Policy <- read.csv("C:\\Users\\Matej\\Documents\\github\\GeneralInsurance_Class\\Data\\lesson5_PolicyHistory.csv") %>%
  distinct(NrPolicy, NrObject, .keep_all = TRUE) 
dt_Claims <- read.csv("C:\\Users\\Matej\\Documents\\github\\GeneralInsurance_Class\\Data\\lesson5_Claims.csv") %>%
  distinct(NrClaim, .keep_all = TRUE)
dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject"))
library(lubridate)
dt_pol_w_claims <- 
  dt_pol_w_claims %>% mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start))
dt_pol_w_claims <- 
  dt_pol_w_claims %>% 
  mutate(Ult_Loss = Paid + Reserves,
         Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure))
  )

ggplot(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
       aes(x = Burning_Cost)) +
  geom_histogram()

# ----- Analysis ----- #

# First, let's take a closer look to variable "Capacity". We assume that this is some capacity of the car's engine in cm^3. (but I might be wrong)
library(ggplot2)
dt_pol_w_claims %>%
  ggplot(aes(y = Burning_Cost, x = Capacity)) + 
  geom_jitter()
summary(dt_pol_w_claims$Capacity)
# We can see, that an average car in our data has the Capacity around 1896, however, there are some of them over 20000 (might be some trucks or super-sport cars).
# dt_pol_w_claims %>% filter(Capacity > 20000)

# Let's take a GLM model with family "Gamma" (as we did during our last lesson), due to a fact, that Burning are closests to a Gamma distribution
# First we take as our dependent variable Capacity 
# We take only those with Burning costs from the interval (0, 100) to avoid outliers. We also take 
model1 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100), # (We might add some boundary to Capacity, but we aren't much into cars, so we don't know what might be the maximum or minimum value for Capacity)
              formula = Burning_Cost ~ Capacity,
              family = Gamma())
summary(model1)
# According to the model summary, we see, that Capacity is a significant variable in our model, and so might have a potential influence on our target.




# Next we will take a closer look to variable D_age to see if age of our client is significant variable to burning costs. We expect it to be significant, becuse riding skills and reflexes changes with our age, family situation, etc...
# (In our data is also a variable "D_age_banded", which is just variable "D_age" spreaded into categories of 5-years length. We will use the D_age variable, because there shouldn't be much difference.)
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age)) + 
  geom_jitter()
summary(dt_pol_w_claims$D_age)
# We see something strange here! Guys over 100 years, one 146 years old! We assume that there's a mistake in the data (we don't believe in miracles and reincarnation).
# An average client is about 45 years old
dt_pol_w_claims %>% 
  filter(D_age < 100) %>% # filter(Burning_Cost != 0, Burning_Cost < 100, D_age < 100)
  ggplot(aes(y = Burning_Cost, x = D_age)) + 
  geom_jitter()
# Now we get a more reasonable plot. Looks like the ones with the highest Burning Costs are between 40 and 45 years old, but we have a lot of data here, so the most risky ones might be the retired ones (>62 years).

# Let's get to the model. Once again, we take only those with Burning costs from the interval (0, 100) and those who are still alive.
model2 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100, D_age < 100),
              formula = Burning_Cost ~ D_age,
              family = Gamma())
summary(model2)
# Looks like age isn't a significant factor for us.


# Bonus. Let's see what happens, if we combine those two variables into one model. (I'm not sure if we can do this.)
model3 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100, D_age < 100),
              formula = Burning_Cost ~ Capacity + D_age,
              family = Gamma())
summary(model3)
# Seems like we've got the same results as before - "Capacity" is significant, "Age" isn't.


# We can continue with such analysis on many other variables. Next, we can probably build up a more difficult model with more variables.
# Personally, I'd expect, that vehicle type, vehicle brand and number of seats will not be significant for us. Taking just engine capacity and construct year might be more reasonable in our case.


