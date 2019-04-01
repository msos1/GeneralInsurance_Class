
##### ------------------Homework6 ------------------ #####

##### --------- Data preparation --------- #####
# as in the lesson...
library(dplyr)
dt_pol_w_claims <- readRDS("C:\\Users\\Matej\\Documents\\github\\GeneralInsurance_Class\\Data\\lesson6_dt_pol_w_claims.rds")
set.seed(47289739)
ind <- sample(2, nrow(dt_pol_w_claims), replace=TRUE, prob=c(0.80, 0.20)) # generate random indicator to split by

dt_pol_w_claims <- mutate(dt_pol_w_claims,
                          data_status = ifelse(ind == 1, 
                                               "Training",
                                               ifelse(ind == 2, 
                                                      "Validation", 
                                                      "Unseen")
                                              )
                          )

train <- dt_pol_w_claims %>% filter(data_status == "Training")
val <- dt_pol_w_claims %>% filter(data_status == "Validation")

mse <- function(prediction, actual){
  return(sum((prediction-actual)^2, na.rm = TRUE)/length(prediction))
}

##### ----------- Models ---------- #####

# First I'll try my model from previous homework

model1 <- glm(data = train,
               formula = Burning_Cost ~ Capacity,
               family = Gamma())
summary(model1)
mse(predict(model1, train, type = "response"), train$Burning_Cost) # 228.1642
mse(predict(model1, val, type = "response"), val$Burning_Cost) # 167.6621

# That doesn't look bad, but let's try to add some more variables. We'll take a closer look to other vehicle parameters.
# VEH_brand brings us a lot of noise and so some grouping of the brands is necessary. However we do not know anything about ste set of numbers representing those brands. Also our intuition is, that it won't be significant for us.
train <- train %>% mutate(Nr_of_seats = ifelse(
  Nr_of_seats < 5, 1, Nr_of_seats))
train <- train %>% mutate(Nr_of_seats = ifelse(
  Nr_of_seats == 5, 5, Nr_of_seats))
train <- train %>% mutate(Nr_of_seats = ifelse(
  Nr_of_seats > 5, 10, Nr_of_seats))
val <- val %>% mutate(Nr_of_seats = ifelse(
  Nr_of_seats < 5, 1, Nr_of_seats))
val <- val %>% mutate(Nr_of_seats = ifelse(
  Nr_of_seats == 5, 5, Nr_of_seats))
val <- val %>% mutate(Nr_of_seats = ifelse(
  Nr_of_seats > 5, 10, Nr_of_seats))
# We split Nr_of_seats into 3 groups
model2 <- glm(data = train,
              formula = Burning_Cost ~ Veh_type2 + Construct_year + Capacity + factor(Nr_of_seats),
              family = Gamma())
summary(model2) # It looks like our model is overfitted, no variable is significant
mse(predict(model2, train, type = "response"), train$Burning_Cost) # 224.3322
mse(predict(model2, val, type = "response"), val$Burning_Cost) # 166.8883

# The results are just slightely better


# From past analysis, it looks like the only significant variables might be Capacity and Veh_type2
train <- train %>% mutate(Veh_type2 = ifelse(as.character(Veh_type2) == 'PICKUP' | as.character(Veh_type2) == 'CAR', 'CAR & PICKUP', as.character(Veh_type2)))
val <- val %>% mutate(Veh_type2 = ifelse(as.character(Veh_type2) == 'PICKUP' | as.character(Veh_type2) == 'CAR', 'CAR & PICKUP', as.character(Veh_type2)))

model3 <- glm(data = train,
              formula = Burning_Cost ~ Veh_type2 + Capacity,
              family = Gamma())
summary(model3)
mse(predict(model3, train, type = "response"), train$Burning_Cost) # 225.6589
mse(predict(model3, val, type = "response"), val$Burning_Cost) # 168.0218

# Very similar number to what we had before. Probably the best (and the easiest) will be just taking Capacity from the car parameters variables.


# In previous homework we looked also at the variable D_age and it didn't seem to be significant, so let's try it now with quadratic D_age.

model4 <- glm(data = train,
              formula = Burning_Cost ~ D_age + I(D_age^2),
              family = Gamma())
summary(model4)
mse(predict(model4, train, type = "response"), train$Burning_Cost) # 228.3873
mse(predict(model4, val, type = "response"), val$Burning_Cost) # 162.1675

# It doesn't seems to be much better.

# Now let's add some more variables to the final model.

model5 <- glm(data = train,
              formula = Burning_Cost ~ D_age + Capacity + Customer_Type,
              family = Gamma())
summary(model5)
mse(predict(model5, train, type = "response"), train$Burning_Cost) # 227.3891
mse(predict(model5, val, type = "response"), val$Burning_Cost) # 161.5951

# This might be the best model I tried.

# Of course, there are lots of other (better) possibilities for modelling the Burning Costs.