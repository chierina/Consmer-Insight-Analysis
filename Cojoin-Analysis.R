# Read in file
library(readxl)
library(radiant)
install.packages("conjoint")

#Indicate the file to be used
Merch_group_2 <- read_excel("Merch_group 2.xlsx")
View(Merch_group_2)

#There's no missing data here, but if there is, please handle it first.

#Conjoint analysis data needs to be oragnised in a certain way.
#Normally we have one row per respondent
#Remember to factorize variables
Merch_group_2 <- Merch_group_2 %>%
  gather(respondent, rating, starts_with("Respondent")) %>%  # respondent keeps track of the respondent, rating will store the respondent's ratings, and we want to stack every variable that starts with Individual
  mutate(Profile = factor(Profile), respondent = factor(respondent),  
         Price = factor(Price), Material = factor(Material), Straw = factor(Straw), Size = factor(Size))

# Conjoint analysis with 1 respondent -> Individual 1
respondent2 <- Merch_group_2 %>% filter(respondent == "Respondent 2")

respondent13 <- Merch_group_2 %>% filter(respondent == "Respondent 13")

# Perform conjoint analysis - dependent variable, 
conjoint_respondent2 <- conjoint(respondent2, rvar = "rating", evar = c("Price", "Material", "Straw", "Size"))
summary(conjoint_respondent2)

conjoint_respondent13 <- conjoint(respondent13, rvar = "rating", evar = c("Price", "Material", "Straw", "Size"))
summary(conjoint_respondent13)

#Plot the results
plot(conjoint_respondent2)
plot(conjoint_respondent13)

# Run this regression if you're interested in learning which predictor is significant or what the R-squared of the overall model is.
# We tend not to consider this because it's expected to be non-significant
summary(lm(rating ~ Price + Material + Straw + Size, data = respondent2))

# Predicting preference
# first, you need to establish how many profiles are tested
profiles.2 <- Merch_group_2 %>% 
  filter(respondent == "Respondent 2") %>% 
  select(Price,Material,Straw,Size)

profiles.2

profiles.13 <- Merch_group_2 %>% 
  filter(respondent == "Respondent 13") %>% 
  select(Price,Material,Straw,Size)

profiles.13

#Now you're ready to predict. You are predicting the ratings for the profiles based on the conjoint analysis result of that one individual
#Highest rating is highest prediction
predict(conjoint_respondent2, profiles.2) %>% 
arrange(desc(Prediction))

predict(conjoint_respondent13, profiles.13) %>% 
  arrange(desc(Prediction))

#This prediction is based on what this person had answered, which is not so interesting
# We can also predict the unknown profiles

#This is to see all the possible combinations
#First, you list out all the possible combinations
Price <- c("625","425","225")
Crust <- c("Regular","Medium","Large")
Toppings <- c("crisp Capsicum","Baby Corn","Lpaneer","Fresh Tomato")
Organic <- c("Organic","Not organic")

expand.grid(Price,Material,Straw,Size)

# Sometimes, there are a lot of attributes and a lot of levels.
# There is an easier way to get attribute levels than creating the vectors manually:
# Make sure all the attributes are factorized.
# Remember to rename the variables created by expand.grid, so the predict function can read the data

profiles.all <- expand.grid(levels(Merch_group_2$Price),levels(Merch_group_2$Material),levels(Merch_group_2$Straw),levels(Merch_group_2$Size)) %>% 
  rename("Price" = "Var1", "Material" = "Var2", "Straw" = "Var3", "Size" = "Var4") %>%
  arrange(desc(Price))
profiles.all

# predict the ratings of all profiles
predict(conjoint_respondent2, profiles.all) %>% 
  arrange(desc(Prediction)) # show the pizzas with the highest predicted rating on top

#Let's do the conjoint for all the respondents
# same as before, but the whole dataset
conjoint_allrespondents <- conjoint(Merch_group_2, rvar = "rating", evar = c("Price", "Material", "Straw", "Size")) 

summary(conjoint_allrespondents) 

#Plot it
plot(conjoint_allrespondents)

# Predict ratings for all possible combinations based on all participants
predict(conjoint_allrespondents, profiles.all) %>% 
  arrange(desc(Prediction)) # show the pizzas with the highest predicted rating on top

#Let's predict the market share for different options
#You can use the formula shown in the slides. Or you can see proportionally, how many respondents preferred a certain option.
# use slice() to select rows
market_profiles <- profiles.all %>% 
  slice(c(38, 7, 15)) # from profiles.all, select rows 3, 21, 45, 71 as the four profiles. I choose this randomly here. You should choose the combination that interests you.

market_profiles

# We already know how to predict the ratings
predict(conjoint_allrespondents, market_profiles) %>%
  arrange(desc(Prediction))

#This tell us the overall rating but not the marketshare. To know the share, you need to know how every single respondents would react
# same model as before, but now add by = "respondent"
conjoint_perrespondent <- conjoint(Merch_group_2, rvar = "rating", evar = c("Price", "Material", "Straw", "Size"), by = "respondent")
conjoint_perrespondent

predict(conjoint_perrespondent, market_profiles) %>% 
  arrange(respondent, desc(Prediction)) # sort by respondent and then by predicted rating

summary(conjoint_perrespondent)

#Retain for each individual only his or her highest rated profile
highest_rated <- predict(conjoint_perrespondent, market_profiles) %>% 
  group_by(respondent) %>% 
  mutate(ranking = rank(Prediction))
# have a look
highest_rated %>% 
  arrange(respondent, ranking)
# we need to retain only the highest ranked pizza
highest_rated <- highest_rated %>% 
  arrange(respondent, ranking) %>% 
  filter(ranking == 3)

highest_rated

#Now you're finally ready to estimate the market share
market_share <- highest_rated %>% 
  group_by(Price, Material, Straw, Size) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

market_share #you can see 425, regular, crisp Capsicum Organic, is prefered by 16 out of 22 respondents
