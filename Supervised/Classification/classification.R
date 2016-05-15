#sample(x, size, replace = False, prob = NULL)
#probability = c("Bronze", "Bronze", "Bronze", "Black", "Red", "Red", "Brown", "Brown", "Brown", "Brown")
#samples = sample(probability, 100, replace=TRUE)
#samples
library(e1071)
library(dplyr)
peopleCount <- 500
hairColor <- sapply(list("Bronze", "Black", "Red", "Brown"), length)
race <- sapply(list("Asian", "European", "Latino"), length)
maleHair <- sample(c("Bronze", "Black", "Red", "Brown"),
                 size = peopleCount,
                 prob = rep(c(0.3, 0.1, 0.2, 0.4) / hairColor, hairColor),
                 replace = TRUE)
maleRace <- sample(c("Asian", "European", "Latino"),
                  size = peopleCount,
                  prob = rep(c(0.3, 0.6, 0.1) / race, race),
                  replace = TRUE)
male <- data.frame(Hair=maleHair, Race=maleRace, Gender="M")
femaleHair <- sample(c("Bronze", "Black", "Red", "Brown"),
                    size = peopleCount,
                    prob = rep(c(0.05, 0.5, 0.3, 0.15) / hairColor, hairColor),
                    replace = TRUE)
femaleRace <- sample(c("Asian", "European", "Latino"),
                    size = peopleCount,
                    prob = rep(c(0.2, 0.1, 0.7) / race, race),
                    replace = TRUE)
female <- data.frame(Hair=femaleHair, Race=femaleRace, Gender="F")
people <- rbind(male, female)
model <- naiveBayes(Gender ~ ., data=people)
filter(people, Hair=="Black", Gender=="M")