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

getProb <- function(gender, probdf){
  result <- probdf[probdf$Gender==gender,]$Prob
}

zetaNaiveBayes <- function(input, output, data){
  probs <- list()
  cmd_str  <- paste("data %>% group_by(", output, ") %>% summarise(count=n())", sep="")
  outputdf <- eval(parse(text=cmd_str))
  outputdf$Prob=outputdf$count/nrow(data)
  probs[["Output"]] = outputdf
  v_getProb <- Vectorize(getProb, vectorize.args="gender")
  
  for (in_var in input){
    cmd_str  <- paste("data %>% group_by(", output, ",", in_var, ") %>% summarise(count=n())", sep="")
    inputdf <- eval(parse(text=cmd_str))
    inputdf$Prob=inputdf$count/(nrow(data)*v_getProb(inputdf$Gender, outputdf))
    probs[[in_var]] = inputdf
  }
  #groupdf <- people %>% group_by(Gender, Hair, Race) %>% summarise(count=n())
  #groupdf$Prob=groupdf$count/500
}