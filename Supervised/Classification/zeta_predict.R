library(e1071)
library(dplyr)
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
  
  return(probs)
}


zetaPredict <- function(model, testing){
  outputProbability <- model[["Output"]]
  inputNames <- names(model)[-1]
  notOutputNames <- c(inputNames, "count", "Prob")
  result <- c()
  for (i in 1:nrow(testing)){
    testing_instance <- testing[i,]
    testing_instance <- testing_instance[names(testing_instance) %in% inputNames]
    probList <- list()
    for (j in 1:length(testing_instance)){
      cmd_str <- paste("model[[names(testing_instance[j])]] %>% filter(",names(testing_instance[j]), "=='", testing_instance[[j]], "')", sep="")
      allProb <- eval(parse(text=cmd_str))
      #print(allProb)
      for (h in 1:nrow(allProb)){
        prob <- allProb[h,]
        #print(prob)
        if ((toString(unlist(prob[1])) %in% names(probList))==FALSE){
          probList[toString(unlist(prob[1]))] = c(allProb$Prob[allProb[[names(prob[!(names(prob) %in% notOutputNames)])]]==unlist(prob[1])])
        } else {
          probList[[toString(unlist(prob[1]))]] = c(probList[[toString(unlist(prob[1]))]], allProb$Prob[allProb[[names(prob[!(names(prob) %in% notOutputNames)])]]==unlist(prob[1])])
        }
      }
    }
    maxOutput <- NA
    maxOutputProb <- -1
    for (name in names(probList)){
      probList[[name]] = c(probList[[name]], outputProbability$Prob[outputProbability[[names(outputProbability[!(names(outputProbability) %in% notOutputNames)])]]==name])
      if (prod(probList[[name]])>maxOutputProb){
        maxOutput <- name
        maxOutputProb <- prod(probList[[name]])
      }
    }
    result <- c(result, maxOutput)
  }
  return(result)
}

#Main
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
model <- zetaNaiveBayes(c("Hair", "Race"), "Gender", people)
count <- 1:1000
ind <- sample(count, 800, replace=FALSE)
training <- people[ind,]
testing <- people[-ind,]