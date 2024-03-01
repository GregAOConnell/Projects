###320 final project
library(poliscidata)
library(tidyverse)
dataset <- read.csv("Final project dataset ct.csv")

##Lets make our dummies, first Trump Vote
dataset$trumpvote <- as.numeric(dataset$Q19=="Donald Trump")
freqC(dataset$trumpvote)
freqC(dataset$Q19)

##now the broken institutions vars Q65
dataset$broke <- dataset$Q65
dataset <- dataset %>%
  mutate(broke = case_when(
    Q65 %in% c("Strongly agree", "Somewhat agree") ~ "agree",
    Q65 %in% c("Neither agree nor disagree", "Strongly disagree", "Somewhat disagree") ~ "disagree",
    TRUE ~ NA_character_
  ))
levels(dataset$broke)=c("agree", "disagree")
levels(dataset$broke)
describe(dataset$broke)

##party id Q57
dataset <- dataset %>%
  mutate(partyid = case_when(
    Q57 %in% c("Democrat", "Independent, but closer to Democrat") ~ "democrat",
    Q57 %in% c("Republican", "Independent", "I'm not really a political person", "Independent, but closer to Republican") ~ "notdemocrat",
    TRUE ~ NA_character_
  ))
levels(dataset$partyid)=c("democrat", "notdemocrat")

describe(dataset$partyid)

##Education Level Q35
freqC(dataset$Q35)

dataset <- dataset %>%
  mutate(college = case_when(
    Q35 %in% c("Associate degree in college (2-years)", "Bachelor's degree in college (4-years)", 
               "Doctoral degree", "Master's degree", "Professional degree (JD, MD)") ~ "college",
    Q35 %in% c("High school graduate (high school diploma or equivalent including GED)", "Less than high school degree", "Some college but no degree") ~ "nocollege",
    TRUE ~ NA_character_
  ))

levels(dataset$college)=c("college", "nocollege")
describe(dataset$college)

  #design dataset
datasetD <- svydesign(id=~1, data=dataset)

#logit model
trump.logit <- svyglm(trumpvote~broke+partyid+college, design=datasetD, family="quasibinomial")
summary(trump.logit)

##prediction frames, dem and non dem
levels(dataset$broke)
prediction.frame.1 = expand.grid(broke = levels(dataset$broke),
                                 partyid = "democrat",
                                 college = levels(dataset$college))
prediction.frame.1
prediction.frame.2 = expand.grid(broke = levels(dataset$broke),
                                 partyid = "notdemocrat",
                                 college = levels(dataset$college))
prediction.frame.2

#you would have two predictions & two prediction frame
?predict.glm
predictions.1 = predict.glm(trump.logit, newdata = prediction.frame.1,
                            type = "response", se.fit = T,
                            interval = "confidence", level = .95)
predictions.2 = predict.glm(trump.logit, newdata = prediction.frame.2,
                            type = "response", se.fit = T,
                            interval = "confidence", level = .95)

predictions.1$fit
predictions.2
predictions.1


#plot of predicted prob
plot(1:4, y = predictions.1$fit, ylim = c(0,1), axes = F,
     xlab = "View of Institutions in the U.S. and Level of Education",
     ylab = "Probability of Trump Vote",
     main =
       "Figure 3:View of Insitituions and Education Level Predicting \n the Probability for a Trump Vote",
     pch =16,
     col = c("lightblue", "lightblue", "lightblue", "lightblue"))
points(1:4, y = predictions.2$fit, pch =16,
       col = c("palevioletred", "palevioletred",
               "palevioletred", "palevioletred"))
segments(x0=1:4 , y0=predictions.1$fit-1.96*predictions.1$se.fit ,
         x1=1:4 , y1=predictions.1$fit+1.96*predictions.1$se.fit)
segments(x0=1:4 , y0=predictions.2$fit-1.96*predictions.2$se.fit ,
         x1=1:4 , y1=predictions.2$fit+1.96*predictions.2$se.fit)
axis(side=1, at=1:4, labels=c("College Educated \n Institutions Broken", 
                              "College Educated \n Institutions Not Broken", 
                              "Not College Educated \n Institutions  Broken", 
                              "Not College Educated \n Institutions Not Broken"))
axis(side=2)
legend("topright", legend = c("Democrat", "Republican"),
      col = c("lightblue", "palevioletred"), pch = 16, title = "Party")







#pie charts
 pie_old <- data.frame(response = c("Neither agree nor disagree ", "Somewhat agree", 
                                        "Strongly agree", "Somewhat disagree", "Strongly disagree"),
                          proportion = c(.196, 0.349, .387, .048, .02))

  pie_chart_old <- ggplot(pie_old, aes(x = "", y = proportion, fill = response)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    theme_minimal() +
    labs(title = "Figure1: Orginial proprtions of view of institutions in the U.S.") +
    geom_text(aes(label = paste0(round(proportion * 100, 1), "%\n(", round(proportion, 3), ")")),
              position = position_stack(vjust = 0.5)) +
    theme(legend.position = "bottom")  # Optional: Move legend to the bottom
  pie_chart_old
  
  #new pie chart
  
  pie_new <- data.frame(response = c("Agree", "Disagree"),
                     proportion = c(0.735, 0.265))
  
  # Create a pie chart
  pie_chart_new <- ggplot(pie_new, aes(x = "", y = proportion, fill = response)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    theme_minimal() +
    labs(title = "Figure 2:Transformed proprtions of view of insitutions in the U.S.") +
    geom_text(aes(label = paste0(round(proportion * 100, 1), "%\n(", round(proportion, 3), ")")),
              position = position_stack(vjust = 0.5)) +
    theme(legend.position = "bottom")  # Optional: Move legend to the bottom

  