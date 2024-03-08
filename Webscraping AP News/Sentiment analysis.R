
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
library(reticulate)
library(tidyverse)
library(udpipe)
library(ggwordcloud)
library(rvest)
library(xml2)
library(tidytext)
library(SnowballC)
library(quanteda)
library(text2vec)
library(flextable)
library(LDAvis)
##################################Netanyahu Sentence Analysis##########################################
#Tokenize sentences and create a new column that gives them an ID
text <- read.csv('israelframe.csv')

# extract only articles that mention Important entities

netanyahutest<-text%>%
  filter(str_detect(bodytext, '\\bNetanyahu\\b'))

tokens<-netanyahutest%>%
  unnest_tokens(input = bodytext, token='sentences', output='sentence')%>%
  mutate(sentence_id = seq(nrow(.)),
         netanyahu = str_detect(sentence, 'netanyahu')
         )%>%
  unnest_tokens(input = sentence, token ='words', output = 'word')


#Join sentence IDs to the sentiments dictionary
netanyahu_tokens<-tokens%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentence_id, netanyahu)%>%
  summarise(n = n(),
            #make positive and negative sentiments and get average, 
            #more normally dist with log
            positive= sum(sentiment == 'positive'),
            negative = sum(sentiment =='negative'),
            average_tone_is= (positive-negative)/(positive + negative),
            logit_tone_is = log(positive+0.5)/log(negative +0.5)
  )





########################################Putin Sentence Anlysis########################################################
#Tokenize sentences and create a new column that gives them an ID
text <- read.csv('ukraineframe.csv')

# extract only articles that mention Important entities

putintest<-text%>%
  filter(str_detect(bodytext, '\\bPutin\\b'))

tokens<-putintest%>%
  unnest_tokens(input = bodytext, token='sentences', output='sentence')%>%
  mutate(sentence_id = seq(nrow(.)),
         putin = str_detect(sentence, 'putin')
  )%>%
  unnest_tokens(input = sentence, token ='words', output = 'word')


#Join sentence IDs to the sentiments dictionary
putin_tokens<-tokens%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentence_id, putin)%>%
  summarise(n = n(),
            #make positive and negative sentiments and get average, 
            #more normally dist with log
            positive= sum(sentiment == 'positive'),
            negative = sum(sentiment =='negative'),
            average_tone_uk= (positive-negative)/(positive + negative),
            logit_tone_uk = log(positive+0.5)/log(negative +0.5)
  )

#Make a linear model

putinlm <- lm(logit_tone_uk ~ putin, data=putin_tokens)

ggplot(putin_tokens, aes(x = factor(putin), y = logit_tone_uk, fill = factor(putin))) +
  geom_boxplot(position = "dodge", )+labs(title = "Putin Sentiment",
                                          x = "Is Putin in the Sentence",
                                          y = "Average Sentiment") +
  scale_x_discrete(labels = c("True" = "Group True", "False" = "Group False"))

########################################Biden Sentiment in Ukraine#####################################################
#Tokenize sentences and create a new column that gives them an ID
text <- read.csv('ukraineframe.csv')

# extract only articles that mention Important entities

biden_uk_test<-text%>%
  filter(str_detect(bodytext, '\\bBiden\\b'))

tokens<-biden_uk_test%>%
  unnest_tokens(input = bodytext, token='sentences', output='sentence')%>%
  mutate(sentence_id = seq(nrow(.)),
         biden = str_detect(sentence, 'biden')
  )%>%
  unnest_tokens(input = sentence, token ='words', output = 'word')



#Join sentence IDs to the sentiments dictionary

biden_uk_tokens<-tokens%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentence_id, biden)%>%
  summarise(n = n(),
            #make positive and negative sentiments and get average, 
            #more normally dist with log
            positive= sum(sentiment == 'positive'),
            negative = sum(sentiment =='negative'),
            average_tone_buk= (positive-negative)/(positive + negative),
            logit_tone_buk = log(positive+0.5)/log(negative +0.5)
  )


########################################Biden Sentiment in Israel#################################
text <- read.csv('israelframe.csv')

biden_is_test<-text%>%
  filter(str_detect(bodytext, '\\bBiden\\b'))

tokens<-biden_is_test%>%
  unnest_tokens(input = bodytext, token='sentences', output='sentence')%>%
  mutate(sentence_id = seq(nrow(.)),
         biden = str_detect(sentence, 'biden')
  )%>%
  unnest_tokens(input = sentence, token ='words', output = 'word')


#Join sentence IDs to the sentiments dictionary
biden_is_tokens<-tokens%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentence_id, biden)%>%
  summarise(n = n(),
            #make positive and negative sentiments and get average, 
            #more normally dist with log
            positive= sum(sentiment == 'positive'),
            negative = sum(sentiment =='negative'),
            average_tone_bis= (positive-negative)/(positive + negative),
            logit_tone_bis = log(positive+0.5)/log(negative +0.5)
  )



#########################################Boxplots and Transformations for Israel#####################
combinedbidennet <-rbind(biden_is_tokens, netanyahu_tokens) 

combinedbidennet <- combinedbidennet%>%
  mutate(leader_biden= biden==T&!is.na(biden), 
         leader_net= netanyahu==T&!is.na(netanyahu),
         leader=ifelse(leader_biden==T, "Biden",ifelse(leader_net==T, "Netanyahu", "Neither" )))

  
combinedbidennet$logit_tone_combined <-rowSums(combinedbidennet[, c("logit_tone_bis", "logit_tone_is")], na.rm = T)
  
  
  plot_z <- ggplot(combinedbidennet)+geom_boxplot(aes(x=leader, y=logit_tone_combined), fill='lightblue', color="black")+labs(x="Leader Mentioned in Sentence"
                                                   ,y="Average Tone of Sentences mentioning the Leader",
                                                    title="Sentences with Biden and Netanyahu Sentiments Compared",
                                                   )
  plot_z
  
#regression table

summary(lm(logit_tone_combined~leader, combinedbidennet))

###########################################Boxplots and Transformations for Ukraine##################
combinedbidenput <-rbind(biden_uk_tokens, putin_tokens) 

combinedbidenput<- combinedbidenput%>%
  mutate(leader_biden= biden==T&!is.na(biden), 
         leader_put= putin==T&!is.na(putin),
         leader=ifelse(leader_biden==T, "Biden",ifelse(leader_put==T, "Putin", "Neither" )))
combinedbidenput$logit_tone_combined <-rowSums(combinedbidenput[, c("logit_tone_buk", "logit_tone_uk")], na.rm = T)


#linear model
summary(lm(logit_tone_combined~leader, combinedbidenput))

ggplot(combinedbidenput)+geom_boxplot(aes(x=leader, y=logit_tone_combined), fill='pink', color="black")+labs(x="Leader Mentioned in Sentence"
                                                                                                                  ,y="Average Tone of Sentences mentioning the Leader",
                                                                                                                  title="Sentences with Biden and Putin Sentiments Compared")
