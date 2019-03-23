library(dplyr)
library(stringr)
library(ggplot2)
setwd("~/Desktop/tcd_study/Semester_2/Text_Analytics/Pratical/Data")
reviews <- read.csv('imdb_master.csv', header = T, stringsAsFactors = F)
reviews %>% group_by(label) %>% count() %>% arrange(desc(n))
reviews %>% group_by(label) %>% count()  %>% ggplot(aes(x = '', y = n, fill = label)) + 
  geom_bar(width = 1, stat ='identity')+
  labs(title = "IMDB Review Dataset",
       x = 'label', y = "number of observations") +
  theme(legend.position = "right",
      panel.background = element_rect(fill = 'azure2'),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()) +
  coord_polar("y", start=0)

###         ####
review <- reviews$review[reviews$label != 'unsup']
label <- reviews$label[reviews$label != 'unsup']
#number of question mark
question_mark <- str_count(review, '\\?')
#number of exclamation mark
exclamation_mark <- str_count(review, '!')
#number of sentence
sentence_length <- c()
i = 1
for (i in 1:50000){
  sentence_length <- c(sentence_length,length(gregexpr('[[:alnum:] ][.!?]', reviews$review[i])[[1]]))
}
#dataframe
df <- data.frame(review, question_mark, exclamation_mark, sentence_length, label)

#topcode function removing outliers
topcode<- function(a,top){
  return(ifelse(a>top,top,a))
}
q_top <- topcode(question_mark, 20)
e_top <- topcode(exclamation_mark, 30)
s_top <- topcode(sentence_length, 100)
df_topcode <- data.frame(q_top, e_top, s_top, label)


ggplot(df_topcode, aes(q_top)) + 
  geom_density(aes(group = label, colour = label, fill = label), alpha=0.3, adjust = 4) +
  labs(title = "Distribution of Question Marks",
       x = 'Numbers of Question Marks', y = "Density") +
       theme(plot.title = element_text(hjust = 0.5)) +
       geom_vline(aes(xintercept = mean(q_top)),
             color = "blue", linetype = "dashed") 
#+scale_x_continuous(limits = c(0, 20), breaks = c(seq(0, 20, by = 5), round(mean(q_top),3)))
ggplot(df_topcode, aes(e_top)) + 
  geom_density(aes(group = label, colour = label, fill = label), alpha = 0.2, adjust = 4) +
  scale_fill_manual(values=c("#F8766D", "#56B4E9" ))+
  scale_color_manual(values=c("#F8766D", "#56B4E9" ))+
  labs(title = "Distribution of Exclamation Marks",
       x = 'Numbers of Exclamation Marks', y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept = mean(e_top)),
             color = "blue", linetype = "dashed") 
ggplot(df_topcode, aes(s_top)) + 
  geom_density(aes(group = label, colour = label, fill = label), alpha = 0.3) +
  labs(title = "Distribution of Sentences",
       x = 'Numbers of Sentence', y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept = mean(s_top)),
             color = "blue", linetype = "dashed") 

## remove outliers by boxplot
boxplot(df$question_mark)
outliers_q <- df$question_mark[df$question_mark > 25]
outliers_e <- df$exclamation_mark[df$exclamation_mark > 58]
outliers_s <- df$sentence_length[df$sentence_length > 88]
# drop 50 outliers
df <- df[-which((df$question_mark %in% outliers_q)|(df$exclamation_mark %in% outliers_e)|(df$sentence_length %in% outliers_s)),-1]
write.csv(df, file = "./data_punctuation.csv")
