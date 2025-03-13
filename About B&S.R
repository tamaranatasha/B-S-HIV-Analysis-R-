#WB & BS LIKERT SCALES - ORDERED FACTORS
library(coin)
library(tidyverse)
library(likert)
library(RColorBrewer)
library(ordinal)
#load data33
BS_HIV_dta <- read_csv('../Data Analysis/Planning for the Future 2024 Data.csv'
)
BS_HIV_dta 
colnames(BS_HIV_dta)

#BODY AND SOUL SCALE AND ABOUT BODY AND SOUL 
BS_Scale <- BS_HIV_dta |> 
  select(`What B&S means to you`: 
           `Has B&S helped you to cope and build your resilience`:
           `Has B&S helped you to cope and build your resilience`,
         `Borough`,
         `Gender`,
  )
BS_Scale

#create levels
answer_levelBS <- c("Yes",
                    "Sometimes",
                    "No"
)

#Recode variables to ordered factors BS Scale 
colnames(BS_Scale)
BS_Scale$`Has B&S helped you to feel less isolated or lonely` <- factor(BS_Scale$`Has B&S helped you to feel less isolated or lonely`,
                                                                        levels = answer_levelBS)

BS_Scale$`Has B&S helped you to feel less anxious or stressed` <- factor(BS_Scale$`Has B&S helped you to feel less anxious or stressed`,
                                                                         levels = answer_levelBS)

BS_Scale$`Has B&S helped you to learn new skills or ways to understand and regulate your emotions` <- factor(BS_Scale$`Has B&S helped you to learn new skills or ways to understand and regulate your emotions`,
                                                                                                             levels = answer_levelBS)

BS_Scale$`Has B&S helped you to support your physical health during this time` <- factor(BS_Scale$`Has B&S helped you to support your physical health during this time`,
                                                                                         levels = answer_levelBS)

BS_Scale$`Has accessing B&S helped you to find moments of connection and happiness` <- factor(BS_Scale$`Has accessing B&S helped you to find moments of connection and happiness`,
                                                                                              levels = answer_levelBS)

BS_Scale$`Has B&S helped you to feel more confident and capable` <- factor(BS_Scale$`Has B&S helped you to feel more confident and capable`, 
                                                                           levels = answer_levelBS)


BS_Scale$`Has your contact with B&S helped you to talk about your feelings more` <- factor(BS_Scale$`Has your contact with B&S helped you to talk about your feelings more`,
                                                                                           levels = answer_levelBS)

BS_Scale$`Has the support you received from B&S helped you to improve your sense of self-worth` <- factor(BS_Scale$`Has the support you received from B&S helped you to improve your sense of self-worth`,
                                                                                                          levels = answer_levelBS)

BS_Scale$`Has B&S helped you to cope and build your resilience` <- factor(BS_Scale$`Has B&S helped you to cope and build your resilience`,
                                                                          levels = answer_levelBS)

view(BS_Scale)

#BS_Scale - counts percentages
colnames(BS_Scale)
BS_Scale  |> 
  count(`Has B&S helped you to feel less isolated or lonely`) |> 
  mutate(percentage = n/sum(n)*100) |> 
  filter(`Has B&S helped you to feel less isolated or lonely`!= "NA")

BS_Scale  |> 
  count(`Has B&S helped you to feel less anxious or stressed`) |> 
  mutate(percentage = n/sum(n)*100) |> 
  filter(`Has B&S helped you to feel less anxious or stressed`!= "NA")

BS_Scale  |> 
  count(`Has B&S helped you to feel less anxious or stressed`,
        `Has B&S helped you to feel less isolated or lonely`) |> 
  mutate(percentage = n/sum(n)*100)

#convert to data rame - likert only
colnames(BS_Scale)
BS_Scale_dtf <- as.data.frame(BS_Scale) |> 
  select(`Has B&S helped you to feel less isolated or lonely`:
           `Has B&S helped you to cope and build your resilience`)
data.frame(BS_Scale_dtf)
#likert scales 
plot(likert(BS_Scale_dtf[,1:9]))
colnames(BS_Scale_dtf) 

#cuter scales
#order and color the likert 
BS_likert <- plot(likert(BS_Scale_dtf[,1:9]), ordered = F)+
  labs(title = "Body and Soul Scale") +
  scale_fill_brewer(palette = "YlOrBr")


BS_likert
colnames(BS_Scale_dtf)

#save
ggsave(filename = "BS-likert.png", plot = BS_likert,
       height = 4,
       width = 9)

#what B&S means to you
BS_Scale$`What B&S means to you`

BS_mean_to_you <- BS_Scale |> 
  select(`What B&S means to you`) |> 
  unnest_tokens(word, `What B&S means to you`)|> 
  anti_join(stop_words)

BS_mean_to_you |> 
  count(word, sort = TRUE) |> 
  print(n = 144)

mean_to_you_word_steM_percent <- BS_mean_to_you |> 
  filter(word != "NA") |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  print(n = 25)

#128 words in tottal after stemming - graph shows top 25 - occurrences > 2
bs_meanto_you_plot  <- mean_to_you_word_steM_percent |>
  filter(n > 2,
         stem != "alot",
         stem != "feel",
         stem != "time")  |> 
  ggplot(aes(n, fct_reorder(stem, n))) +
  geom_col(show.legend = FALSE, fill = "darkgoldenrod2", alpha = 3/4) +
  labs(title = "What does Body&Soul mean to you?",
       y = "occurence",
       x = "word stem") +
  theme_gray(base_size = 8)+
  theme(
    plot.title = element_text(size = 9))

bs_meanto_you_plot
mean_to_you_word_steM_percent

ggsave(filename = "bs_mean_to_you.png", plot = bs_meanto_you_plot,
       height = 4,
       width = 8)

mean_to_you <- mean_to_you_word_steM_percent |>
  filter(n > 2,
         stem != "alot",
         stem != "feel",
         stem != "time") |> 
  print(n = 22)

write.csv(mean_to_you, "mean_to_you.csv")

#What services have you found most helpful
BS_Scale$`What services have you found most helpful`

services_helpful <- BS_Scale |> 
  select(`What services have you found most helpful`) |> 
  unnest_tokens(word, `What services have you found most helpful`)|> 
  anti_join(stop_words)

services_helpful |> 
  count(word, sort = TRUE)

services_helpful_2 <- services_helpful |> 
 mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE) |> 
  filter(n > 2) |> 
  mutate(percent = n/sum(n)*100)

write.csv(services_helpful_2, "services_helpful_2.csv")

# occurrence of word stem is > 2
services_helpful_plot <- services_helpful |> 
  filter(word != "NA") |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |>
  filter(n > 2) |> 
  ggplot(aes(stem, n)) +
  geom_col(show.legend = FALSE, fill = "darkorange2", alpha = 3/4) +
  labs(title = "What services have you found the most helpful?",
       y = "occurence",
       x = "word stem") +
  theme_gray(base_size = 8)+
  theme(
    plot.title = element_text(size = 9)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(filename = "services_helpful_plot.png", plot = services_helpful_plot,
       height = 4,
       width = 8)

services_helpful_plot

#Something B&S could do better
sop_words_filtered <- stop_words |> 
  filter(word != "nothing",
         word != "no",
         word != "none",
         word != "yes")

BS_Scale$`Something B&S could do better`


BS_Scale <- BS_Scale |> 
  mutate(`Something B&S could do better` = str_to_lower(`Something B&S could do better`),
        `Something B&S could do better` = str_replace_all(`Something B&S could do better`, "nothing", "no"),
        `Something B&S could do better` = str_replace_all(`Something B&S could do better`, "none", "no"))

better_BS <- BS_Scale |> 
  select(`Something B&S could do better`) |> 
  unnest_tokens(word, `Something B&S could do better`)|> 
  anti_join(sop_words_filtered)
better_BS

better_BS |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, word, sort = TRUE) |> 
  filter(n > 2) |> 
  mutate(percent = n/sum(n)*100)

better_BS_plot <- better_BS |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  filter(n > 2,
         word != "NA",
         word != "provide") |> 
  ggplot(aes(word, n)) +
  geom_col(show.legend = FALSE, fill = "orangered3", alpha = 3/4) +
  labs(title = "Something B&S could do better?",
       y = "occurence",
       x = "word") +
  theme_gray(base_size = 8)+
  theme(
    plot.title = element_text(size = 9)) 

better_BS_plot

better_BS_percent <- better_BS |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  filter(n > 2,
         word != "NA",
         word != "provide")

write.csv(better_BS_percent, "better_BS_percent.csv")

#A way you would like to get more involved with B&S
BS_Scale$`A way you would like to get more involved with B&S`

getting_involvd <- BS_Scale |> 
  select(`A way you would like to get more involved with B&S`) |> 
  unnest_tokens(word, `A way you would like to get more involved with B&S`) |> 
 anti_join(sop_words_filtered)

getting_involvd |> 
  count(word, sort = TRUE) |> 
  print(n = 105)


getting_involvd |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  filter(n > 1) 


getting_involvd_plot <- getting_involvd |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  filter(n > 1,
         stem != "due",
         stem != "come",
         stem != "week",
         stem != "time") |> 
  ggplot(aes(stem, n)) +
  geom_col(show.legend = FALSE, fill = "lightgoldenrod2", alpha = 3/4) +
  labs(title = "A way you would like to get more involved with B&S?",
       y = "occurence",
       x = "stem") +
  theme_gray(base_size = 8)+
  theme(
    plot.title = element_text(size = 9)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggsave(filename = "getting_involvd_plot.png", plot = getting_involvd_plot,
       height = 4,
       width = 8)

getting_involvd_plot

getting_involvd_percent <- getting_involvd|> 
  mutate(stem = wordStem(word)) %>%
  count(stem, word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  filter(n > 1,
         stem != "due",
         stem != "come",
         stem != "week",
         stem != "time")
getting_involvd_percent
write.csv(getting_involvd_percent, "getting_involvd_percent.csv")

#Subjects/activities you would like to see in workshops
BS_Scale$`Subjects/activities you would like to see in workshops`

workshops <- BS_Scale |> 
  select(`Subjects/activities you would like to see in workshops`) |> 
  unnest_tokens(word, `Subjects/activities you would like to see in workshops`) |> 
  anti_join(stop_words_with_no)

workshops |> 
  count(word, sort = TRUE) 

workshops |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE) |>
  mutate(percent = n/sum(n)*100) |> 
  filter(n > 1) |> 
  print(n  = 28)


workshops_plot <- workshops |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |>
  filter(n > 1,
         stem != "men'",
         stem != "workshop",
         stem != "out",
         stem != "medic") |> 
  ggplot(aes(stem, n)) +
  geom_col(show.legend = FALSE, fill = "darkorange2", alpha = 3/4) +
  labs(title = "Subjects/activities you would like to see in workshops",
       y = "occurence",
       x = "stem") +
  theme_gray(base_size = 8)+
  theme(
    plot.title = element_text(size = 9)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

workshops_percent <- workshops |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |>
  filter(n > 1,
         stem != "men'",
         stem != "workshop",
         stem != "out",
         stem != "medic")
ggsave(filename = "workshops_plot.png", plot = workshops_plot,
       height = 4,
       width = 8)
workshops_percent
write.csv(workshops_percent, "workshops_percent.csv")

#plots
bs_meanto_you_plot
services_helpful_plot
better_BS_plot
getting_involvd_plot
workshops_plot
BS_likert


