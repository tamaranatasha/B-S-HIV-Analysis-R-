#LOAD LIBS
library(coin)
library(tidyverse)
library(likert)
library(RColorBrewer)
library(ordinal)
library(SnowballC)
library(hunspell)
library(tidytext)
library(tokenizers)
library(tidyr)
library(scales)

#load data
BS_HIV_dta <- read_csv('../Data Analysis/Planning for the Future 2024 Data.csv')

#SUBSET - CAMDEN & ISLINGTON
camden_islington <- BS_HIV_dta |> 
  filter(Borough == "Camden and Islington")

#ABOUT RELATIONSHIPS
#`Who or where do you turn to for support`
camden_islington$`Who or where do you turn to for support`

camden_islington_parsed <- camden_islington |> 
  mutate(`Who or where do you turn to for support`= str_to_lower(`Who or where do you turn to for support`),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`,"b&s", "body_soul"),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`, "care worker", "care_worker"),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`, "friends", "friend"))
         
camden_islington_parsed$`Who or where do you turn to for support`

c_i_token <- camden_islington_parsed |> 
  select(`Who or where do you turn to for support`) |> 
  unnest_tokens(word, `Who or where do you turn to for support`) %>%
  mutate(word = case_when(word == "body_soul" ~ "body & soul",
                          word == "care_worker" ~ "care worker",
                          TRUE ~ word))|> 
  anti_join(stop_words)
c_i_token |> 
  count(word, sort = TRUE)

c_i_token |> 
  count(word, sort = TRUE)|>
  mutate(percent = n/sum(n)*100)

c_i_tokenplot <- c_i_token |> 
  filter(
    word != "related",
    word != "soul",
    word != "body",
    word != "met",
    word != "related",
    word != "hiv") |> 
  count(word, sort = TRUE)|>
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.8,
           width = 0.8) +
  labs(y = NULL, title = "Who or where do you turn to for support?", 
       x = "%") +
  theme_gray(base_size = 8)+
  theme(
    plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "RdPu")
c_i_tokenplot

c_i_token_percent <- c_i_token|> 
  filter(
    word != "related",
    word != "soul",
    word != "body",
    word != "met",
    word != "related",
    word != "hiv") |> 
  count(word, sort = TRUE)|>
  mutate(percent = n/sum(n)*100) 
  
c_i_token_percent

ggsave(filename = "c_i_tokenplot.png", plot = c_i_tokenplot,
       height = 4,
       width = 8) 

write.csv(c_i_token_percent, "support_ci.csv")

#Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`
camden_islington$`Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`

camden_islington <- camden_islington |> 
  rename(outside_support = `Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`)

camden_islington$outside_support

support_oustide_bs <- camden_islington %>%
  mutate(outside_support = str_replace_all(outside_support, "Family members", "yes"),
         outside_support = str_replace_all(outside_support, "not really", "no"),
         outside_support = str_replace_all(outside_support, "n/a", "no"),
         outside_support = str_replace_all(outside_support, "Wife and children", "yes"),
  )

support_oustide_bs <- support_oustide_bs|> 
  select(outside_support) |> 
  unnest_tokens(word, outside_support)|> 
  anti_join(stop_words_with_yes_no)



support_oustide_bs |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

support_oustide_bs_plot <- support_oustide_bs |> 
  filter(word == "yes"| word == "no") |>
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)|>
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 0.8, width = 0.8) +
  labs(title = "Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "RdPu")
support_oustide_bs |> 
  filter(word == "yes"| word == "no") |>
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)


support_oustide_bs_plot
support_oustide_bs_plot
ggsave(filename = "support_oustide_bs.png", plot = support_oustide_bs_plot,
       height = 4,
       width = 8) 

support_oustide_bs_percent <- support_oustide_bs |> 
  filter(word != "close",
         word != "status",
         word != "talk",
         word != "told") |>
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) 
support_oustide_bs_percent
write.csv(support_oustide_bs_percent, "support_oustide_bs_percent.csv")

#Any family/friends/loved ones that you would want to share your HIV status to
camden_islington$`Any family/friends/loved ones that you would want to share your HIV status to`

sharestatus_yes_no <- camden_islington|> 
  select(`Any family/friends/loved ones that you would want to share your HIV status to`) |> 
  unnest_tokens(word, `Any family/friends/loved ones that you would want to share your HIV status to`)|> 
  anti_join(stop_words_with_yes_no) |> 
  filter(word == "yes" | word == "no")

sharestatus_yes_no_percent <- sharestatus_yes_no |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)
sharestatus_yes_no_percent

#save table
write_csv(sharestatus_yes_no_percent, "sharestatus_yes_no_percent.csv")

sharestatus_yes_no_plot <- sharestatus_yes_no |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "Any family/friends/loved ones that you would want to share your HIV status to",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "RdPu")
 
brewer.pal.info

sharestatus_yes_no_plot
ggsave(filename = "sharestatus_yes_no_plot.png", plot = sharestatus_yes_no_plot,
       height = 4,
       width = 8) 

#ABOUT HEALTH
#`I am ___ at taking my HIV treatment compared to before I started coming to B&S
camden_islington$`I am ___ at taking my HIV treatment compared to before I started coming to B&S`

treatment_table <- camden_islington |> 
  select(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`
) |> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`
) |> 
  mutate(percent = n/sum(n)*100)

treatment_table
write_csv(treatment_table, "treatment.csv")

ggplot(camden_islington, aes(x = `I am ___ at taking my HIV treatment compared to before I started coming to B&S`, 
                             y = `Do you feel that you are able to manage your HIV medication`)) +
  geom_count()

treatment_medication <- camden_islington |> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`,
        `Do you feel that you are able to manage your HIV medication`) |> 
  mutate(percent = n/sum(n)*100)

write.csv(treatment_medication, "treatment_medication.csv")

#Do you feel that you are able to manage your HIV medication
camden_islington$`Do you feel that you are able to manage your HIV medication`

camden_islington_percent <- camden_islington|> 
  select(`Do you feel that you are able to manage your HIV medication`) |> 
  count(`Do you feel that you are able to manage your HIV medication`) |> 
  mutate(percent = n/sum(n)*100)

managing_medication <- camden_islington |> 
  select(`Do you feel that you are able to manage your HIV medication`) |> 
  count(`Do you feel that you are able to manage your HIV medication`, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(`Do you feel that you are able to manage your HIV medication`, percent)) |> 
  ggplot(aes(`Do you feel that you are able to manage your HIV medication`, percent, fill = `Do you feel that you are able to manage your HIV medication`))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "Do you feel that you are able to manage your HIV medication?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "RdPu")

managing_medication
ggsave(filename = "managing_medication.png", plot =managing_medication ,
       height = 4,
       width = 8) 
camden_islington_percent

write_csv(camden_islington_percent, "managemeds.csv")

#unused variables
camden_islington$`Something you've learnt at B&S about managing your health better`
camden_islington$`Is there something different that brings you to B&S compared to other HIV services?`


#Any other HIV organisations you attend, and where
camden_islington$`Any other HIV organisations you attend, and where`

camden_islington <- camden_islington |> 
  rename(other_orgs = `Any other HIV organisations you attend, and where`)

HIV_orgs  <- camden_islington %>%
  mutate(other_orgs = str_to_lower(other_orgs),
         other_orgs= str_replace_all(other_orgs, "only b&s", "no"),
         other_orgs= str_replace_all(other_orgs, "straight talk", "straight_talk"),
         other_orgs= str_replace_all(other_orgs, "positive east", "positive_east"))

HIV_orgs <- HIV_orgs|> 
  select(other_orgs) |> 
  unnest_tokens(word, other_orgs)|> 
  mutate(word = case_when(word == "straight_talk" ~ "straight talk",
                          word == "positive_east" ~ "positive east",
                          TRUE ~ word))|>
  anti_join(stop_words_with_yes_no)

HIV_orgs <- HIV_orgs |> 
  filter(word != "attend",
         word != "meetings")

HIV_orgs |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)


HIV_orgs_plot <- HIV_orgs |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "Any other HIV organisations you attend, and where?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "RdPu")
HIV_orgs_plot

ggsave(filename = "HIV_orgs_plot.png", plot = HIV_orgs_plot,
       height = 4,
       width = 8) 

HIV_orgs_percent <- HIV_orgs|> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

HIV_orgs_percent
write.csv(HIV_orgs_percent, "HIV_orgs_percent.csv")

#WELLBEING SCALE
WB_Scale_camden_islington <- as.data.frame(WB_Scale) |> 
  select(`I've been feeling optimistic about the future`:
           `I've been feeling cheerful`,
         Borough) |> 
  filter(Borough  == "Camden and Islington")

colnames(WB_Scale_camden_islington)

liker_WB_plot <- plot(likert(WB_Scale_camden_islington[,1:14]), ordered = F)+
  labs(title = "Well Being Scale") +
  scale_fill_manual(values = brewer.pal(n=5,"RdPu"),
                    breaks = c("Never",
                               "Rarely", 
                               "Some of the time",
                               "Often", 
                               "Always")) +
  guides(fill = guide_legend(title="Responses"))
liker_WB_plot
ggsave(filename = "liker_WB_plot.png", plot = liker_WB_plot,
       height = 4,
       width = 8) 


#ABOUT BODY AND SOUL
camden_islington$`What B&S means to you`
#What services have you found most helpful
services <- camden_islington|> 
  select(`What services have you found most helpful`) |> 
  unnest_tokens(word,`What services have you found most helpful`)|> 
  anti_join(stop_words)

services |> 
  count(word, sort = TRUE) |> 
  print(n = 28)

services <- services |> 
  mutate(word = str_to_lower(word),
         word = str_replace_all(word, "massages", "massage"),
         word = str_replace_all(word, "workshops", "workshop")) |> 
  count(word, sort = TRUE) |> 
  filter(word == "massage"|
         word == "workshop"|
         word == "casework"|
         word == "yoga"|
           word =="shiatsu"|
           word == "medication"|
           word == "nutrition"|
           word == "foodbank "|
           word == "call"|
           word == "food"|
           word == "acupuncture"|
           word == "immigration"|
           word == "doctors")
services

services_plot <- services |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(percent, word))+
  geom_col(show.legend = FALSE, alpha = 3/4,  fill = "deeppink4") +
  labs(title = "What services have you found most helpful?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))
services_plot

ggsave(filename = "services_plot.png", plot = services_plot,
       height = 4,
       width = 8) 

brewer.pal.info
services_percent <- services |> 
  mutate(percent = n/sum(n)*100)
services_percent

write.csv(services_percent, "services_percent.csv")

#Something B&S could do better
camden_islington$`Something B&S could do better`
 
better <- camden_islington|> 
  select(`Something B&S could do better`) |> 
  unnest_tokens(word,`Something B&S could do better`)|> 
  anti_join(stop_words) |> 
  print(n = 26)


better <- better |> 
  count(word, sort = TRUE) |> 
  filter(word == "workshops"|
           word == "therapies"|
           word =="skills"|
           word == "medication"|
           word == "nutrition"|
           word == "food"|
           word == "activites"|
           word == "doctors")

better_plot <- better |> 
  mutate(percent = n/sum(n)*100)|> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4 ) +
  labs(title = "Something B&S could do better?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "RdPu")
better_plot

ggsave(filename = "better_plot.png", plot = better_plot,
       height = 4,
       width = 8) 

better_percent <- better |> 
  mutate(percent = n/sum(n)*100)
better_percent
write.csv(better_percent, "better_percent.csv")

#`A way you would like to get more involved with B&S`
camden_islington |> 
  select(`A way you would like to get more involved with B&S`)
  
involved <- camden_islington|> 
  mutate(`A way you would like to get more involved with B&S`= str_to_lower(`A way you would like to get more involved with B&S`),
         `A way you would like to get more involved with B&S` = str_replace_all(`A way you would like to get more involved with B&S`, "help other members", "volunteer"),
         `A way you would like to get more involved with B&S` = str_replace_all(`A way you would like to get more involved with B&S`, "not at the moment", "no"))

involved <- involved |> 
  select(`A way you would like to get more involved with B&S`) |>
           unnest_tokens(word,`A way you would like to get more involved with B&S`)|> 
           anti_join(stop_words_with_yes_no)
involved

involved_plot <- involved |> 
  filter(word == "workshops"|
         word == "volunteer"|
         word == "connections"|
         word == "no") |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4 ) +
  labs(title = "A way you would like to get more involved with B&S?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "RdPu")

involved_plot
ggsave(filename = "involved_plot.png", plot = involved_plot,
       height = 4,
       width = 8) 

involved_percent <- involved |> 
  filter(word == "workshops"|
           word == "volunteer"|
           word == "connections"|
           word == "no") |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

involved_percent
write.csv(involved_percent, "involved_percent.csv")

#`Subjects/activities you would like to see in workshops`
camden_islington$`Subjects/activities you would like to see in workshops`

activities <- camden_islington |> 
  select(`Subjects/activities you would like to see in workshops`) |>
  unnest_tokens(word,`Subjects/activities you would like to see in workshops`)|> 
  anti_join(stop_words)

activities <- activities |> 
  mutate(word = str_replace_all(word, "nutrition", "health"),
         word= str_replace_all(word, "diet", "health"),
         word= str_replace_all(word, "exercise", "health"),
         word = str_replace_all(word, "trip", "excursions"))

activities_plot <- activities |> 
  count(word, sort = TRUE) |> 
  filter(word != "men's") |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4, fill = "maroon4" ) +
  labs(title = "Subjects/activities you would like to see in workshops?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))
activities_plot
ggsave(filename = "activities_plot.png", plot = activities_plot,
       height = 4,
       width = 8) 
activities

activities_percent <- activities |> 
  count(word, sort = TRUE) |> 
  filter(word != "men's") |> 
  mutate(percent = n/sum(n)*100) 
activities_percent
write.csv(activities_percent, "activities_percent.csv")


#BODY AND SOUL SCALE
camden_islington$`Has B&S helped you to feel less isolated or lonely`

#likert scales 
plot(likert(BS_Scale_dtf[,1:9]))
BS_Scale_c_i <- BS_Scale |> 
  filter(`Borough`== "Camden and Islington")

BS_Scale_dtf_c_i <- as.data.frame(BS_Scale_c_i) |> 
  select(`Has B&S helped you to feel less isolated or lonely`:
           `Has B&S helped you to cope and build your resilience`)
colnames(BS_Scale_dtf_c_i)
#cuter scales
#order and color the likert 
BS_likert_c_i <- plot(likert(BS_Scale_dtf_c_i[,1:9]), ordered = F)+
  labs(title = "Body and Soul Scale") +
  scale_fill_manual(values = brewer.pal(n=3, "RdPu"),
                    breaks = c("No",
                               "Sometimes",
                               "Yes")) +
  guides(fill = guide_legend(title="Responses"))
BS_likert_c_i

ggsave(filename = "BS_likert_c_i.png", plot =BS_likert_c_i ,
       height = 4,
       width = 8) 



