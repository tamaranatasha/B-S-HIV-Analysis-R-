#WB & Health 
library(coin)
library(tidyverse)
library(likert)
library(RColorBrewer)
library(ordinal)
library(tidytext)
#load data33
BS_HIV_dta <- read_csv('../Data Analysis/Planning for the Future 2024 Data.csv'
)
BS_HIV_dta 
colnames(BS_HIV_dta)

#WELL BEING SCALE AND HEALTH
WB_Scale <- BS_HIV_dta |> 
  select(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`:
         `I've been feeling cheerful`,
         Gender,
         Borough)

#list of levels
answer_levelsWB <- c("Always", 
                   "Often", 
                   "Some of the time",
                   "Rarely",
                   "Never")


#Recode variables to ordered factors WB Scale
colnames(WB_Scale)
WB_Scale$`I've been feeling useful` <- factor(WB_Scale$`I've been feeling useful`,
                                              levels = answer_levelsWB) 
 
WB_Scale$`I've been feeling relaxed` <- factor(WB_Scale$`I've been feeling relaxed`,
                                               levels = answer_levelsWB)

WB_Scale$`I've been feeling interested in other people` <- factor(WB_Scale$`I've been feeling interested in other people`,
                                               levels = answer_levelsWB)

WB_Scale$`I've been feeling optimistic about the future` <- factor(WB_Scale$`I've been feeling optimistic about the future`,
                                              levels = answer_levelsWB)

WB_Scale$`I've had energy to spare` <- factor(WB_Scale$`I've had energy to spare`,
                    levels = answer_levelsWB)

WB_Scale$`I've been dealing with problems well` <- factor(WB_Scale$`I've been dealing with problems well`,
                    levels = answer_levelsWB)

WB_Scale$`I've been thinking clearly` <- factor(WB_Scale$`I've been thinking clearly`,
                    levels = answer_levelsWB)

WB_Scale$`I've been feeling good about myself` <- factor(WB_Scale$`I've been feeling good about myself`,
                    levels = answer_levelsWB)

WB_Scale$`I've been feeling close to other people` <- factor(WB_Scale$`I've been feeling close to other people`,
             levels = answer_levelsWB)

WB_Scale$`I've been feeling confident` <- factor(WB_Scale$`I've been feeling confident`,
                    levels = answer_levelsWB)

WB_Scale$`I've been able to make up my own mind` <- factor(WB_Scale$`I've been able to make up my own mind`,
                    levels = answer_levelsWB)

WB_Scale$`I've been feeling loved` <- factor(WB_Scale$`I've been feeling loved`,
                    levels = answer_levelsWB)

WB_Scale$`I've been interested in new things` <- factor(WB_Scale$`I've been interested in new things`,
                   levels = answer_levelsWB)

WB_Scale$`I've been feeling cheerful` <- factor(WB_Scale$`I've been feeling cheerful`,
                    levels = answer_levelsWB)

#Counts and %

#WB_Scale
WB_Scale  |> 
  count(`I've been interested in new things`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been feeling useful`) |> 
  mutate(percentage = n/sum(n)*100) 

WB_Scale |> 
  count(`I've been feeling optimistic about the future`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been feeling relaxed`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been feeling interested in other people`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've had energy to spare`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been dealing with problems well`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been thinking clearly`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been feeling good about myself`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been feeling close to other people`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been feeling confident`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been able to make up my own mind`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been feeling loved`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(`I've been interested in new things`) |> 
  mutate(percentage = n/sum(n)*100)

WB_Scale |> 
  count(Gender) |> 
  mutate(percentage = n/sum(n)*100)

#create a crosstab which represents the disribution of 2 categories 
#WBscale
WB<-table(WB_Scale$`I've been feeling optimistic about the future`,
          WB_Scale$`I've been feeling useful`)
WB
addmargins(WB)
#as a percentage
prop.table(WB)
prop.table(WB,1)*100
sum(WB)

spineplot(WB)

#null hypothesis; there is no association between the two variables
lbl_test(WB)
#p = 0.004 - statistically significant, implies association 
#alpha = 0.05
#If the p-value for the test is less than alpha, 
#we reject the null hypothesis.


#likert Scales - always to never 
#turn into dataframes


WB_Scale_dtf <- as.data.frame(WB_Scale) |> 
  select(`I've been feeling optimistic about the future`:
           `I've been feeling cheerful`) 
  
colnames(WB_Scale_dtf)
WB_Scale_dtf
#plot likert scales

#all obs
plot(likert(WB_Scale_dtf[,1:14])) +
  labs(title = "WB Scale")

#likert all WB Scale questions using brewer palettes 
WB_likert <- plot(likert(WB_Scale_dtf[,1:14]), ordered = F)+
  labs(title = "Well Being Scale") +
 scale_fill_brewer(palette = "RdPu")
WB_likert
#save
ggsave(filename = "wb-likert.png", plot = WB_likert,
       height = 4,
       width = 9)


#display palettes and info
display.brewer.all()
brewer.pal.info

#Order and tidy about health - 
#I am ___ at taking my HIV treatment compared to before I started coming to B&S
levels <- c("better", "same", "worse") 
WB_Scale$`I am ___ at taking my HIV treatment compared to before I started coming to B&S`

WB_Scale <- WB_Scale |> 
  mutate(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`= 
           str_replace_all(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`,"The same", "Same"),
         `I am ___ at taking my HIV treatment compared to before I started coming to B&S`=
           str_to_lower(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`))
#as factor
WB_Scale$`I am ___ at taking my HIV treatment compared to before I started coming to B&S` <- 
  factor(WB_Scale$`I am ___ at taking my HIV treatment compared to before I started coming to B&S`,
        levels = levels)
#check results
WB_Scale |> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`)

better_plot <- WB_Scale |> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`) |> 
  filter(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`!= "NA") |> 
  ggplot(aes(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`, n)) +
  geom_col(show.legend = FALSE, 
           fill = "deeppink4",
           alpha = 3/4,
           width = 0.8) +
  labs(title = "I am ___ at taking my HIV treatment compared to before I started coming to B&S?", 
       y = NULL, x = NULL) +
  theme_gray(base_size = 8) +
  theme(
    plot.title = element_text(size = 9))

better_plot 
ggsave(filename = "better_plot.png", plot = better_plot ,
       height = 4,
       width = 8)

treatment_percent <- WB_Scale |> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`) |> 
  mutate(percent = n/sum(n)*100)

treatment_percent
write_csv(treatment_percent, "treatment.csv")

#Do you feel that you are able to manage your HIV medication
WB_Scale$`Do you feel that you are able to manage your HIV medication`

#as factor
WB_Scale$`Do you feel that you are able to manage your HIV medication` <- 
  factor(WB_Scale$`Do you feel that you are able to manage your HIV medication`,
         levels = answer_levelBS)

#check results
WB_Scale |> 
  count(`Do you feel that you are able to manage your HIV medication`)

managing_medication_percent <- WB_Scale |> 
  count(`Do you feel that you are able to manage your HIV medication`) |> 
  mutate(percent = n/sum(n)*100)

managing_medication_percent
write.csv(managing_medication_percent, "managing_medication_percent.csv")

med_plot <- WB_Scale |> 
  count(`Do you feel that you are able to manage your HIV medication`) |> 
  filter(`Do you feel that you are able to manage your HIV medication`!= "NA") |> 
  ggplot(aes(`Do you feel that you are able to manage your HIV medication`, n)) +
  geom_col(show.legend = FALSE, 
           fill = "violetred3",
           alpha = 3/4,
           width = 0.8) +
  labs(title = "Do you feel that you are able to manage your HIV medication?", 
       y = NULL, x = NULL) +
  theme_gray(base_size = 8) +
  theme(
    plot.title = element_text(size = 9))

med_plot

ggsave(filename = "med_plot.png", plot = med_plot ,
       height = 4,
       width = 8)


# Something you've learnt at B&S about managing your health better
WB_Scale$`Something you've learnt at B&S about managing your health better`

managing_health <- WB_Scale |> 
  select(`Something you've learnt at B&S about managing your health better`)
managing_health

managing_health <- WB_Scale |> 
  select(`Something you've learnt at B&S about managing your health better`) |> 
  unnest_tokens(word, `Something you've learnt at B&S about managing your health better`)|> 
  anti_join(stop_words)

managing_health <- managing_health |> 
  mutate(word = str_to_lower(word),
         word = str_replace_all(word, "confident","confidence"),
         word = str_replace_all(word, "eating", "nutrition"),
         word = str_replace_all(word, "eat", "nutrition"),
         word = str_replace_all(word, "diet", "nutrition"),
         word = str_replace_all(word, "healthy", "health"),
         word = str_replace_all(word, "food", "nutrition"),
         word = str_replace_all(word, "medications", "medication"))

managing_health |> 
  count(word, sort = TRUE )

managing_health_percent <- managing_health |> 
  count(word, sort = TRUE) |> 
  filter(word != "NA",
         word != "lot",
         word != "learn",
         word != "speak",
         word != "taking",
         n > 1) |> 
  mutate(word = reorder(word, n),
         percent = n/sum(n)*100) |> 
  print(n = 30)

mh_plot <- managing_health |> 
  count(word, sort = TRUE) |> 
  filter(word != "NA",
         word != "lot",
         word != "learn",
         word != "speak",
         word != "taking",
         n > 3) |> 
  mutate(word = reorder(word, n),
         percent = n/sum(n)*100) |> 
  ggplot(aes(n, word))+
  geom_col(show.legend = FALSE, fill = "hotpink2", alpha = 3/4) +
  labs(title = "Something you've learnt at B&S about managing your health better?",
       y = NULL,
       x = "number of responses") +
  theme_gray(base_size = 9)+
  theme(
    plot.title = element_text(size = 8)) 

mh_plot
ggsave(filename = "mh_plot.png", plot = mh_plot ,
       height = 4,
       width = 8)

managing_health |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

managing_health_percent2 <- managing_health |> 
  count(word, sort = TRUE) |> 
  filter(word != "NA",
         word != "lot",
         word != "learn",
         word != "speak",
         word != "taking",
         n > 3) |> 
  mutate(word = reorder(word, n),
         percent = n/sum(n)*100)

managing_health_percent2
write.csv(managing_health_percent2, "managing_health_percent2.csv")

#Any other HIV organisations you attend, and where
stop_words_with_no <- stop_words |> 
  filter(word != "none")

WB_Scale$`Any other HIV organisations you attend, and where`

other_orgs <- WB_Scale |> 
  select(`Any other HIV organisations you attend, and where`)

other_orgs <- other_orgs |> 
  rename(organisation = `Any other HIV organisations you attend, and where`)

other_orgs |> 
  print(n = 82)
other_orgs <- other_orgs |> 
  mutate(organisation = str_to_lower(organisation),
         organisation = str_replace_all(organisation, "positive east", "positive_east"),
         organisation = str_replace_all(organisation,"positively uk", "positively_uk"),
         organisation = str_replace_all(organisation,"no", "none"),
         organisation = str_replace_all(organisation,"river house", "river_house"),
         organisation = str_replace_all(organisation,"straight talk", "straight_talk"),
         organisation = str_replace_all(organisation,"red ribbon", "red_ribbon"),
         organisation = str_replace_all(organisation,"mortimer market", "mortimer_market"))
other_orgs         

other_orgs <- other_orgs |> 
  select(organisation) |> 
  unnest_tokens(word, organisation)|>
  mutate(word = case_when(word == "positive_east" ~ "positive east",
                          word == "positively_uk"~ "positively uk",
                          word == "river_house"~ "river house",
                          word == "straight_talk"~ "straight talk",
                          word == "red_ribbon"~ "red ribbon",
                          word == "mortimer_market" ~ "mortimer market",
                          TRUE ~ word)) |> 
  anti_join(stop_words_with_no)

other_orgs |> 
  filter(word != "NA") |> 
  count(word, sort = TRUE) |> 
  mutate(n/sum(n)*100) |> 
  print(n = 49)

#plot hiv orgs
other_orgs_plot <- other_orgs |> 
  count(word, sort = TRUE) |> 
  filter(word != "NA",
         word != "closed",
         word != "afford",
         word != "hammersmith",
         word != "nonew",
         word != "nonene",
         word != "positive",
         word != "travel",
         n > 1) |> 
  mutate(word = reorder(word, n),
         ) |> 
  ggplot(aes(n, word)) +
  geom_col(show.legend = FALSE, fill = "rosybrown2", alpha = 3/4) +
  labs(title = "Any other HIV organisations you attend, and where?",
       y = NULL,
       x = "number of responses") +
  theme_gray(base_size = 8)+
  theme(
    plot.title = element_text(size = 9)) 

other_orgs_plot
ggsave(filename = "other_orgs.png", plot = other_orgs_plot ,
       height = 4,
       width = 8)

other_orgs_percent2 <- other_orgs |> 
  count(word, sort = TRUE) |> 
  filter(word != "NA",
         word != "closed",
         word != "afford",
         word != "hammersmith",
         word != "nonew",
         word != "nonene",
         word != "positive",
         word != "travel",
         n > 1) |> 
  mutate(percent = n/sum(n)*100)

other_orgs_percent <- other_orgs |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

other_orgs_percent2
other_orgs_percent
write.csv(other_orgs_percent2, "other_orgs_percent2.csv")

#percentage of other hiv orgs filtered
other_orgs_percent_stemmed <- other_orgs |> 
  count(word, sort = TRUE) |> 
  filter(word != "NA",
         word != "closed",
         word != "afford",
         word != "hammersmith",
         word != "nonew",
         word != "nonene",
         word != "positive",
         word != "travel",
         n > 1) |> 
  mutate(prcent = n/sum(n)*100)
other_orgs_percent_stemmed

#Is there something different that brings you to B&S compared to other HIV services? 
WB_Scale$`Is there something different that brings you to B&S compared to other HIV services?`

why_bs_different <- WB_Scale |> 
  select(`Is there something different that brings you to B&S compared to other HIV services?`)

why_bs_different <- why_bs_different |> 
  rename(different = `Is there something different that brings you to B&S compared to other HIV services?`)
why_bs_different |> 
  print(n = 82)
#tokenization 
why_bs_tidy <- why_bs_different |> 
  select(different) |> 
  unnest_tokens(word, different)|> 
  anti_join(stop_words)

why_bs_tidy |> 
  mutate()
  
why_bs_tidy_percent <- why_bs_tidy |> 
  filter(word != "NA") |> 
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

why_bs_tidy_percent |> 
  print(n = 92)

why_bs_tidy_percent2 <- why_bs_tidy_percent |> 
  filter(n > 1,
         stem != "feel") |> 
  print(n = 22)

write.csv(why_bs_tidy_percent2, "why_bs_tidy_percent2.csv")  

why_bs_tidy_plot <-  why_bs_tidy_percent |> 
  filter(n > 1,
         stem != "feel") |> 
  ggplot(aes(stem, n)) +
  geom_col(show.legend = FALSE, fill = "maroon4", alpha = 3/4) +
  labs(title = "Is there something different that brings you to B&S compared to other HIV services?",
       y = "occurence",
       x = "word stem") +
  theme_gray(base_size = 8)+
  theme(
    plot.title = element_text(size = 9)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

why_bs_tidy_plot
ggsave(filename = "why_bs_tidy_plot.png", plot = why_bs_tidy_plot ,
       height = 4,
       width = 8)

managing_treatmet_no <- WB_Scale |> 
  filter(`Do you feel that you are able to manage your HIV medication` == "No") |> 
  select(`Do you feel that you are able to manage your HIV medication`,`I am ___ at taking my HIV treatment compared to before I started coming to B&S`)


managing_med_no <- WB_Scale |> 
  filter(`I am ___ at taking my HIV treatment compared to before I started coming to B&S` == "worse") |> 
  select(`Do you feel that you are able to manage your HIV medication`,`I am ___ at taking my HIV treatment compared to before I started coming to B&S`)

managing_med_no
managing_treatmet_no
#tables
treatment_percent
other_orgs_percent_stemmed



