#Subset Data
hackney <- BS_HIV_dta |> 
  filter(Borough == "Hackney")

#`Who or where do you turn to for support`
hackney$`Who or where do you turn to for support`

hackney_subset <- hackney |> 
  mutate(`Who or where do you turn to for support`= str_to_lower(`Who or where do you turn to for support`),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`,"b&s", "body_soul"),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`, "no one", "no_one"),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`, "familys", "family"),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`, "god", "church"))

hackney_unested <- hackney_subset |> 
  select(`Who or where do you turn to for support`) |> 
  unnest_tokens(word, `Who or where do you turn to for support`) %>%
  mutate(word = case_when(word == "body_soul" ~ "body & soul",
                          word == "care_worker" ~ "care worker",
                          word == "no_one" ~ "no one",
                          TRUE ~ word))|> 
  anti_join(stop_words)
hackney_unested
hackney_counts <- hackney_unested |> 
  filter(
    word == "family"|
    word == "church"|
    word == "friends"|
    word == "body & soul"|
    word == "no one"|
    word == "carer") |> 
  count(word, sort = TRUE)

hackney_percent <- hackney_counts |> 
  mutate(percent = n/sum(n)*100)
hackney_percent
write.csv(hackney_percent, "hackney_percent.csv")

hackney_support <- hackney_counts |> 
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
  scale_fill_brewer(palette = "YlOrRd")
hackney_support

ggsave(filename = "hackney_support.png", plot = hackney_support,
       height = 4,
       width = 8) 


brewer.pal.info
#Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`

hackney$`Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`

hackney <- hackney |> 
  rename(outside = `Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`)

hackney$outside

hackney <- hackney |> 
  mutate(outside = str_to_lower(outside),
         outside = str_replace_all(outside, "my two daughters.", "yes"),
         outside = str_replace_all(outside, "family", "yes"),
         outside = str_replace_all(outside, "my mum", "yes"))

hackney_outside_support <- hackney|> 
  select(outside) |> 
  unnest_tokens(word, outside)|> 
  anti_join(stop_words_with_yes_no)

hackney_outside_support_percent <- hackney_outside_support |> 
  filter(word != "NA") |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)
hackney_outside_support_percent

write.csv(hackney_outside_support_percent, "hackney_outside_support.csv")

hackney_outside_support_plot <- hackney_outside_support |> 
  filter(word != "NA") |> 
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
  scale_fill_brewer(palette = "YlOrRd")

hackney_outside_support_plot
ggsave(filename = "hackney_outside_support_plot.png", plot = hackney_outside_support_plot,
       height = 4,
       width = 8) 

#Any family/friends/loved ones that you would want to share your HIV status to`
hackney$`Any family/friends/loved ones that you would want to share your HIV status to`

hackney_share <- hackney|> 
  select(`Any family/friends/loved ones that you would want to share your HIV status to`) |> 
  unnest_tokens(word, `Any family/friends/loved ones that you would want to share your HIV status to`)|> 
  anti_join(stop_words_with_yes_no) |> 
  filter(word == "yes" | word == "no")

hackney_share_percent <- hackney_share |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

hackney_share_percent

hackney_share_plot <- hackney_share |> 
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
  scale_fill_brewer(palette = "YlOrRd")

ggsave(filename = "hackney_share_plot.png", plot = hackney_share_plot,
       height = 4,
       width = 8) 
write.csv(hackney_share_percent, "hackney_share_percent.csv")


#I am ___ at taking my HIV treatment compared to before I started coming to B&S`
hackney$`I am ___ at taking my HIV treatment compared to before I started coming to B&S`

hackney <- hackney |> 
  mutate(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`= str_to_lower(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`))


hackney_treatment <- hackney|> 
  filter(`I am ___ at taking my HIV treatment compared to before I started coming to B&S` != "NA") |> 
  select(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`
  ) |> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`
  ) |> 
  mutate(percent = n/sum(n)*100) 

hackney_treatment_plot <- ggplot(hackney_treatment, aes(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`, 
                              percent, 
                              fill = `I am ___ at taking my HIV treatment compared to before I started coming to B&S`))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "I am ___ at taking my HIV treatment compared to before I started coming to B&S",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "YlOrRd")
hackney_treatment_plot

ggsave(filename = "hackney_treatment_plot.png", plot = hackney_treatment_plot,
       height = 4,
       width = 8) 
write.csv(hackney_treatment, "hackney_treatment.csv")
hackney_treatment
hackney_treatment_plot
#`Do you feel that you are able to manage your HIV medication`
hackney$`Do you feel that you are able to manage your HIV medication`

hackney <- hackney|> 
  mutate(`Do you feel that you are able to manage your HIV medication`= 
           str_to_lower(`Do you feel that you are able to manage your HIV medication`))

hackney_two_vars <- hackney|> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`,
        `Do you feel that you are able to manage your HIV medication`,
        sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

write.csv(hackney_two_vars, "hackney_two_vars.csv")

hackney_medication <- hackney|> 
  filter(`Do you feel that you are able to manage your HIV medication` != "NA") |> 
  count(`Do you feel that you are able to manage your HIV medication`,
        sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)


hackney_medication_plot <- ggplot(hackney_medication, aes(`Do you feel that you are able to manage your HIV medication`, 
                              percent, 
                              fill = `Do you feel that you are able to manage your HIV medication`))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "Do you feel that you are able to manage your HIV medication",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "YlOrRd")

ggsave(filename = "hackney_medication_plot.png", plot = hackney_medication_plot,
       height = 4,
       width = 8) 
write.csv(hackney_medication, "hackney_medication.csv")
hackney_medication

#Something you've learnt at B&S about managing your health better`
hackney$`Something you've learnt at B&S about managing your health better`

hackney_health <- hackney|> 
  select(`Something you've learnt at B&S about managing your health better`) |> 
  unnest_tokens(word,`Something you've learnt at B&S about managing your health better`)|> 
  anti_join(stop_words) |> 
  print(n = 25)

hackney_health |> 
  filter(word == "medication"| 
           word == "rest"|
           word == "friends"|
           word == "confidence")


#Any other HIV organisations you attend, and where
hackney$`Any other HIV organisations you attend, and where`
hackney <- hackney |> 
  rename(other_orgs2 = `Any other HIV organisations you attend, and where`)
hackney$other_orgs2

hackney[3,]
df[3,]
hackney |> 
  select(other_orgs2)
hackney_otherorgs <- hackney |> 
  mutate(other_orgs2 = str_to_lower(other_orgs2),
         other_orgs2= str_replace_all(other_orgs2, "doctor and clinic", "doctor_clinic"),
         other_orgs2= str_replace_all(other_orgs2, "straight talk", "straight_talk"),
         other_orgs2= str_replace_all(other_orgs2, "positive east", "positive_east"),
         other_orgs2= str_replace_all(other_orgs2, "red ribbon", "red_ribbon"),
         other_orgs2= str_replace_all(other_orgs2, "positively uk", "positively_uk"),
         other_orgs2= str_replace_all(other_orgs2, "rise.com", "rise_com"))

hackney_otherorgs

hackney_otherorgs <- hackney_otherorgs|> 
  select(other_orgs2) |> 
  unnest_tokens(word, other_orgs2)|> 
  mutate(word = case_when(word == "straight_talk" ~ "straight talk",
                          word == "positive_east" ~ "positive east",
                          word == "doctor_clinic" ~ "doctor/clinic",
                          word == "red_ribbon" ~ "red ribbon",
                          word == "rise_com" ~ "rise.com",
                          word == "positively_uk" ~ "positively uk",
                          TRUE ~ word))|>
  anti_join(stop_words_with_yes_no)
hackney_otherorgs |> 
  count(word, sort = TRUE)

hackney_otherorgs_plot <- hackney_otherorgs |> 
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
  scale_fill_brewer(palette = "YlOrRd")

hackney_otherorgs <- hackney_otherorgs |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

ggsave(filename = "hackney_otherorgs_plot.png", plot = hackney_otherorgs_plot,
       height = 4,
       width = 8) 
write.csv(hackney_otherorgs, "hackney_otherorgs.csv")

hackney_otherorgs_plot

#Wellbeing Scale
WB_Scale_hackney <- as.data.frame(WB_Scale) |> 
  select(`I've been feeling optimistic about the future`:
           `I've been feeling cheerful`,
         Borough) |> 
  filter(Borough  == "Hackney")

WB_Scale_hackney_plot <- plot(likert(WB_Scale_hackney[,1:14]), ordered = F)+
  labs(title = "Well Being Scale") +
  scale_fill_brewer(palette = "YlOrRd")


WB_Scale_hackney_plot

ggsave(filename = "WB_Scale_hackney_plot.png", plot = WB_Scale_hackney_plot,
       height = 4,
       width = 8) 


#What services have you found most helpful`
hackney$`What services have you found most helpful`

hackney_services <- hackney|> 
  select(`What services have you found most helpful`) |> 
  unnest_tokens(word,`What services have you found most helpful`)|> 
  anti_join(stop_words) |> 
  print(n = 24)

hackney_services |> 
  count(word, sort = TRUE)

hackney_services <- hackney_services |> 
  count(word, sort = TRUE) |> 
  filter(word == "massage"|
           word == "eating"|
           word == "casework"|
           word == "acupuncture"|
           word =="treatment"|
           word == "therapies")
hackney_services

hackney_services <- hackney_services |> 
  mutate(percent = n/sum(n)*100) 
hackney_services
hackney_services_plot <- hackney_services |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "What services have you found most helpful?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "YlOrRd")

ggsave(filename = "hackney_services_plot.png", plot = hackney_services_plot,
       height = 4,
       width = 8) 
hackney_services_plot
write.csv(hackney_services, "hackney_services.csv")


#A way you would like to get more involved with B&S`
hackney$`A way you would like to get more involved with B&S`

hackney_involved <- hackney|> 
  mutate(`A way you would like to get more involved with B&S`= str_to_lower(`A way you would like to get more involved with B&S`),
         `A way you would like to get more involved with B&S` = str_replace_all(`A way you would like to get more involved with B&S`, "voluntary", "volunteer"),
         `A way you would like to get more involved with B&S` = str_replace_all(`A way you would like to get more involved with B&S`, "drum teacher", "drum_teacher"),
         `A way you would like to get more involved with B&S` = str_replace_all(`A way you would like to get more involved with B&S`, "peer mentoring", "peer_mentoring"))

hackney_involved <- hackney_involved |> 
  select(`A way you would like to get more involved with B&S`) |>
  unnest_tokens(word,`A way you would like to get more involved with B&S`)|>
  mutate(word = case_when(word == "drum_teacher" ~ "drum teacher",
                          word == "peer_mentoring" ~ "peer mentoring",
                          TRUE ~ word)) |> 
  anti_join(stop_words_with_yes_no)

hackney_involved_percent <- hackney_involved |> 
  filter(word == "drum teacher"|
           word == "volunteer"|
           word == "experiences"|
           word == "peer mentoring") |> 
  count(word) |> 
  mutate(percent = n/sum(n)*100)
hackney_involved_percent
hackney_involved_plot <- hackney_involved |> 
  filter(word == "drum teacher"|
           word == "volunteer"|
           word == "experiences"|
           word == "peer mentoring") |> 
  count(word) |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "A way you would like to get more involved with B&S?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "YlOrRd")
  
ggsave(filename = "hackney_involved_plot.png", plot = hackney_involved_plot,
       height = 4,
       width = 8) 
hackney_involved_plot
write.csv(hackney_involved_percent, "hackney_involved_percent.csv")

#`Subjects/activities you would like to see in workshops
hackney$`Subjects/activities you would like to see in workshops`

hackney_activities <- hackney|> 
  select(`Subjects/activities you would like to see in workshops`) |> 
  unnest_tokens(word,`Subjects/activities you would like to see in workshops`)|> 
  anti_join(stop_words) 

hackney_activities <- hackney_activities|> 
  filter(word == "dancing"|
           word == "exercise"|
           word == "bingo"|
           word == "drums"|
           word =="music"|
           word == "writing") |> 
  count(word, sort = TRUE)

hackney_activities_percent <- hackney_activities |> 
  mutate(percent = n/sum(n)*100)

hackney_activities_plot <- hackney_activities |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "Subjects/activities you would like to see in workshops?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "YlOrRd")

ggsave(filename = "hackney_activities_plot.png", plot = hackney_activities_plot,
       height = 4,
       width = 8) 
write.csv(hackney_activities_percent, "hackney_activities_percent.csv")
hackney_activities_percent
#unused variables 
hackney$`Something B&S could do better`
hackney$`What B&S means to you`


#BODY AND SOUL SCALE
hackney$`Has B&S helped you to feel less isolated or lonely`

hackney_BS_scale <- as.data.frame(BS_Scale) |> 
  select(`Has B&S helped you to feel less isolated or lonely`:
           `Has B&S helped you to cope and build your resilience`,
         Borough)
hackney_BS_scale
colnames(hackney_BS_scale)

#likert scales 
plot(likert(hackney_BS_scale[,1:9]))

hackney_BS_scale <- hackney_BS_scale |> 
  filter(`Borough`== "Hackney")

hackney_BS_scale

hackney_BS_scale_plot <- plot(likert(hackney_BS_scale[,1:9]), ordered = F)+
  labs(title = "Body and Soul Scale") +
  scale_fill_brewer(palette = "YlOrRd")
  
hackney_BS_scale_plot

ggsave(filename = "hackney_BS_scale_plot.png", plot = hackney_BS_scale_plot,
       height = 4,
       width = 8) 
