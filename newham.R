#Data
newham <- BS_HIV_dta |> 
  filter(Borough == "Newham")
newham
#Who or where do you turn to for support
newham$`Who or where do you turn to for support`

newham_support <- newham |> 
mutate(`Who or where do you turn to for support`= str_to_lower(`Who or where do you turn to for support`),
       `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`,"b&s", "body_soul"))

newham_support <- newham_support |> 
  select(`Who or where do you turn to for support`) |> 
  unnest_tokens(word, `Who or where do you turn to for support`) %>%
  mutate(word = case_when(word == "body_soul" ~ "body & soul",
                          TRUE ~ word))|> 
  anti_join(stop_words) 
newham_support

newham_counts <- newham_support |> 
  filter(
      word == "family"|
      word == "god"|
      word == "friends"|
      word == "body & soul") |> 
  count(word, sort = TRUE)
newham_counts

newham_percent <- newham_counts |> 
  mutate(percent = n/sum(n)*100)

newham_percent
brewer.pal.info

write.csv(newham_percent, "newham_percent.csv")

newham_percent_plot <- newham_percent|> 
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
  scale_fill_brewer(palette = "YlGnBu")

ggsave(filename = "newham_percent_plot.png", plot = newham_percent_plot,
       height = 4,
       width = 8) 
write.csv(newham_percent, "newham_percent.csv")


#Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`
newham$`Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`

newham_out <- newham |> 
  rename(outside = `Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`)

newham_out$outside

newham_out <- newham_out |> 
  mutate(outside = str_to_lower(outside),
         outside = str_replace_all(outside, "my family", "yes"),
         outside = str_replace_all(outside, "i have a family and friends, group of friends who we organise day to go for community walk or help. i choose who to talk to about my hiv status.", "yes"),
         outside = str_replace_all(outside, "only daughter", "yes"),
         outside = str_replace_all(outside, "only people at b&s", "no"))

newham_out <- newham_out|> 
  select(outside) |> 
  unnest_tokens(word, outside)|> 
  anti_join(stop_words_with_yes_no)

newham_out_percent <- newham_out |>
  filter(word == "yes"| word == "no") |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)
newham_out_percent

newham_out_plot <- newham_out_percent|>
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 0.8, width = 0.8) +
  labs(title = "Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "YlGnBu")  

ggsave(filename = "newham_out_plot.png", plot = newham_out_plot,
       height = 4,
       width = 8) 
write.csv(newham_out_percent, "newham_out_percent.csv")

#`Any family/friends/loved ones that you would want to share your HIV status to`
newham$`Any family/friends/loved ones that you would want to share your HIV status to`

newham_share <- newham|> 
  select(`Any family/friends/loved ones that you would want to share your HIV status to`) |> 
  unnest_tokens(word, `Any family/friends/loved ones that you would want to share your HIV status to`)|> 
  anti_join(stop_words_with_yes_no) |> 
  filter(word == "yes" | word == "no")

newham_share_percent <- newham_share|> 
  count(word) |> 
  mutate(percent = n/sum(n)*100) 

newham_share_plot <- newham_share |> 
  count(word) |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "Any family/friends/loved ones that you would want to share your HIV status to",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "YlGnBu")

ggsave(filename = "newham_share_plot.png", plot = newham_share_plot,
       height = 4,
       width = 8) 
write.csv(newham_share_percent, "newham_share_percent.csv")

#I am ___ at taking my HIV treatment compared to before I started coming to B&S`
newham$`I am ___ at taking my HIV treatment compared to before I started coming to B&S`

newham_treatment <- newham |> 
  select(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`
  ) |> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`
  ) |> 
  mutate(percent = n/sum(n)*100)

newham_treatment
newham_treatment_plot <- ggplot(newham_treatment, aes(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`, 
                                                        percent, 
                                                        fill = `I am ___ at taking my HIV treatment compared to before I started coming to B&S`))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "I am ___ at taking my HIV treatment compared to before I started coming to B&S",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "YlGnBu")
newham_treatment_plot


ggsave(filename = "newham_treatment_plot.png", plot = newham_treatment_plot,
       height = 4,
       width = 8) 
write.csv(newham_treatment, "newham_treatment.csv")

#Do you feel that you are able to manage your HIV medication`
newham$`Do you feel that you are able to manage your HIV medication`

newham |> 
  count(`Do you feel that you are able to manage your HIV medication`) |> 
  mutate(percent = n/sum(n)*100)

newham_two_vars <- newham|> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`,
        `Do you feel that you are able to manage your HIV medication`,
        sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)
newham_two_vars

newham$`Something you've learnt at B&S about managing your health better`
newham$`Any other HIV organisations you attend, and where`

write.csv(newham_two_vars, "newham_two_vars.csv")

#`Any other HIV organisations you attend, and where`
newham <- newham |> 
  rename(other_orgs = `Any other HIV organisations you attend, and where`)
newham$other_orgs

newham_otherorgs <- newham |> 
  mutate(other_orgs= str_to_lower(other_orgs),
         other_orgs= str_replace_all(other_orgs, "straight talk", "straight_talk"),
         other_orgs= str_replace_all(other_orgs, "positive east", "positive_east"),
         other_orgs= str_replace_all(other_orgs, "positive eat", "positive_east"))

newham_otherorgs <- newham_otherorgs|> 
  select(other_orgs) |> 
  unnest_tokens(word, other_orgs)|> 
  mutate(word = case_when(word == "straight_talk" ~ "straight talk",
                          word == "positive_east" ~ "positive east",
                          TRUE ~ word))|>
  anti_join(stop_words_with_yes_no)

newham_otherorgs_percent <- newham_otherorgs|> 
  filter(word == "no"|
           word == "positive east"|
           word == "straight talk") |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)
newham_otherorgs_percent
newham_otherorgs_plot <- newham_otherorgs |> 
  filter(word == "no"|
           word == "positive east"|
           word == "straight talk") |> 
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
  scale_fill_brewer(palette = "YlGnBu")

ggsave(filename = "newham_otherorgs_plot.png", plot = newham_otherorgs_plot,
       height = 4,
       width = 8) 
write.csv(newham_otherorgs_percent, "newham_otherorgs_percent.csv")

#Wellbeing Scale
WB_Scale_newham <- as.data.frame(WB_Scale) |> 
  select(`I've been feeling optimistic about the future`:
           `I've been feeling cheerful`,
         Borough) |> 
  filter(Borough == "Newham")
WB_Scale_newham

WB_Scale_newham_plot <- plot(likert(WB_Scale_newham[,1:14]), ordered = F)+
  labs(title = "Well Being Scale") +
  scale_fill_brewer(palette = "YlGnBu")

ggsave(filename = "WB_Scale_newham_plot.png", plot = WB_Scale_newham_plot,
       height = 4,
       width = 8)

WB_Scale_newham_plot

#What services have you found most helpful
newham$`What services have you found most helpful`

newham_services <- newham|> 
  select(`What services have you found most helpful`) |> 
  unnest_tokens(word,`What services have you found most helpful`)|> 
  anti_join(stop_words) |> 
  print(n = 24)

newham_services <- newham_services |> 
  mutate(word = str_replace_all(word, "dishes", "food"),
         word = str_replace_all(word, "eat", "food")) |> 
  filter(word == "casework"|
           word == "food"|
           word == "workshops"|
           word == "transport") |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

newham_services
newham_services_plot <- newham_services |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "What services have you found most helpful?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "YlGnBu")
newham_services_plot

ggsave(filename = "newham_services_plot.png", plot = newham_services_plot,
       height = 4,
       width = 8) 
write.csv(newham_services, "newham_services.csv")

#Subjects/activities you would like to see in workshops`
newham$`Subjects/activities you would like to see in workshops`

newham_activities <- newham|> 
  select(`Subjects/activities you would like to see in workshops`) |> 
  unnest_tokens(word,`Subjects/activities you would like to see in workshops`)|> 
  anti_join(stop_words)


newham_activities_percent <- newham_activities|> 
  filter(word == "dancing"|
           word == "exercises"|
           word == "singing"|
           word == "yoga"|
           word =="meditation"|
           word =="eating"|
           word == "therapy") |> 
  count(word, sort = TRUE)

newham_activities_percent |> 
  mutate(percent = n/sum(n)*100)

newham_activities_plot <- newham_activities_percent |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "Subjects/activities you would like to see in workshops?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "YlGnBu")

ggsave(filename = "newham_activities_plot.png", plot = newham_activities_plot,
       height = 4,
       width = 8) 
write.csv(newham_activities_percent, ".csv")

#BODY AND SOUL SCALE
newham_BS_scale <- as.data.frame(BS_Scale) |> 
  select(`Has B&S helped you to feel less isolated or lonely`:
           `Has B&S helped you to cope and build your resilience`,
         Borough) |> 
  filter(Borough == "Newham")
  
newham_BS_scale
colnames(newham_BS_scale)

newham_BS_scale_plot <- plot(likert(newham_BS_scale[,1:9]), ordered = F)+
  labs(title = "Body and Soul Scale") +
  scale_fill_brewer(palette = "YlGnBu")
newham_BS_scale_plot

ggsave(filename = "newham_BS_scale_plot.png", plot = newham_BS_scale_plot,
       height = 4,
       width = 8) 
 

newham$`A way you would like to get more involved with B&S`
