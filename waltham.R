#Data
waltham <- BS_HIV_dta |> 
  filter(Borough == "Waltham Forest")
waltham

brewer.pal.info

#Who or where do you turn to for support`
waltham$`Who or where do you turn to for support`
waltham_support <- waltham |> 
  mutate(`Who or where do you turn to for support`= str_to_lower(`Who or where do you turn to for support`),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`,"b&s", "body_soul"))
waltham_support$`Who or where do you turn to for support`

waltham_support <- waltham_support |> 
  select(`Who or where do you turn to for support`) |> 
  unnest_tokens(word, `Who or where do you turn to for support`) %>%
  mutate(word = case_when(word == "body_soul" ~ "body & soul",
                          TRUE ~ word))|> 
  anti_join(stop_words) 

waltham_support <- waltham_support |> 
  filter(
    word == "family"|
      word == "friends"|
      word == "body & soul") |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)
waltham_support

waltham_support_plot <- waltham_support |> 
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
  scale_fill_brewer(palette = "PuBuGn")
waltham_support_plot
ggsave(filename = "waltham_support_plot.png", plot = waltham_support_plot,
       height = 4,
       width = 8) 
write.csv(waltham_support, "waltham_support.csv")

#Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`
waltham$`Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`

waltham_out <- waltham |> 
  rename(outside = `Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`)

waltham_out <- waltham_out|> 
  select(outside) |> 
  unnest_tokens(word, outside)|> 
  anti_join(stop_words_with_yes_no)

waltham_out_percent <- waltham_out |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)
waltham_out_percent

waltham_out_plot <- waltham_out  |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 0.8, width = 0.8) +
  labs(title = "Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "PuBuGn")  

ggsave(filename = "waltham_out_plot.png", plot = waltham_out_plot,
       height = 4,
       width = 8) 
write.csv(waltham_out_percent, "waltham_out_percent.csv")

#Any family/friends/loved ones that you would want to share your HIV status to`
waltham$`Any family/friends/loved ones that you would want to share your HIV status to`

waltham_share <- waltham|> 
  select(`Any family/friends/loved ones that you would want to share your HIV status to`) |> 
  unnest_tokens(word, `Any family/friends/loved ones that you would want to share your HIV status to`)|> 
  anti_join(stop_words_with_yes_no) |> 
  filter(word == "yes" | word == "no")
waltham_share

waltham_share_percent <- waltham_share |> 
  count(word) |> 
  mutate(percent = n/sum(n)*100) 
waltham_share_percent
waltham_share_plot <- waltham_share |> 
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
  scale_fill_brewer(palette = "PuBuGn")
waltham_share_plot
brewer.pal.info
ggsave(filename = "waltham_share_plot.png", plot = waltham_share_plot,
       height = 4,
       width = 8) 
write.csv(waltham_share_percent, "waltham_share_percent.csv")

#I am ___ at taking my HIV treatment compared to before I started coming to B&S`
waltham$`I am ___ at taking my HIV treatment compared to before I started coming to B&S`
waltham_treatment <- waltham |> 
  select(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`
  ) |> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`
  ) |> 
  mutate(percent = n/sum(n)*100)
waltham_treatment

waltham_treatment_plot <- ggplot(waltham_treatment, aes(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`, 
                             percent, 
                             fill = `I am ___ at taking my HIV treatment compared to before I started coming to B&S`))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "I am ___ at taking my HIV treatment compared to before I started coming to B&S",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "PuBuGn")
waltham_treatment_plot

ggsave(filename = "waltham_treatment_plot.png", plot = waltham_treatment_plot,
       height = 4,
       width = 8) 
write.csv(waltham_treatment, "waltham_treatment.csv")

#Do you feel that you are able to manage your HIV medication
waltham$`Do you feel that you are able to manage your HIV medication`

waltham |> 
  count(`Do you feel that you are able to manage your HIV medication`) |> 
  mutate(percent = n/sum(n)*100)

waltham_two_vars <- waltham|> 
  count(`I am ___ at taking my HIV treatment compared to before I started coming to B&S`,
        `Do you feel that you are able to manage your HIV medication`,
        sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)
waltham_two_vars


write.csv(waltham_two_vars, "waltham_two_vars.csv")

#`Any other HIV organisations you attend, and where`
waltham$`Any other HIV organisations you attend, and where`
waltham <- waltham |> 
  rename(other_orgs = `Any other HIV organisations you attend, and where`)

waltham_otherorgs <- waltham|> 
  mutate(other_orgs= str_to_lower(other_orgs),
         other_orgs= str_replace_all(other_orgs, "fast track cities", "fast_track_cities"),
         other_orgs= str_replace_all(other_orgs, "positive east", "positive_east"))
waltham_otherorgs |> 
  select(other_orgs)

waltham_otherorgs <- waltham_otherorgs|> 
  select(other_orgs) |> 
  unnest_tokens(word, other_orgs)|> 
  mutate(word = case_when(word == "fast_track_cities" ~ "fast track cities",
                          word == "positive_east" ~ "positive east",
                          TRUE ~ word))|>
  anti_join(stop_words_with_yes_no)

waltham_otherorgs_percent <- waltham_otherorgs |> 
  filter(word == "positive east"|
           word == "fast track cities"|
           word == "no") |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) 
waltham_otherorgs_percent
waltham_otherorgs_plot <- waltham_otherorgs |> 
  filter(word == "positive east"|
           word == "fast track cities"|
           word == "no") |> 
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
  scale_fill_brewer(palette = "PuBuGn")
waltham_otherorgs_plot

ggsave(filename = "waltham_otherorgs_plot.png", plot = waltham_otherorgs_plot,
       height = 4,
       width = 8)

write.csv(waltham_otherorgs_percent, "waltham_otherorgs_percent.csv")

#Well-being Scale
WB_Scale_waltham <- as.data.frame(WB_Scale) |> 
  select(`I've been feeling optimistic about the future`:
           `I've been feeling cheerful`,
         Borough) |> 
  filter(Borough == "Waltham Forest")
WB_Scale_waltham

WB_Scale_waltham_plot <- plot(likert(WB_Scale_waltham[,1:14]), ordered = F)+
  labs(title = "Well Being Scale") +
  scale_fill_brewer(palette = "PuBuGn")

brewer.pal.info
WB_Scale_waltham_plot

ggsave(filename = "WB_Scale_waltham_plot.png", plot = WB_Scale_waltham_plot,
       height = 4,
       width = 8) 


#What services have you found most helpful
waltham$`What services have you found most helpful`
waltham_services <- waltham|> 
  select(`What services have you found most helpful`) |> 
  unnest_tokens(word,`What services have you found most helpful`)|> 
  anti_join(stop_words) |> 
  print(n = 24)

waltham_services <- waltham_services |> 
  mutate(word = str_replace_all(word, "therapies", "therapy"),
         word = str_replace_all(word, "therpay", "therapy"),
         word = str_replace_all(word, "workshops", "workshop"))

waltham_services |> 
  count(word, sort = TRUE)

waltham_services <- waltham_services |> 
  filter(word == "casework"|
           word == "food"|
           word == "workshop"|
           word == "therapy"|
           word == "immigration") |>  
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)
waltham_services
waltham_services_plot <- waltham_services |> 
  mutate(word = reorder(word, percent)) |> 
  ggplot(aes(word, percent, fill = word))+
  geom_col(show.legend = FALSE, alpha = 3/4) +
  labs(title = "What services have you found most helpful?",
       y = NULL,
       x = "%") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))+
  scale_fill_brewer(palette = "PuBuGn")
waltham_services_plot

ggsave(filename = "waltham_services_plot.png", plot = waltham_services_plot,
       height = 4,
       width = 8) 
write.csv(waltham_services, "waltham_services.csv")

#BS Scale
#convert to data frame - likert only
colnames(BS_Scale)
BS_Scale_dtf_waltham <- as.data.frame(BS_Scale) |> 
  select(`Has B&S helped you to feel less isolated or lonely`:
           `Has B&S helped you to cope and build your resilience`, Borough) |>
  filter(Borough == "Waltham Forest")
  
data.frame(BS_Scale_dtf_waltham)

#likert 
BS_likert_waltham <- plot(likert(BS_Scale_dtf[,1:9]), ordered = F)+
  labs(title = "Body and Soul Scale") +
  scale_fill_brewer(palette = "PuBuGn")
BS_likert_waltham

ggsave(filename = "BS_likert_waltham.png", plot = BS_likert_waltham ,
       height = 4,
       width = 8) 
waltham$`A way you would like to get more involved with B&S`
waltham$`Subjects/activities you would like to see in workshops`
