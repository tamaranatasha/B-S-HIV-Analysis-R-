#ABOUT RELATIONSHIPS
#load data and align with below subsets
library(tidyverse)
library(SnowballC)
library(hunspell)
library(tidytext)
library(RColorBrewer)
library(tokenizers)
About_relationships_dta <- read_csv('../Data Analysis/Planning for the Future 2024 Data.csv')


About_relationships_dta <- About_relationships_dta |> 
  select(`Who or where do you turn to for support`:`Any family/friends/loved ones that you would want to share your HIV status to`)
About_relationships_dta
#main_support
support_parsed <- About_relationships_dta |> 
  mutate(`Who or where do you turn to for support`= str_to_lower(`Who or where do you turn to for support`),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`,"b&s", "body_soul"),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`, "support groups", "support_groups"),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`, "no one", "no_one"),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`, "social worker", "social worker"),
         `Who or where do you turn to for support`= str_replace_all(`Who or where do you turn to for support`, "support worker", "support_worker")) 


support_parsed |> 
  select(`Who or where do you turn to for support`) |> 
  print(n = 82)

support_tokenized <- support_parsed |> 
  select(`Who or where do you turn to for support`) |> 
  unnest_tokens(word, `Who or where do you turn to for support`) %>%
  mutate(word = case_when(word == "body_soul" ~ "body & soul",
                          word == "support_groups" ~ "support groups",
                          word == "no_one" ~ "no one",
                          word == "social_worker" ~ "social worker",
                          word == "support_worker" ~ "support worker",
                          TRUE ~ word))|> 
  anti_join(stop_words)

support_tokenized |> 
  count(word, sort = TRUE) |> 
  mutate(n/sum(n)*100)

support_tokenized |> 
  select(word) |> 
  print(n = 204)
#all words
support_tokenized |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  filter(word != "NA",
         word != "body",
         word != "soul")  |> 
  mutate(word = reorder(word, n),
             percent = n/sum(n)*100) |> 
  ggplot(aes(percent, word, fill = percent))+
  geom_col(show.legend = FALSE) +
  labs(title = "Who or where do you turn to for support?",
            y = NULL,
            x = "number of observations") +
  theme_gray(base_size = 5)+
  theme(
    plot.title = element_text(size = 6)) 
#bar plot percentage where n words > 1 
support_plot <- support_tokenized |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  filter(word != "NA",
         word != "body",
         word != "soul",
         word != "related",
         word != "hiv",
         word != "support",
         word != "worker",
         word != "talk",
         word != "social",
         word != "people",
         n>1)  |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word, fill = n))+
  geom_col(show.legend = FALSE, fill = "turquoise4", alpha = 3/4) +
  labs(title = "Who or where do you turn to for support?",
       y = NULL,
       x = "number of observations") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))

support_plot
ggsave(filename = "support_plot.png", plot = support_plot ,
       height = 4,
       width = 8)

#percent table n >1 
support_percent <- support_tokenized |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  filter(word != "NA",
         word != "body",
         word != "soul",
         word != "related",
         word != "hiv",
         word != "support",
         word != "worker",
         word != "talk",
         word != "social",
         word != "people",
         n>1) |> 
  mutate(percent = n/sum(n)*100)
support_percent

write_csv(support_percent, "support_percent.csv")


support_tokenized |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

#how to rotate x axis tick labels so they dont overlap
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#palettes
brewer.pal.info
  
stop_words_with_yes_no <- stop_words |> 
  filter(word != "no",
         word != "yes")

#Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`
About_relationships_dta <- About_relationships_dta |> 
  rename(outside_BS_support = `Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV`)

About_relationships_dta |> 
  select(outside_BS_support) |> 
  print(n = 82)

discuss_hiv <- About_relationships_dta %>%
  mutate(outside_BS_support = str_to_lower(outside_BS_support),
         outside_BS_support = str_replace_all(outside_BS_support, "b&s", "body_soul"),
         outside_BS_support = str_replace_all(outside_BS_support, "body & soul", "body_soul"))

discuss_hiv |> 
  select(outside_BS_support) |> 
  print(n = 82)

discuss_hiv <- discuss_hiv |> 
  select(outside_BS_support) |> 
  unnest_tokens(word, outside_BS_support) %>%
  mutate(word = case_when(word == "body_soul" ~ "body & soul",
                          TRUE ~ word))|> 
  anti_join(stop_words_with_yes_no)

discuss_hiv_percent <- discuss_hiv |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  print(n = 33)



#bar plot percentage where n words > 2 
discuss_hiv_plot <-  discuss_hiv |> 
   mutate(stem = wordStem(word)) |> 
   count(word, sort = TRUE) |> 
   filter(word != "NA",
         word != "hiv",
         word != "status",
         word != "close",
              n > 2)  |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word, fill = n))+
  geom_col(show.legend = FALSE, fill = "tomato2", alpha = 3/4) +
  labs(title = "Do you have family/friends/community spaces outside of B&S where you can openly talk about HIV?",
       y = NULL,
       x = "number of observations") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))

discuss_hiv_plot
ggsave(filename = "discuss_hiv_plot.png", plot = discuss_hiv_plot,
       height = 4,
       width = 8)

#percentages
discuss_percent <- discuss_hiv |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  filter(word != "NA",
         word != "hiv",
         word != "status",
         word != "close",
        n > 2) 
discuss_percent

discuss_hiv2 <- discuss_hiv |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  filter(word != "NA",
         word != "hiv",
         word != "status",
         word != "close",
         n > 2) 

discuss_hiv2

discuss_hiv_percent
write_csv(discuss_percent, "discuss_percent.csv")


#any family/friends/loved ones that you would want to share your HIV status to`
About_relationships_dta <- About_relationships_dta %>%
  mutate(`Any family/friends/loved ones that you would want to share your HIV status to` = str_to_lower(`Any family/friends/loved ones that you would want to share your HIV status to`),
         `Any family/friends/loved ones that you would want to share your HIV status to` = str_replace_all(`Any family/friends/loved ones that you would want to share your HIV status to`, "b&s", "body_soul"),
         `Any family/friends/loved ones that you would want to share your HIV status to` = str_replace_all(`Any family/friends/loved ones that you would want to share your HIV status to`, "body & soul", "body_soul"),
         `Any family/friends/loved ones that you would want to share your HIV status to` = str_replace_all(`Any family/friends/loved ones that you would want to share your HIV status to`, "mento", "mentor"))


About_relationships_dta$`Any family/friends/loved ones that you would want to share your HIV status to`

share_status <- About_relationships_dta |> 
  select(`Any family/friends/loved ones that you would want to share your HIV status to`) |> 
  unnest_tokens(word, `Any family/friends/loved ones that you would want to share your HIV status to`)|> 
  mutate(word = case_when(word == "body_soul" ~ "body & soul",
                          TRUE ~ word))|> 
  anti_join(stop_words_with_yes_no)
#plot
share_status_plot <- share_status |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  filter(n > 1,
         word != "shared",
         word != "future",
         word != "status",
         word != "hiv",
         word != "continue",
         word != "educate") |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word, fill = n))+
  geom_col(show.legend = FALSE, fill = "plum3", alpha = 3/4) +
  labs(title = "Any family/friends/loved ones that you would want to share your HIV status to",
       y = NULL,
       x = "number of observations") +
  theme_gray(base_size = 8)+
  theme(plot.title = element_text(size = 9))

#save plot
share_status_plot
ggsave(filename = "share_status.png", plot = share_status_plot,
       height = 4,
       width = 8)

#percentage and obvs
share_status |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100)

share_status_percent <- share_status |> 
  count(word, sort = TRUE) |> 
  mutate(percent = n/sum(n)*100) |> 
  filter(n > 1,
         word != "shared",
         word != "future",
         word != "status",
         word != "hiv",
         word != "continue",
         word != "educate") 

share_status_percent
write_csv(share_status_percent, "share_status.csv")

#correlation
#comparing association of word frequencies across relationships
share_status
support_tokenized
discuss_hiv
frequency_relationships <- bind_rows(mutate(share_status, Category = "share status"),
                       mutate(discuss_hiv, Category = "discuss out of b&s")) %>%
  count(Category, word) %>%
  group_by(Category) %>%
  mutate(prop = n / sum(n)) %>% 
  select(-n) %>%
  pivot_wider(names_from = Category, values_from = prop)

frequency_relationships |> 
  print(n = 60)


frequency_relationships_noNA <- frequency_relationships  |> 
  filter( `discuss out of b&s` != "NA",
          `share status`!= "NA")
frequency_relationships_noNA
write_csv(frequency_relationships_noNA, "frequencyrelationships.csv")


relationships_plot <- ggplot(frequency_relationships, aes(x =`discuss out of b&s`, y =  `share status`, 
                                           color = abs(`discuss out of b&s` - `share status`))) +
  geom_abline(color = "darkgrey", lty = 2) +
  geom_jitter(alpha = 0.7, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.01), 
                       low = "darkslategray3", high = "darkgrey") +
  theme(legend.position = "none") +
  labs(x = "Any family/friends/community spaces outside of B&S where you can openly talk about HIV?",
       y = "Anyone that you would want to share your HIV status to")

relationships_plot
ggsave(filename = "relationships_plot.png", plot = relationships_plot,
       height = 6,
       width = 8)

#cor.test() test for association/correlation between paired samples. It returns both the correlation coefficient 
#and the significance level(or p-value) of the correlation .
cor.test(frequency_relationships$`discuss out of b&s`, frequency_relationships$`share status`)
#t = 11.279, df = 13, p-value = 4.377e-08
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
 # 0.8597701 0.9844355
#sample estimates:
 # cor 
#0.9525174
