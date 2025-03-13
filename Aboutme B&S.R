#ABOUT ME
library(tidyverse)
library(SnowballC)
library(hunspell)
library(tidytext)
library(RColorBrewer)
library(tidyr)
library(scales)

#About me: gender, borough, #3 strengths, hobbies, 
#biggest issue, 6-month goal.
BS_HIV_dta <- read_csv('../Data Analysis/Planning for the Future 2024 Data.csv'
)


About_me_dta <- BS_HIV_dta |> 
  rename(strengths = `3 strengths`,
         biggest_issue = `biggest issue currently`,
         six_month_goal = `goal over the next 6 months and how could B&S support`)

colnames(About_me_dta)

About_me_dta <- About_me_dta |> 
  select(Gender,
         Borough,  
         strengths, 
         hobbies,
         biggest_issue,
         six_month_goal)

about_me_funded_boroughs <- About_me_dta |> 
filter(Borough %in% c("Newham", "Camden and Islington", "Hackney",
                      "Waltham Forest"))


tibble(About_me_dta)
tibble(about_me_funded_boroughs)
data(stop_words)


#3 STRENGTHS
#tokenization for sentiment analysis
About_me_dta <- About_me_dta |> 
  mutate(strengths = str_to_lower(strengths),  
         strengths = str_replace_all(strengths,
                                   "listener", 
                                   "listening"))
About_me_dta |> 
  select(strengths) |> 
  print(n = 82)

strengths_df <- About_me_dta |> 
  select(strengths) |> 
  unnest_tokens(word, strengths)|> 
  anti_join(stop_words)

strengths_df

#counts of strengths words 
strengths_df |> 
  count(word, sort = TRUE) |> 
  print(n = 146)

#stemming
#regex str detect
strengths_df %>%
  count(word, sort = TRUE) %>%
  filter(str_detect(word, "^cook"))

#word stem 
strengths_df_stem <- strengths_df %>%
  mutate(stem = wordStem(word)) %>%
  count(word, sort = TRUE)

strengths_df

#plot word freqs with wordstem 
strengths_df_stem  |>
  filter(word != "time",
         word != "keeping",
         word != "body",
         n > 2)|> 
  ggplot(aes(n, fct_reorder(word, n))) +
  geom_col(fill = "coral",
           show.legend = FALSE,
           width = 0.8,
           alpha = 3/4) +
  labs(title = "Strengths n>2", 
       x = "Word frequency",
       y = NULL) +
  theme_gray(base_size = 8,) +
  theme(plot.title = element_text(size = 9))

#display palettes
display.brewer.all()
brewer.pal.info
display.brewer.pal

# viz using word stem and strengths where n > 2
strengths_barplot <- strengths_df_stem |> 
  filter(word != "keep",
         word != "keeping",
         word != "meeting",
         word != "time",
          n > 2) |> 
  ggplot(aes(n, fct_reorder(word, n))) +
   geom_col(show.legend = FALSE, 
            fill = "coral", 
            alpha = 3/4,
            width = 0.8) +
   labs(y = NULL, title = "Strengths",                     
        x = "Word Frequency") +
  theme_gray(base_size = 8) +
  theme(plot.title = element_text(size = 9))
 strengths_barplot
 ggsave(filename = "strengths.png", plot = strengths_barplot,
        height = 4,
        width = 8) 
 
#percentage of strengths where n > 2 stop words removed 
strengths_percent <- strengths_df_stem |>
  
   filter(word != "keep",
          word != "keeping",
          word != "meeting",
          word != "time",
          n > 2) |> 
   mutate(percent = n/sum(n)*100)
strengths_percent
write_csv(strengths_percent, "strengths_percent.csv")


#hobbies - freqs
 About_me_dta <- About_me_dta |> 
   mutate(hobbies = str_to_lower(hobbies),  
          hobbies = str_replace_all(hobbies,
                                    "exercising", 
                                    "exercise"))
#tokenization
hobbies_df <- About_me_dta |> 
  select(hobbies) |> 
  unnest_tokens(word, hobbies) |> 
  anti_join(stop_words)

hobbies_df

#hobbies - word stems and counts  
hobbies_df |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE)


hobbies_df %>%
  mutate(stem = hunspell_stem(word)) %>%
  unnest(stem) |> 
  count(stem, sort = TRUE)

#number of word frequency is greater than 2 
hobbies_barplot <- hobbies_df |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  filter(word != "watching",
         word != "keeping",
         word != "time",
         word != "meeting",
         word != "taking",
         word != "listening",
         n > 2) |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word)) +
  geom_col(show.legend = FALSE, 
           fill = "hotpink2",
           alpha = 3/4,
           width = 0.8) +
  labs(title = "Hobbies", y = NULL, x = "Word frequency") +
  theme_gray(base_size = 8) +
  theme(
    plot.title = element_text(size = 9))

hobbies_barplot
ggsave(filename = "hobbies.png", plot = hobbies_barplot,
       height = 4,
       width = 8) 

hobbies_percentage <- hobbies_df |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  filter(word != "watching",
         word != "keeping",
         word != "time",
         word != "meeting",
         word != "taking",
         word != "listening",
         n > 2) |> 
  mutate(percent = n/sum(n)*100)
hobbies_percentage

write_csv(hobbies_percentage, "hobbies_percentage.csv")

#biggest issue 
issue_df <- About_me_dta |> 
  select(biggest_issue) |> 
  unnest_tokens(word, biggest_issue) |> 
  anti_join(stop_words) 
issue_df

issue_df_stem <- issue_df |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) 
issue_df_stem  
  
issues_barplot <- issue_df_stem |> 
  filter(n > 2,
         word != "issues",
         word != "challenges",
         word != "due",
         word != "lack") |> 
  mutate(word = reorder(word, n )) |> 
  ggplot(aes(n, word)) +
  geom_col(show.legend = FALSE, 
           fill = "cornflowerblue", 
           alpha = 0.8,
           width = 0.8) +
  labs(y = NULL, title = "Biggest issue currently", x = "Word frequency") +
  theme_gray(base_size = 8)+
  theme(
    plot.title = element_text(size = 9))
issues_barplot 
ggsave(filename = "issues.png", plot = issues_barplot,
       height = 4,
       width = 8) 

issue_percent <- issue_df_stem |> 
  filter(n > 2,
         word != "issues",
         word != "challenges",
         word != "due",
         word != "lack") |> 
  mutate(percent = n/sum(n)*100)
issue_percent
write_csv(issue_percent, "issue_percent.csv")


#six_month_goal frequencies 
#replace healthy with health to condense similar words
About_me_dta <- About_me_dta |> 
    mutate(six_month_goal = str_to_lower(six_month_goal),  
          six_month_goal = str_replace_all(six_month_goal,
                                           "healthy", "health"))
           
goals_df <- About_me_dta |> 
  select(six_month_goal) |> 
  unnest_tokens(word, six_month_goal)|> 
  anti_join(stop_words)

goals_df |>   
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE)
  

goals_df

goals_barplot <- goals_df |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  filter(n > 2, 
         word != 'finding',
         word != 'support',
         word != 'call',
         word != 'issue') |> 
  mutate(word = reorder(word, n )) |> 
  ggplot(aes(n, word)) +
  geom_col(fill = "darkgreen", alpha = 1/2, width = 0.8) +
  labs(y = NULL, 
       title = "Goal over the next 6 months and how could B&S support",
       x = "Word frequency") +
  theme_gray(base_size = 8)+
  theme(
    plot.title = element_text(size = 9))

goals_barplot
ggsave(filename = "goals.png", plot = goals_barplot,
       height = 4,
       width = 8)

goals_percent <- goals_df |>
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  filter(n > 2, 
         word != 'finding',
         word != 'support',
         word != 'call',
         word != 'issue') |> 
  mutate(percent = n/sum(n)*100)
goals_percent
write_csv(goals_percent, "goals_percent.csv")
#How correlated are the word frequencies
#comparing association of word frequencies across hobbies and strengths
frequency <- bind_rows(mutate(hobbies_df, Category = "Hobbies"),
                       mutate(strengths_df, Category = "Strengths")) %>%
  count(Category, word) %>%
  group_by(Category) %>%
  mutate(prop = n / sum(n)) %>% 
  select(-n) %>%
  pivot_wider(names_from = Category, values_from = prop)
  frequency
frequency_not_NA <- frequency |> 
  filter(Hobbies != "NA",
         Strengths != "NA") |> 
  print(n = 40)
write_csv(frequency_not_NA, "frequency_not_NA.csv")
#Interpretation of Pearson Correlation Coefficient 
#The value of the correlation coefficient (r)

# How correlated are the word frequencies between strengths and hobbies
#t = 3.0261, df = 38, p-value = 0.004429, cor 0.4406701 
#r=0.30 to 0.50; moderate correlation
cor.test(frequency$Hobbies, frequency$Strengths)

# expect a warning about rows with missing values being removed
strength_hobbies_plot <- ggplot(frequency, aes(x = Hobbies, y = Strengths, 
                      color = abs(Strengths - Hobbies))) +
  geom_abline(color = "darkgrey", lty = 2) +
  geom_jitter(alpha = 0.7, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.04), 
                       low = "darkslategray3", high = "darkgrey") +
  theme(legend.position="none") +
  labs(y = "Strengths", x = "Hobbies")

strength_hobbies_plot
ggsave(filename = "str_hob.png", plot = strength_hobbies_plot,
       height = 4,
       width = 8)

#SAVE AS TABLES EXPORT
hobbies_percentage <- as.data.frame(hobbies_percentage)
hobbies_percentage
strengths_percent <- as.data.frame(strengths_percent)
strengths_percent
issue_percent <- as.data.frame(issue_percent)
issue_percent
goals_percent <- as.data.frame(goals_percent)
goals_percent

write_csv(hobbies_percentage, "hobbies.csv")
write.csv(strengths_percent, "strengths.csv")
write.csv(issue_percent, "issue.csv")
write.csv(goals_percent, "goal.csv")
hobbies_df
strengths_df

#association between issues and goals
issue_df
goals_df

frequency2 <- bind_rows(mutate(goals_df, Category = "6 Month Goal"),
                       mutate(issue_df, Category = "Biggest Issue")) %>%
  count(Category, word) %>%
  group_by(Category) %>%
  mutate(prop = n / sum(n)) %>% 
  select(-n) %>%
  pivot_wider(names_from = Category, values_from = prop)

frequency2 |> 
  print(n = 300)
write_csv(frequency2NA, "frequency2NA.csv")

frequency2NA <- frequency2|> 
  filter(`6 Month Goal`!= "NA", 
         `Biggest Issue`!= "NA") |> 
  print(n = 39)


#plot word frequencies

#Words that are close to the line in these plots have similar 
#frequencies in both sets of texts

#Words that are far from the line are words that 
#are found more in one set of texts than another
issue_goals_plot <- ggplot(frequency2, aes(x =`6 Month Goal`, y = `Biggest Issue`, 
                      color = abs(`6 Month Goal` - `Biggest Issue`))) +
  geom_abline(color = "darkgrey", lty = 2) +
  geom_jitter(alpha = 0.7, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.03), 
                       low = "darkslategray3", high = "darkgrey") +
  theme(legend.position="none") +
  labs(x = "6 Month Goal", y= "Biggest Issue")


issue_goals_plot
ggsave(filename = "iss_gol.png", plot = issue_goals_plot,
       height = 4,
       width = 8)
# How correlated are the word frequencies between
#issues and goals

#cor.test() test for association/correlation between paired samples. It returns both the correlation coefficient 
#and the significance level(or p-value) of the correlation .
cor.test(frequency2$`6 Month Goal`, frequency2$`Biggest Issue`)
#t = 6.3334, df = 37, p-value = 2.224e-07, cor = 0.7212315 
#r=0.50 to 1 highly correlated.

