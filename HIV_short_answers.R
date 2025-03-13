#ABOUT ME

library(tidyverse)
library(SnowballC)
library(hunspell)
library(tidytext)
library(RColorBrewer)

#About me: gender, borough, #3 strengths, hobbies, 
#biggest issue, 6-month goal.
About_me_dta <- read_csv('../Data Analysis/Planning for the Future 2024 Data.csv'
)


About_me_dta <- About_me_dta |> 
  rename(strengths = `3 strengths`,
         biggest_issue = `biggest issue currently`,
         six_month_goal = `goal over the next 6 months and how could B&S support`)


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
strengths_df <- About_me_dta |> 
  select(strengths, Gender, Borough) |> 
  unnest_tokens(word, strengths)|> 
  anti_join(stop_words)
strengths_df

#counts of strengths words 
strengths_df |> 
  count(word, sort = TRUE)

#stemming
#regex str detect
strengths_df %>%
  count(word, sort = TRUE) %>%
  filter(str_detect(word, "^cook"))

#word stem 
strengths_df %>%
  mutate(stem = wordStem(word)) %>%
  count(word, sort = TRUE)

#plot word freqs with wordstem 
strengths_df %>%
  mutate(stem = wordStem(word)) %>%
  count(word, sort = TRUE) |>
  filter(word != "time",
         word != "keeping",
         word != "body") |>
  top_n(20, n) |> 
  ggplot(aes(n, fct_reorder(word, n), fill = word)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Common strengths among respondents: Top 20", 
       x = NULL,
       y = NULL) +
  theme_gray(base_size = 8,) +
  theme(plot.title = element_text(size = 9))

#using hunspell word stem 
strengths_df %>%
  mutate(stem = hunspell_stem(word)) %>%
  unnest(stem) %>%
  count(word, Gender, sort = TRUE) |>
  mutate(percent = n/sum(n)*100)

#faceted by Gender
strengths_df %>%
  mutate(stem = hunspell_stem(word)) %>%
  unnest(stem) %>%
  count(word, Gender, sort = TRUE) |>
  mutate(percent = n/sum(n)*100) |> 
  filter(word != "keep",
         word != "keeping",
         word != "meet",
         word != "time") |> 
  top_n(25, percent) |> 
  ggplot(aes(percent, fct_reorder(word, percent))) +
  geom_col(aes(fill = Gender), show.legend = FALSE) +
  labs(title = "Strengths - Word frequencies", 
         x = "%",
         y = NULL) +
  theme_gray(base_size = 5,) +
  theme(plot.title = element_text(size = 6)) +
  facet_grid(~Gender) +
  scale_fill_brewer(palette = "Accent")

#display palettes
display.brewer.all()

#a simple viz using hunspell word stem and strengths 
 strengths_df %>%
  mutate(stem = hunspell_stem(word)) %>%
  unnest(stem) %>%
  count(word, sort = TRUE) |> 
  filter(word != "keep",
         word != "keeping",
         word != "meet",
         word != "time") |> 
  top_n(25, n) |> 
  ggplot(aes(n, fct_reorder(word, n), fill = word)) +
   geom_col(show.legend = FALSE) +
   labs(y = NULL, title = "Strengths - Word frequencies",                     
        x = NULL) +
  theme_gray(base_size = 8,) +
  theme(plot.title = element_text(size = 9))
  
  
#hobbies - freqs
#tokenization
hobbies_df <- About_me_dta |> 
  select(hobbies, Gender, Borough) |> 
  unnest_tokens(word, hobbies) |> 
  anti_join(stop_words)
hobbies_df

#hobbies - funded boroughs  
hobbies_df |> 
  mutate(stem = wordStem(word)) |> 
  count(word, stem, sort = TRUE)

hobbies_df |> 
  mutate(stem = wordStem(word)) |> 
  filter(Borough %in% c("Newham", "Camden and Islington", "Hackney",
                         "Waltham Forest")) |> 
  count(word, sort = TRUE, Borough) |> 
  top_n(15, n) |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word)) +
  geom_col(aes(fill = Borough), show.legend = FALSE) +
  labs(title = "Hobbies", y = NULL) +
  theme_gray(base_size = 4 )+
  theme(plot.title = element_text(size = 9)) +
  facet_grid(~Borough) +
  scale_fill_brewer(palette = "Set2")
  

hobbies_df %>%
  mutate(stem = hunspell_stem(word)) %>%
  unnest(stem) |> 
  count(stem, sort = TRUE)

hobbies_df |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE) |> 
  filter(word != "watching",
         word != "keeping",
         word != "time",
         word != "meeting") |> 
  top_n(20, n) |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word, fill = word))+
  geom_col(show.legend = FALSE) +
  labs(title = "Common hobbies among respondents", y = NULL) +
  theme_gray(base_size = 7) +
  theme(
    plot.title = element_text(size = 9))

hobbies_df |> 
  count(word, sort = TRUE) |> 
  filter(n > 5, 
         word != "listening") |> 
  mutate(word = reorder(word, n )) |> 
  ggplot(aes(n, word)) +
  geom_col(aes(fill = word)) +
  labs(y = NULL, title = "Hobbies") +
  scale_fill_brewer(palette = "Set3")

#biggest issue 
issue_df <- About_me_dta |> 
  select(biggest_issue, Borough, Gender) |> 
  unnest_tokens(word, biggest_issue) |> 
  anti_join(stop_words) 

#tokenisation filtered by specified funded boroughs 
issue_df_funded_borough <- about_me_funded_boroughs |> 
  select(biggest_issue, Borough, Gender) |> 
  unnest_tokens(word, biggest_issue) |> 
  anti_join(stop_words)

issue_df_funded_borough |> 
  count(word, sort = TRUE, Borough)

issue_df_funded_borough |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE, Borough) |> 
  top_n(20, n) |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word)) +
  geom_col(aes(fill = Borough), show.legend = FALSE) +
  labs(title = "Biggest issues currently", y = NULL) +
  theme_gray(base_size = 4 )+
  theme(plot.title = element_text(size = 9)) +
  facet_grid(~Borough) +
  scale_fill_brewer(palette = "Set2")
  

issue_df_stem <- issue_df |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE, Borough) 
  
  
issue_df |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE, Gender) |> 
  top_n(25, n) |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(n, word))+
  geom_col(aes(fill = Gender), show.legend = FALSE) +
  labs(title = "Biggest issues currently", y = NULL) +
  theme_gray(base_size = 7)+
  theme(plot.title = element_text(size = 9)) +
  facet_grid(~Gender) +
  scale_fill_brewer(palette = "Set2")

issue_df_stem|> 
  filter(Borough %in% c("Newham", "Camden and Islington", "Hackney",
                        "Waltham Forest")) |> 
  count(Borough) 
 
issue_df |> 
  count(word, sort = TRUE) |> 
  filter(n > 1,
         word != "issue",
         word != "challenges",
         word != "due",
         word != "lack") |> 
  mutate(word = reorder(word, n )) |> 
  ggplot(aes(n, word, fill = word)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL, title = "Biggest issue currently") +
  theme_gray(base_size = 7)+
  theme(
    plot.title = element_text(size = 9))
 

#six_month_goal freqs
goals_df <- About_me_dta |> 
  select(six_month_goal, Borough, Gender) |> 
  unnest_tokens(word, six_month_goal) |> 
  anti_join(stop_words)

goals_df

goals_df |> 
  count(word, sort = TRUE)

goals_df |> 
  count(word, sort = TRUE) |> 
  filter(n > 2, 
         word != 'finding',
         word != 'support') |> 
  mutate(word = reorder(word, n )) |> 
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL, title = "Goal over the next 6 months and how could B&S support")


#SENTIMENT ANALYSIS
strengths_sentiment <- BS_HIV_dta |>
  mutate(line_number = row_number()) |> 
  unnest_tokens(word, `3 strengths`) 

BS_mean_to_you <- BS_HIV_dta |> 
  mutate(line_number = row_number()) |> 
  unnest_tokens(word, `What B&S means to you`)

BS_mean_to_you

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

#count words with positive sentiment 
strengths_sentiment |> 
  select(word) |> 
  inner_join(nrc_joy) |> 
  count(word, sort = TRUE) |> 
  print(n = 27)

BS_mean_to_you |> 
  select(word) |> 
  inner_join(nrc_joy) |> 
  count(word, sort = TRUE)

#positive or negative sentiment
BS_mean_to_you |> 
  inner_join(get_sentiments("bing")) |> 
  count(word, line_number, sentiment) |> 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |> 
  mutate(sentiment = positive - negative) |> 
  print(n = 27)

strengths_sentiment |> 
  inner_join(get_sentiments("bing")) |> 
  count(word, line_number, sentiment) |> 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |> 
  mutate(sentiment = positive - negative) |> 
  print(n = 27)

bing_word_counts_str <- strengths_sentiment %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts_BS <- BS_mean_to_you %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts_str
bing_word_counts_BS

bing_word_counts_str %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

bing_word_counts_BS %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL,
       title = "What does B&S mean to you")

#Wordclouds
library(wordcloud)

BS_mean_to_you %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

strengths_sentiment %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#looking beyond words
p_and_p_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

strengths_sentences <- BS_HIV_dta |> 
  unnest_tokens(sentence, `3 strengths`, token = "sentences")
strengths_sentences |> 
  select(sentence)
#token by commas 
issues_regex <- HIV_dta_short |> 
unnest_tokens(word, biggest_issue, token = 'regex', pattern=",") |> 
  select(word) |> 
  anti_join(stop_words)

issues_regex |> 
  count(word, sort = TRUE) 

issues_regex |> 
  mutate(stem = wordStem(word)) |> 
  count(word, sort = TRUE)
  
#tf-idf - term frequency

strengths_df |> 
  count(word, sort = TRUE)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

issues_location <- HIV_dta_short |> 
  unnest_tokens(word, biggest_issue) |>
  count(Borough, word, sort = TRUE)

issues_location

issues_words <- HIV_dta_short |> 
  unnest_tokens(word, biggest_issue) |>
  count(Gender, word, sort = TRUE)
issues_words
  
issues_tf_idf <- issues_words %>%
    bind_tf_idf(word, Gender, n)

issues_tf_idf

issues_tf_idf %>%
  group_by(Gender) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Gender)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Gender, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#relationships between words - n-grams

issues_bigrams <- HIV_dta_short %>%
  unnest_tokens(bigram, biggest_issue, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))
issues_bigrams |> 
  select(bigram)

issues_bigrams %>%
  count(bigram, sort = TRUE)

