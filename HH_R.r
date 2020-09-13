library(tidyverse)

#READING THE DATA
polls <- readr::read_csv('C:/Users/sammy/Documents/HipHopR_S/polls.csv')
rankings <- readr::read_csv('C:/Users/sammy/Documents/HipHopR_S/ranking.csv')


##EDA
polls

rankings

polls %>% 
  count(title, sort = TRUE)

polls %>% 
  count(year) %>% 
  mutate(decade = floor(year / 10)*10) %>% 
  mutate(decade = as.factor(decade)) %>% 
  ggplot(aes(x = year, y = n, fill = decade)) + geom_col() 

polls %>% 
  count(year) %>% 
  mutate(decade = floor(year / 10)*10) %>% 
  mutate(decade = as.factor(decade)) %>% 
  ggplot(aes(x = decade, y = n, fill = decade)) + geom_col() 

polls %>% 
  count(artist, sort = TRUE) %>% 
  ggplot(aes(x = n)) + geom_density()

rankings %>% 
  select(artist, n, n1, n2, n3, n4, n5) %>% 
  group_by(artist) %>% 
  summarise_all(sum) %>% 
  filter(!str_detect(artist, "ft.")) %>% 
  ggplot(aes(x = n1, y = n5)) + geom_jitter() 

rankings %>% 
  select(artist, n, n1, n2, n3, n4, n5) %>% 
  group_by(artist) %>% 
  summarise_all(sum) %>% 
  filter(!str_detect(artist, "ft.")) %>% 
  arrange(desc(n1)) %>% 
  slice(1:5)

rankings %>% 
  select(artist, n, n1, n2, n3, n4, n5) %>% 
  group_by(artist) %>% 
  summarise_all(sum) %>% 
  filter(!str_detect(artist, "ft.")) %>% 
  arrange(desc(n5)) %>% 
  slice(1:5)

polls %>% 
  count(title, critic_country, name = "song_nom") %>% 
  add_count(title, name = "number_of_countries") %>% 
  filter(number_of_countries == 1 & critic_country != "US") %>% 
  nrow() / nrow(polls)

polls %>% 
  count(title, critic_country, name = "song_nom_country") %>% 
  add_count(title, name = "number_of_countries") %>% 
  filter(number_of_countries != 1) %>% 
  select(-number_of_countries) %>% 
  pivot_wider(names_from = "critic_country", values_from = "song_nom_country")


#Startingwith the recommendation engine

library(recommenderlab)

rap_matrix <- polls %>% 
  select(critic_name, title) %>% 
  mutate(n = 1) %>% 
  arrange(title) %>% 
  pivot_wider(names_from = "title", values_from = "n", values_fill = list(n = 0)) %>% 
  select(-critic_name) %>% 
  as.matrix() %>% 
  as("binaryRatingMatrix")

training_schema <- evaluationScheme(rap_matrix, method = "split", train = .8, given = -1)
training_schema

UBCF_Model <- evaluate(training_schema, method = "UBCF", type = "topNList", n = 5)

IBCF_Model <- evaluate(training_schema, method = "IBCF", type = "topNList", n = 5)

UBCF_Model %>% avg()

IBCF_Model %>% avg() %>% as_tibble()

tune_engines <- function(schema, parameters){
  
  UBCF_Model <- evaluate(schema, method = "UBCF", type = "topNList", n = 5, param = list(nn = parameters))
  IBCF_Model <- evaluate(schema, method = "IBCF", type = "topNList", n = 5, param = list(k = parameters))
  
    UBCF_Model %>% 
    avg() %>% 
    as_tibble() %>% 
    mutate(model = "UBCF") %>% 
    rbind(IBCF_Model %>% 
            avg() %>% 
            as_tibble() %>% 
            mutate(model = "IBCF")) %>% 
    return()
  
  
  
}


tune_grid <- tibble(parameters = c(2, 3, 5, 10, 15, 20, 25))

history <- tune_grid %>% 
  mutate(results = map(parameters, ~tune_engines(training_schema, .x))) %>% 
  unnest()

history %>% 
  ggplot(aes(x = parameters, y = TPR, fill = model, label = parameters)) + geom_col(position = "dodge") + geom_text(aes(x = parameters, y = TPR))


UBCF_Final_model <- Recommender(getData(training_schema, "train"), "UBCF", param = list(nn = 5))

UBCF_Final_model

predictions <- predict(UBCF_Final_model, getData(training_schema, "known"), type = "topNList")
calcPredictionAccuracy(predictions, getData(training_schema,"unknown"), given = -1)

rec_engine <- Recommender(rap_matrix, "UBCF", param = list(nn = 5))
rec_engine

polls %>% filter(str_detect(artist, "2Pac")) %>% distinct(title) %>% arrange(title)

#GIVING Songs via which recommendation would be given
andrew_songs <- polls %>% 
  select(title) %>% 
  distinct() %>% 
  arrange(title) %>% 
  filter(title %in% c("All Of The Lights", "Alright", "Bitch Don’t Kill My Vibe", "m.A.A.d. city", "Changes")) %>%  
  rbind(polls %>% select(title) %>% distinct()) %>% 
  count(title) %>% 
  mutate(n = n -1) %>% 
  pivot_wider(names_from = "title", values_from = "n", values_fill = list(n = 0)) %>% 
  as.matrix() %>% 
  as("binaryRatingMatrix")


rec_engine

predict(rec_engine, andrew_songs) %>% as("list") %>% as.data.frame()





