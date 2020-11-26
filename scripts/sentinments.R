full.set <- read.csv("raw_set.csv")

str(full.set)

full.set %>%
  mutate(date = as_datetime(date),
         candidate = as.factor(candidate),
         sources = as.factor(sources)) -> election.sent

str(election.sent)

col.names <- names(election.sent)

#Sentiments Analysis
sentiments %>% arrange(word)
get_sentiments("afinn")

#NRC Sentiments
nrc_sent <- unique(get_sentiments("nrc")$sentiment)

nrc_words <- get_sentiments("nrc")$word

election.sent %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(word %in% nrc_words) -> text_df

text_df %>%
  inner_join(get_sentiments("nrc")) -> text_df

text_df %>%
  group_by(date, candidate, name, sources, isretweeted, length, favorites, retweets,
           economy, security, covid19, healthcare, education, race, gun, crime, abortion,
           immigration, climate, foreign, taxes, deficit, china, russia, fakenews, info,
           sentiment) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = sentiment, values_from = count) -> sent_count

sent_count[is.na(sent_count)] <- 0

write.csv(sent_count, file = "sent_training.csv", row.names = FALSE)

#BY DEVICES/SOURCES
sent_count %>%
  group_by(date, sources) %>%
  summarize(length_average = mean(length),
            favorites_total = sum(favorites),
            retweets_total = sum(retweets)) -> devices_covsent

devices_covsent %>%
  inner_join(sent_count, by = c("date" = "date", "sources" = "sources")) -> devices_covsent

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#FF9999")

devices_covsent %>%
  group_by(sources) %>%
  summarize(device_retw = sum(retweets_total)) %>%
  mutate(prop = round(device_retw/sum(device_retw),2)*100) %>%
  arrange(desc(sources)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)-> pie

pie %>%
  ggplot(aes(x = 2, y = prop, fill = sources)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(y = lab.ypos, label = paste(sources,":", prop,"%", sep = "")), color = "black") +
  scale_fill_manual(values = mycols) +
  theme_void() +
  theme(legend.position = "none") +
  xlim(0.5, 2.5)