# Working file

setup_twitter_oauth(key, secret, atoken, asecret)

avail_trends <- availableTrendLocations()
avail_trends

globaltrends <- getTrends(1)
globaltrends
ustrends <- getTrends(23424977)
ustrends

#Pull Election Tweets

# New Data using #election2020 #2020election #election #vote #voting
hashelection2020 <- searchTwitter('#election2020', n=1000, since='2020-11-03', until='2020-11-04')
hash2020election <- searchTwitter('#2020election', n=1000, since='2020-11-03', until='2020-11-04')
hashvote <- searchTwitter('#vote', n=1000, since='2020-11-03', until='2020-11-04')

new <- c(hashelection2020, hash2020election, hashvote)

#Date Created
datecreated <- new %>% map(~.$created)
datecreated <- map(datecreated, ~data.frame(.))
datecreated <- map_dfr(datecreated, ~mutate_all(.,as_datetime))
datecreated <- rename(datecreated, date = .)

#Screen Name
screenname <- new %>% map(~.$screenName)
screenname <- map(screenname, ~data.frame(.))
screenname <- map_dfr(screenname, ~mutate_all(.,as.character()))
screenname <- rename(screenname, name = .)

#text
tweettext <- sapply(new,function(x) x$getText())
tweettext <- as.data.frame(tweettext)
tweettext <- rename(tweettext, text = tweettext)
tweettext %>%
  mutate(length = str_length(text)) -> tweettext

#favorite count
favoritecount <- new %>% map(~.$favoriteCount)
favoritecount <- map(favoritecount,~data.frame(.))
favoritecount <- map_dfr(favoritecount,~mutate_all(.,as.integer))
favoritecount <- rename(favoritecount, favorites = .)

#retweet count
retweetcount <- new %>% map(~.$retweetCount)
retweetcount <- map(retweetcount,~data.frame(.))
retweetcount <- map_dfr(retweetcount,~mutate_all(.,as.integer))
retweetcount <- rename(retweetcount, retweets = .)

#sources
statussources <- sapply(new,function(x) x$getStatusSource())
statussources <- gsub("</a>","",statussources)
statussources <- strsplit(statussources, ">")
statussources <- sapply(statussources, function(x) ifelse(length(x) > 1, x[2], x[1]))
statussources <- as.data.frame(statussources)
statussources <- rename(statussources, sources = statussources)

statussources %>%
  mutate(sources = as.character(sources)) %>%
  mutate(sources = case_when(str_detect(sources, "iPad") ~"iPad",
                             str_detect(sources, "iPhone") ~ "iPhone",
                             str_detect(sources, "Android") ~"Android",
                             str_detect(sources, "Web") ~"Web",
                             TRUE ~ sources))  -> statussources

statussources$sources = replace(x = statussources$sources, 
                                list =  !statussources$sources %in% c('iPad', 'iPhone', 'Android',
                                                                      'Web'),
                                values =  'others')

#tidy data frame
new <- cbind(datecreated, screenname, statussources, tweettext, favoritecount, retweetcount)

new %>%
  mutate(isretweeted = str_extract(text, "RT"),
         isretweeted = ifelse(isretweeted == "RT", TRUE, FALSE),
         text = str_replace_all(text, "RT\\s+", ""),
         candidate = "Biden") %>%
  select(date, candidate, name, sources, isretweeted, text, length, favorites, retweets) -> new

# ----------

#Combine

new.tidy <- new

new.tidy %>%
  mutate(sources = as.factor(sources)) ->
  new.tidy

#Save to CSV
write.csv(new.tidy, file = "new11032020.csv", row.names = FALSE)

# ----------

#Full training set
oct27 <- read.csv("new10272020.csv")
oct28 <- read.csv("new10282020.csv")
oct29 <- read.csv("new10292020.csv")
oct30 <- read.csv("new10302020.csv")
oct31 <- read.csv("new10312020.csv")
nov1 <- read.csv("new11012020.csv")
nov2 <- read.csv("new11022020.csv")
nov3 <- read.csv("new11032020.csv")

new.set <- rbind(oct27,oct28,oct29,oct30,oct31,nov1,nov2,nov3) %>%
  mutate(isretweeted = replace_na(isretweeted, FALSE))

# List of key topics

new.set %>%
  mutate(economy = str_count(str_to_sentence(text), c("economy", "market")),
         security = str_count(str_to_sentence(text), c("terrorism", "security", 
                                                       "national")),
         covid19 = str_count(str_to_sentence(text), c("covid", "virus", "pandemic", 
                                                      "response", "corona")),
         healthcare = str_count(str_to_sentence(text), c("medicare", "healthcare",
                                                         "medical")),
         education = str_count(str_to_sentence(text), c("education", "school", "student")), 
         race = str_count(str_to_sentence(text), c("racist", "racism")),
         gun = str_count(str_to_sentence(text), c("gun", "progun", "pro-gun", "2nd")), 
         crime = str_count(str_to_sentence(text), c("crime", "police", "brutal")),
         abortion = str_count(str_to_sentence(text), c("abortion", "prolife", "pro-life",
                                                       "prochoice", "pro-choice")),
         immigration = str_count(str_to_sentence(text), c("immigration", "border")),
         climate = str_count(str_to_sentence(text), c("climate", "environment",
                                                      "emission")),
         foreign = str_count(str_to_sentence(text), c("foreign", "world")),
         taxes = str_count(str_to_sentence(text), c("tax", "income")),
         deficit = str_count(str_to_sentence(text), c("budget", "deficit")),
         china = str_count(str_to_sentence(text), c("china", "chinese", "cyber")),
         russia = str_count(str_to_sentence(text), c("russia", "interference")),
         fakenews = str_count(str_to_sentence(text), c("fake")),
         info = str_count(str_to_sentence(text), c("misinformation", 
                                                   "disinformation"))) -> new.set

write.csv(new.set, file = "raw_new.csv", row.names = FALSE)

# ----------

#New set Sentiments

new.set <- read.csv("raw_new.csv")

str(new.set)

new.set %>%
  mutate(date = as_datetime(date),
         candidate = as.factor(candidate),
         sources = as.factor(sources)) -> new.sent

str(new.sent)

col.names <- names(new.sent)

#Sentiments Analysis
sentiments %>% arrange(word)
get_sentiments("afinn")

#NRC Sentiments
nrc_sent <- unique(get_sentiments("nrc")$sentiment)

nrc_words <- get_sentiments("nrc")$word

new.sent %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(word %in% nrc_words) -> new_df

new_df %>%
  inner_join(get_sentiments("nrc")) -> new_df

new_df %>%
  group_by(date, candidate, name, sources, isretweeted, length, favorites, retweets,
           economy, security, covid19, healthcare, education, race, gun, crime, abortion,
           immigration, climate, foreign, taxes, deficit, china, russia, fakenews, info,
           sentiment) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = sentiment, values_from = count) -> new_count

new_count[is.na(new_count)] <- 0

write.csv(new_count, file = "sent_new.csv", row.names = FALSE)
