# Working file

setup_twitter_oauth(key, secret, atoken, asecret)

avail_trends <- availableTrendLocations()
avail_trends

globaltrends <- getTrends(1)
globaltrends
ustrends <- getTrends(23424977)
ustrends

#Pull Election Tweets

# Training Set
# Biden
hashbiden <- searchTwitter('#biden2020', n=1000, since='2020-11-01', until='2020-11-02')
hashbidenharris <- searchTwitter('#BidenHarris2020', n=1000, since='2020-11-01', until='2020-11-02')

biden <- c(hashbiden, hashbidenharris)

#Date Created
datecreated <- biden %>% map(~.$created)
datecreated <- map(datecreated, ~data.frame(.))
datecreated <- map_dfr(datecreated, ~mutate_all(.,as_datetime))
datecreated <- rename(datecreated, date = .)

#Screen Name
screenname <- biden %>% map(~.$screenName)
screenname <- map(screenname, ~data.frame(.))
screenname <- map_dfr(screenname, ~mutate_all(.,as.character()))
screenname <- rename(screenname, name = .)

#text
tweettext <- sapply(biden,function(x) x$getText())
tweettext <- as.data.frame(tweettext)
tweettext <- rename(tweettext, text = tweettext)
tweettext %>%
  mutate(length = str_length(text)) -> tweettext

#favorite count
favoritecount <- biden %>% map(~.$favoriteCount)
favoritecount <- map(favoritecount,~data.frame(.))
favoritecount <- map_dfr(favoritecount,~mutate_all(.,as.integer))
favoritecount <- rename(favoritecount, favorites = .)

#retweet count
retweetcount <- biden %>% map(~.$retweetCount)
retweetcount <- map(retweetcount,~data.frame(.))
retweetcount <- map_dfr(retweetcount,~mutate_all(.,as.integer))
retweetcount <- rename(retweetcount, retweets = .)

#sources
statussources <- sapply(biden,function(x) x$getStatusSource())
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
biden <- cbind(datecreated, screenname, statussources, tweettext, favoritecount, retweetcount)

biden %>%
  mutate(isretweeted = str_extract(text, "RT"),
         isretweeted = ifelse(isretweeted == "RT", TRUE, FALSE),
         text = str_replace_all(text, "RT\\s+", ""),
         candidate = "Biden") %>%
  select(date, candidate, name, sources, isretweeted, text, length, favorites, retweets) -> biden

# ----------

# Trump
hashtrumppence <- searchTwitter('#TrumpPence2020', n=1000, since='2020-11-01', until='2020-11-02')
hashtrump <- searchTwitter('#trump2020', n=1000, since='2020-11-01', until='2020-11-02')

trump <- c(hashtrumppence, hashtrump)

#Date Created
datecreated <- trump %>% map(~.$created)
datecreated <- map(datecreated, ~data.frame(.))
datecreated <- map_dfr(datecreated, ~mutate_all(.,as_datetime))
datecreated <- rename(datecreated, date = .)

#Screen Name
screenname <- trump %>% map(~.$screenName)
screenname <- map(screenname, ~data.frame(.))
screenname <- map_dfr(screenname, ~mutate_all(.,as.character()))
screenname <- rename(screenname, name = .)

#text
tweettext <- sapply(trump,function(x) x$getText())
tweettext <- as.data.frame(tweettext)
tweettext <- rename(tweettext, text = tweettext)
tweettext %>%
  mutate(length = str_length(text)) -> tweettext

#favorite count
favoritecount <- trump %>% map(~.$favoriteCount)
favoritecount <- map(favoritecount,~data.frame(.))
favoritecount <- map_dfr(favoritecount,~mutate_all(.,as.integer))
favoritecount <- rename(favoritecount, favorites = .)

#retweet count
retweetcount <- trump %>% map(~.$retweetCount)
retweetcount <- map(retweetcount,~data.frame(.))
retweetcount <- map_dfr(retweetcount,~mutate_all(.,as.integer))
retweetcount <- rename(retweetcount, retweets = .)

#sources
statussources <- sapply(trump,function(x) x$getStatusSource())
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
trump <- cbind(datecreated, screenname, statussources, tweettext, favoritecount, retweetcount)

trump %>%
  mutate(isretweeted = str_extract(text, "RT"),
         isretweeted = ifelse(isretweeted == "RT", TRUE, FALSE),
         text = str_replace_all(text, "RT\\s+", ""),
         candidate = "Trump") %>%
  select(date, candidate, name, sources, isretweeted, text, length, favorites, retweets) -> trump

# ----------

#texts of interest

raw.tidy <- rbind(biden,trump)

raw.tidy %>%
  mutate(sources = as.factor(sources)) ->
  raw.tidy

#Save to CSV
write.csv(raw.tidy, file = "11012020training.csv", row.names = FALSE)

# ----------

#Full training set
oct17 <- read.csv("10172020training.csv")
oct18 <- read.csv("10182020training.csv")
oct19 <- read.csv("10192020training.csv")
oct20 <- read.csv("10202020training.csv")
oct21 <- read.csv("10212020training.csv")
oct22 <- read.csv("10222020training.csv")
oct23 <- read.csv("10232020training.csv")
oct24 <- read.csv("10242020training.csv")
oct25 <- read.csv("10252020training.csv")
oct26 <- read.csv("10262020training.csv")
oct27 <- read.csv("10272020training.csv")
oct28 <- read.csv("10282020training.csv")
oct29 <- read.csv("10292020training.csv")
oct30 <- read.csv("10302020training.csv")
oct31 <- read.csv("10312020training.csv")
nov1 <- read.csv("11012020training.csv")

raw.set <- rbind(oct17,oct18,oct19,oct20,oct21,oct22,oct23,oct24,oct25,oct26) %>%
  mutate(isretweeted = replace_na(isretweeted, FALSE))

# List of key topics

raw.set %>%
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
                                                   "disinformation"))) -> raw.set

write.csv(raw.set, file = "raw_training.csv", row.names = FALSE)
