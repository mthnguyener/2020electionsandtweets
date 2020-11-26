# Combine and create a full list
electionfull <- rbind(election16, electiontidy)

# dataset for models
df <- electionfull %>%
  select(-date, -name, -text)

# Statistics
df %>%
  ggplot(aes(x=favorites, y=retweets)) +
  geom_line()
