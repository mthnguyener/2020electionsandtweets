full.set <- read.csv("Data/sent_training.csv")

str(full.set)

# For Factor with 3 levels (no, low, high)
full.set %>% mutate(surprise = as.factor(case_when(surprise == 0 ~ "no",
                                         surprise == 1 ~ "low",
                                         surprise > 1 ~ "high"))) -> test

str(test)


# Clean Set
full.set %>%
  mutate(date = as_datetime(date),
         candidate = as.factor(candidate),
         sources = as.factor(sources)) %>%
  select(-date, -name, -isretweeted, -sources, -length, -favorites, -retweets) -> full.set

str(full.set)

full.set %>%
  rename(c("biden"="candidate")) %>%
  mutate(biden = ifelse(biden=="Biden", 1, 0)) -> num.set

# CORR
rcorr(as.matrix(num.set))

cor(num.set)

# CLUSTER
election.norm <- scale(num.set)
set.seed(12345)
election.kmclusters <- kmeans(election.norm, 4, nstart=10)
unscale(election.kmclusters$centers, election.norm)

set.seed(12345)
gaps <- clusGap(election.norm,kmeans,30,d.power=2)
maxSE(gaps$Tab[,"gap"],gaps$Tab[,"SE.sim"],"Tibs2001SEmax")
plot(gaps$Tab[,"gap"])

distances <- dist(election.norm, method="euclidean")
election.hclusters <- hclust(distances, method="centroid")
ggdendrogram(election.hclusters,labels=FALSE)

# LOGIT
logit <- glm(candidate ~ ., data = full.set, family = binomial)
summary(logit)

res.dev <- logit$deviance
null.dev <- logit$null.deviance
dev.Rsq <- (null.dev-res.dev)/null.dev
dev.Rsq  # Deviance explained by the model

log.odds <- coef(logit)
odds <- exp(coef(logit))
prob <- odds/(1+odds)
cbind(log.odds, odds, prob)

# Classification TREE
class.tree <- tree(candidate ~ . , data = full.set, mindev=0.005)
summary(class.tree) # Get basic data on the fitted model
plot(class.tree)
text(class.tree, pretty=0)

