library(magrittr)
library(tidyr)
library(dplyr)

# Create dummy data
set.seed(142)
data_path <- "./data/team_standings.csv"
data_used <- read.csv(data_path)
data_used$Budget <- runif(length(data_used$Standing), 10, 100)
data_used$Group <- sample(LETTERS[1:8],
                          length(data_used$Standing),
                          replace = T)
data_used$FormerRank <- sample(1:32)
data_used$NumPlayers <- sample(23:30,
                               length(data_used$Standing),
                               replace = T)
data_used$Dress <- sample(c("Blue", "Green", "Blue", "Red"),
                          length(data_used$Standing),
                          replace = T)
data_used$Pool <- sample(1:3,
                         length(data_used$Standing),
                         replace = T)

# Piping
summary(head(filter(data_used, Group=="H")))
data_used %>%
  filter(Group=="H") %>%
  head(5) %>%
  summary()

# Summarise
data_used %>%
  summarize(n_obs = n(),
            max_budget = max(Budget),
            min_budget = min(Budget))

summarise(group_by(data_used, Group, Pool), mean(Budget))

data_used %>%
  group_by(Group, Pool) %>%
  summarise(maxBudget = max(Budget)) %>%
  head(10)

# Filter and Select
data_used %>%
  group_by(Group, Pool) %>%
  summarise(maxBudget = max(Budget)) %>%
  filter(maxBudget > 80)

data_used %>%
  filter(Group == "C") %>%
  select(Team, Budget, FormerRank) %>%
  summary()

# Mutate
data_used %>%
  select(Team, Group) %>%
  mutate(Standings = row.names(data_used))

data_used %>%
  group_by(Group) %>%
  mutate(avgBudget = mean(Budget)) %>%
  ungroup() %>%
  select(Team, avgBudget, Group) %>%
  head(8)

# Rename
data_used %>%
  select(Team, Budget, Standing) %>%
  rename(BudgetInMillions = Budget) %>%
  head(5)
