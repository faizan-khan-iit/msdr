library(readr)
library(dplyr)
library(tidyr)
library(readxl)

## read in data
dspec <- read_csv("./data/daily_SPEC_2014.csv.bz2",
                  col_types = c("ccc--dd-c--D----d-------c----"))

cols_excel <- rep("blank", 28) # Skip these columns
cols_excel[c(1:3, 8,9,23)] <- "text"
cols_excel[4:5] <- "numeric"
aqs_sites <- read_excel("./data/aqs_sites.xlsx",
                        col_types = cols_excel)

# What is average Arithmetic.Mean for "Bromine PM2.5 LC" in
# the state of Wisconsin in this dataset?
ans1 <- dspec %>%
  select(`Parameter Name`, `State Name`, `Arithmetic Mean`) %>%
  filter(`State Name` == "Wisconsin" & 
           `Parameter Name` == "Bromine PM2.5 LC") %>%
  summarise(avgBudget = mean(`Arithmetic Mean`))
ans1$avgBudget


# Which constituent Parameter.Name has the highest average level?
ans2 <- dspec %>%
  select(`Parameter Name`, `Arithmetic Mean`) %>%
  group_by(`Parameter Name`) %>%
  summarise(meanParameter = max(`Arithmetic Mean`))
ans2[ans2$meanParameter == max(ans2$meanParameter), ]


# Which monitoring site has the highest average level of 
# "Sulfate PM2.5 LC" across all time?
ans3 <- dspec %>%
  rename(PN = `Parameter Name`,
         SC = `State Code`,
         CC = `County Code`,
         SN = `Site Num`,
         AM = `Arithmetic Mean`) %>%
  select(PN, SC, CC, SN, AM) %>%
  filter(PN == "Sulfate PM2.5 LC") %>%
  group_by(SC, CC, SN) %>%
  summarise(maxVal = mean(AM),
            n_obs = n())
ans3[ans3$maxVal == max(ans3$maxVal), ]


# What is the absolute difference in the average levels of
# "EC PM2.5 LC TOR" between the states California and Arizona,
# across all time and all monitoring sites?
ans4 <- dspec %>%
  rename(SN = `State Name`,
         PN = `Parameter Name`,
         AM = `Arithmetic Mean`) %>%
  select(SN, PN, AM) %>%
  filter(PN %in% c("EC PM2.5 LC TOR")
         & SN %in% c("California", "Arizona")) %>%
  group_by(SN) %>%
  summarise(n_obs = n(),
            meanVal = mean(AM))

abs(ans4$meanVal[[1]] - ans4$meanVal[[2]])

# What is the median level of "OC PM2.5 LC TOR" in the western US,
# across all time? Define western as any monitoring location that 
# has a Longitude LESS THAN -100.
ans5 <- dspec %>%
  rename(L = Longitude,
         La = Latitude,
         PN = `Parameter Name`,
         AM = `Arithmetic Mean`) %>%
  select(L, PN, AM, La) %>%
  group_by(L, La) %>%
  filter(L < -100 & PN %in% c("OC PM2.5 LC TOR"))
median(ans5$AM)


# How many monitoring sites are labelled as both RESIDENTIAL for 
# "Land Use" and SUBURBAN for "Location Setting"?
ans6 <- aqs_sites %>%
  rename(LU = `Land Use`,
         LS = `Location Setting`) %>%
  select(LU, LS) %>%
  filter(LU %in% c("RESIDENTIAL") & LS %in% c("SUBURBAN")) %>%
  summarise(n = n())
ans6$n

# What is the median level of "EC PM2.5 LC TOR" amongst monitoring
# sites that are labelled as both "RESIDENTIAL" and "SUBURBAN" in
# the eastern U.S., where eastern is defined as Longitude greater
# than or equal to -100?
temp1 <- aqs_sites %>%
  rename(L = Latitude,
         Lo = Longitude,
         LU = `Land Use`,
         LS = `Location Setting`) %>%
  select(L, Lo, LU, LS) %>%
  filter(LU %in% c("RESIDENTIAL") & LS %in% c("SUBURBAN") &
           Lo >= -100)

temp2 <- dspec %>%
  rename(L = Latitude,
         Lo = Longitude,
         PN = `Parameter Name`,
         AM = `Arithmetic Mean`) %>%
  select(L, Lo, PN, AM) %>%
  filter(Lo >= -100 & PN %in% c("EC PM2.5 LC TOR"))

ans7 <- inner_join(temp1, temp2, by = c("L", "Lo"))
median(ans7$AM)


# Amongst monitoring sites that are labeled as COMMERCIAL 
# for "Land Use", which month of the year has the highest
# average levels of "Sulfate PM2.5 LC"
temp3 <- aqs_sites %>%
  rename(L = Latitude,
         Lo = Longitude,
         LU = `Land Use`,
         LS = `Location Setting`) %>%
  select(L, Lo, LU, LS) %>%
  filter(LU %in% c("COMMERCIAL"))

temp4 <- dspec %>%
  rename(L = Latitude,
         Lo = Longitude,
         PN = `Parameter Name`,
         AM = `Arithmetic Mean`,
         DL = `Date Local`) %>%
  filter(PN %in% c("Sulfate PM2.5 LC")) %>%
  mutate(Mo = months(DL)) %>%
  select(L, Lo, PN, AM, Mo)

ans8 <- inner_join(temp3, temp4, by = c("L", "Lo")) %>%
  group_by(Mo) %>%
  summarise(meanVal = mean(AM))
ans8[ans8$meanVal == max(ans8$meanVal), "Mo"]


# Take a look at the data for the monitoring site identified by
# State Code 6, County Code 65, and Site Number 8001
# (this monitor is in California). At this monitor, for how many 
# days is the sum of "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC"
# greater than 10?
temp5 <- dspec %>%
  rename(PN = `Parameter Name`,
         SC = `State Code`,
         CC = `County Code`,
         SN = `Site Num`,
         AM = `Arithmetic Mean`,
         DL = `Date Local`) %>%
  select(PN, SC, CC, SN, AM, DL) %>%
  filter(SC == "06" &
         CC == "065" &
         SN == "8001" &
         PN %in% c("Sulfate PM2.5 LC")) %>%
  group_by(DL) %>%
  summarise(sumAM1 = mean(AM),
            n = n())

temp6 <- dspec %>%
  rename(PN = `Parameter Name`,
         SC = `State Code`,
         CC = `County Code`,
         SN = `Site Num`,
         AM = `Arithmetic Mean`,
         DL = `Date Local`) %>%
  select(PN, SC, CC, SN, AM, DL) %>%
  filter(SC == "06" &
           CC == "065" &
           SN == "8001" &
           PN %in% c("Total Nitrate PM2.5 LC")) %>%
  group_by(DL) %>%
  summarise(sumAM2 = mean(AM),
            n = n())

ans9 <- full_join(temp5, temp6, by="DL") %>%
  group_by(DL) %>%
  mutate(AM = sum(sumAM1, sumAM2, na.rm = T))
ans9[ans9$AM > 10, ]


# Which monitoring site in the dataset has the highest correlation
# between "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" across all
# dates? Identify the monitoring site by its State, County, and 
# Site Number code
# For each of the chemical constituents, there will be some dates that
# have multiple Sample.Values at a monitoring site. When there are 
# multiple values on a given date, take the average of the constituent
# values for that date

temp7 <- dspec %>%
  rename(PN = `Parameter Name`,
         SC = `State Code`,
         CC = `County Code`,
         SN = `Site Num`,
         AM = `Arithmetic Mean`,
         DL = `Date Local`) %>%
  select(PN, SC, CC, SN, AM, DL) %>%
  filter(PN %in% c("Sulfate PM2.5 LC")) %>%
  group_by(SC, CC, SN, DL) %>%
  mutate(meanAM1 = mean(AM))

temp8 <- dspec %>%
  rename(PN = `Parameter Name`,
         SC = `State Code`,
         CC = `County Code`,
         SN = `Site Num`,
         AM = `Arithmetic Mean`,
         DL = `Date Local`) %>%
  select(PN, SC, CC, SN, AM, DL) %>%
  filter(PN %in% c("Total Nitrate PM2.5 LC")) %>%
  group_by(SC, CC, SN, DL) %>%
  mutate(meanAM2 = mean(AM))

ans10 <- inner_join(temp7, temp8, by = c("SC", "CC", "SN", "DL")) %>%
  group_by(SC, CC, SN) %>%
  summarise(corAM = cor(meanAM1, meanAM2))
ans10[ans10$corAM == max(ans10$corAM), ]
