library(tidyverse)
library(patchwork)
library(hrbrthemes)
library(stringr)
library(scales)
library(kableExtra)
NFT_volume <- read_csv("chart.csv")
options(scipen=999)

#[1.1] Volume of NFT market
NFT_volume_long <- NFT_volume |>
  pivot_longer(cols = c(`Number of sales`, `Sales USD`), 
               names_to = "type",
               values_to = "value")

NFT_volume|>
  ggplot() +
  geom_line(aes(x = DateTime, y = `Number of sales`, color = "The number of sales")) +
  geom_line(aes(x = DateTime, y = `Sales USD`*230000/420000000, color = "Sales USD")) +
  scale_y_continuous(name = "The number of sales", 
                     labels = comma, 
                     sec.axis = sec_axis(~.*420000000/230000, name = "Sales USD", labels = comma)) +
  labs(title = "Volume of the NFT market (USD)", x = "", y = "", color = "Type")
## These graphs show the volume of the NFT market using two different axises:
## The number of sales and Sales USD. Both graphs show the volume of the NFT
## market started to surge since 2021.
## Why? : Timestamp should be added.

#[1.2] Volume of NFT market by sector
# The rank of sectors by year
NFT_volume_by_sector <- tibble(
  `sector` = c("All", "Collectible", "Game", "Art", "Metaverse", "Utility", "DeFi", "Undefined"),
  `2018` = c(36.77, 13.86, 5.19, 0.05, 16.35, 1.29, 0, 0.03),
  `2019` = c(24.02, 2.71, 11.59, 0.45, 5.38, 4.11, 0, 0),
  `2020` = c(66.78, 16.45, 15.26, 17.11, 15.97, 2.41, 0, 0),
  `2021` = c(13981.9, 7130.05, 2153.82, 2107.57, 630.99, 75.5, 19.75, 1864.22)
)

# NFT_volume_by_sector_long <- NFT_volume_by_sector |>
#  pivot_longer(cols = c(`2018`, `2019`, `2020`, `2021`),
#               names_to = "year",
#               values_to = "value") 
#NFT_volume_by_sector_long$sector <- as.factor(NFT_volume_by_sector_long$sector)

# category_name + arrange(desc(year)) by each year => new table
NFT_volume_by_sector_rank <- tibble(c(1:8))

for (x in c(1:4)){
a <- NFT_volume_by_sector[c(1,x+1)] |>
  arrange(desc(NFT_volume_by_sector[x+1]))
colnames(a) <- c('sector', 'year') 
a <- str_c(a$`sector`, " (", a$`year`, ")")
NFT_volume_by_sector_rank <- NFT_volume_by_sector_rank |>
  cbind(a)
}

NFT_volume_by_sector_rank <- NFT_volume_by_sector_rank[-1, -1]
colnames(NFT_volume_by_sector_rank) <- c(2018, 2019, 2020, 2021)
knitr::kable(
NFT_volume_by_sector_rank,
title = "The rank of NFT market volumes by sectors over time",
caption = "Source: NonFungible"
)
## 1. By and large, collectible are one of the most populars sectors
## over time, especially taking the first place in 2021 when the
## public interest on NFT started to surge by earnest, leaving a
## huge gap over other sectors.
## 2. On the other hand, although metaverse sector was the most popular
## sector initially in 2018, its relative scale has been shtrinked
## gradually.
## 3. The scale of art sector is also noticeable in that it showed
## the remarkable expansion in 2020 and also recorded third largest
## scale among all sectors in 2021 as well.
## Limitation of the data: since individual NFT products are transacted
## in a high price, this data does not show the overall progress of
## each sector over time. 
## 4. Also, the DeFi sector is noticeable. From 2018 to 2020 the
## scale of the sector was standstill but it started to expand in 2021.
## The why should be reviewed.

#[1.3] The public interest on NFT industry
# pubilc interest in 1y
world_interest_1y <- read_csv("NFT_worldwide_1y_interest.csv", skip = 2)
colnames(world_interest_1y) <- c("DateTime", "Interest(/100)")
world_interest_1y$`Interest(/100)` <- world_interest_1y$`Interest(/100)` |>
  str_replace_all("<1", "0")
world_interest_1y$`Interest(/100)` <- as.numeric(world_interest_1y$`Interest(/100)`)
world_interest_1y$`DateTime` <- format(world_interest_1y$DateTime, "%y-%m")
view(unique(world_interest_1y))
world_interest_1y_cleaned <- world_interest_1y |>
  group_by(DateTime) |>
  summarise("Interest(/100)" = max(`Interest(/100)`))

# public interest in 5y
world_interest_5y <- read_csv("NFT_worldwide_5y_interest.csv", skip = 2)
colnames(world_interest_5y) <- c("DateTime", "Interest(/100)")
world_interest_5y$`Interest(/100)` <- world_interest_5y$`Interest(/100)` |>
  str_replace_all("<1", "0")
world_interest_5y$`Interest(/100)`<- as.numeric(world_interest_5y$`Interest(/100)`)
world_interest_5y$DateTime <- format(as.Date(world_interest_5y$DateTime), "%y-%m")
view(world_interest_5y)
world_interest_5y_cleaned <- world_interest_5y |>
  group_by(DateTime) |>
  summarise("Interest(/100)" = max(`Interest(/100)`))
world_interest_5y_cleaned |>
  ggplot(aes(x = `DateTime`, y = `Interest(/100)`))+
  geom_col()

# The correlation between the interest and the actual sales
NFT_volume_by_month <- NFT_volume
NFT_volume_by_month$DateTime <- format(as.Date(NFT_volume_by_month$DateTime), "%y-%m")
NFT_volume_by_month <- NFT_volume_by_month |>
  group_by(DateTime) |>
  summarise(`Number of sales` = sum(`Number of sales`),
         `Sales USD` = sum(`Sales USD`))

a <- merge(NFT_volume_by_month, world_interest_5y_cleaned[-c(1,2),], by = "DateTime")
a<- as_tibble(a)

cor.test(x = a$`Sales USD`, y = a$`Interest(/100)`, method = "spearman")
# The p-value is lower than 0.05, so the correlation is significant.

#[1.4] The top/rising topics
# 5y Top
top_topic_5y <- read_csv("related_topics_5y_top.csv",  skip = 2)
top_topic_5y <- top_topic_5y |>
  separate(TOP, into = c("Topic", "Score"), sep = ",")
top_topic_5y <- top_topic_5y[c(1:25),]
top_topic_5y$Topic <- top_topic_5y$Topic |>
  tolower()
top_topic_5y$Score <- as.double(top_topic_5y$Score)
top_topic_5y$Score[is.na(top_topic_5y$Score)] <- 0.5

# Specify sectors
concept <- c("non-fungible token", "fungibility", "security token","whitelisting")
exchange <- c("opensea", "marketplace", "online marketplace","exchange", "airdrop", "korbit") #Investment facade
store <- c("wallet","metamask" ) #Possession facade
econ <- c("coin", "cryptocurrency", "ethereum", "binanace", "wallet", "metamask",
          "expensive", "blockchain.com", "coinbase", "cardano", "coinmarketcap", "shiba inu",
          "polygon", "whitelisting", "exchange", "oh hyun-min", "federal tax service", "airdrop", 
          "korbit", "investment","mint")
# especially related to cryptocurrency
art <- c("art","mike winkelmann", "tory lanez", "work of art", "intellectual property", "lee duhui",
         "melania trump")
collectibles <- c("apes", "monkey", "cryptopunks", "yacht", "yacht club", "neymar", "lee duhui",
                  "charlie bit my finger", "adzuki bean")
metaverse <- c("metaverse", "picrew", "meta")
game <- c("axie infinity", "mir4", "cyber dragon")

sectors <- list(concept, exchange, store, econ, art, collectibles, metaverse, game)

# Sector Scores
# Create a table
sector_score_tb <- tibble(
  sector = c("concept", "exchange", "store", "econ", "art", "collectibles", "metaverse", "game"),
  score_5y = c(0,0,0,0,0,0,0,0),
  score_1y = c(0,0,0,0,0,0,0,0),
  score_sk_5y = c(0,0,0,0,0,0,0,0),
  score_sk_1y = c(0,0,0,0,0,0,0,0),
  score_us_5y = c(0,0,0,0,0,0,0,0),
  score_us_1y = c(0,0,0,0,0,0,0,0)
)

# Fill the table
for(x in c(1:8)){
sector_score_tb[x,2] <- sum(top_topic_5y$Score[(top_topic_5y$Topic%in%unlist(sectors[x])==TRUE)])
}

# 1y Top
top_topic_1y <- read_csv("related_topics_1y_top.csv",  skip = 2)
top_topic_1y <- top_topic_1y |>
  separate(TOP, into = c("Topic", "Score"), sep = ",")
top_topic_1y <- top_topic_1y[c(1:25),]
top_topic_1y$Topic <- top_topic_1y$Topic |>
  tolower()
top_topic_1y$Score <- as.double(top_topic_1y$Score)
top_topic_1y$Score[is.na(top_topic_1y$Score)] <- 0.5

# Fill the sectors
for(x in c(1:8)){
  sector_score_tb[x,3] <- sum(top_topic_1y$Score[(top_topic_1y$Topic%in%unlist(sectors[x])==TRUE)])
}
top_topic_1y$Topic%in%unlist(sectors[2])==TRUE
NFT_volume_by_sector_rank
sector_score_tb
# sector_score_tb => Almost no difference. The slight difference is that the public interest in
# market place, art, econ decreased a bit, and that for the collectibles and metaverse increased
# 0.5 score.
# Not meaningful tho, and it's not consistent with the volume of the sectors in sales.


# South Korea Top topic in 5y/1y
# 5y
top_topic_sk_5y <- read_csv("SK_5y.csv", skip = 2)
top_topic_sk_5y <- top_topic_sk_5y |>
  separate(TOP, into = c("Topic", "Score"), sep = ",")
top_topic_sk_5y <- top_topic_sk_5y[c(1:13),]
top_topic_sk_5y$Topic <- top_topic_sk_5y$Topic |>
  tolower()
top_topic_sk_5y$Score <- as.double(top_topic_sk_5y$Score)

# Fill sectors
for(x in c(1:8)){
  sector_score_tb[x,4] <- sum(top_topic_sk_5y$Score[(top_topic_sk_5y$Topic%in%unlist(sectors[x])==TRUE)])
}

# 1y
top_topic_sk_1y <- read_csv("SK_1y.csv", skip = 2)
top_topic_sk_1y <- top_topic_sk_1y |>
  separate(TOP, into = c("Topic", "Score"), sep = ",")
top_topic_sk_1y <- top_topic_sk_1y[c(1:21),]
top_topic_sk_1y$Score <- as.double(top_topic_sk_1y$Score)
top_topic_sk_1y$Score[is.na(top_topic_sk_1y$Score)==TRUE] <- 0.5
top_topic_sk_1y$Topic <- top_topic_sk_1y$Topic |>
  tolower()

# Fill sectors
for(x in c(1:8)){
  sector_score_tb[x,5] <- sum(top_topic_sk_1y$Score[(top_topic_sk_1y$Topic%in%unlist(sectors[x])==TRUE)])
}

# United States Top Topic 5y/1y
# 5y
top_topic_us_5y <- read_csv("US_5y.csv", skip=2)
top_topic_us_5y <- top_topic_us_5y |>
  separate(col = TOP, into = c("Topic", "Score"), sep = ",")
top_topic_us_5y <- top_topic_us_5y[c(1:25),]
top_topic_us_5y$Score <- as.double(top_topic_us_5y$Score) 
top_topic_us_5y$Score[is.na(top_topic_us_5y$Score)==TRUE]<-0.5
top_topic_us_5y$Topic <- top_topic_us_5y$Topic |>
  tolower()

# Fill sectors
for(x in c(1:8)){
  sector_score_tb[x,6] <- sum(top_topic_us_5y$Score[(top_topic_us_5y$Topic%in%unlist(sectors[x])==TRUE)])
}

# United States Top Topic 1y
# 1y
top_topic_us_1y <- read_csv("US_1y.csv", skip = 2)
top_topic_us_1y <- top_topic_us_1y |>
  separate(col = TOP, into = c("Topic", "Score"), sep = ",")
top_topic_us_1y <- top_topic_us_1y[c(1:24),]
top_topic_us_1y$Score <- as.double(top_topic_us_1y$Score)
top_topic_us_1y$Topic <- top_topic_us_1y$Topic|>
  tolower()
top_topic_us_1y$Score[is.na(top_topic_us_1y$Score)==TRUE] <- 0.5


# Fill sectors
for(x in c(1:8)){
  sector_score_tb[x,7] <- sum(top_topic_us_1y$Score[(top_topic_us_1y$Topic%in%unlist(sectors[x])==TRUE)])
}

# [Table] Final outcome
knitr::kable(sector_score_tb,
             caption = "Interest Score in each segment by categories",
             col.names = c("Segment", "Worldwide(5Y)", "Worldwide(1Y)", 
                           "South Korea(5Y)", "South Korea(1Y)",
                           "United States(5Y)", "United States(1Y)"),
             booktabs = TRUE,
             linesep = "") 
# |> kable_styling(latext_options = "HOLD_position")
# Limitation 1: "school of open learning"(in US 1y) => cannot interpret in the NFT context. Plus, the score was below 1,
# so concluded it won't affect significantly to the final outcome, so exclude the topic.
# Limitation 2: some of the keywords were overlapped in more than two segments.
# Segments were referring to the categories defined by Nonfungible.com.
# Standard should be reviewed in the further studies.
# Limitation 3: was not able to use the local search Engine's datalab(Naver)
# The reason why the score in 'concept' sector is above 100 is that the 'Non-fungible token' keyword
# itself was counted as a related keyword.

# Analysis : 
# Vertical analysis(over time)
# Worldwide - In the beginning, the pubic looked up the keywords related to the concept of NFT itself
# but it slightly decreased over time. There is no significant fluctuation in every segment between
# the two periods, but we can tell there is a slight decrease in economics and art sectors while
# there is a slight increase in collectibles and metaverse sectors.
# South Korea - Overall, the public interest on NFT is biased on economic segments, in both long term
# and short term. The interest on art, collectibles, metaverse and game are much lower than 1/5 of
# the scores of economics. That on art decreased over time, while the counterpart of the game increased
# from 0 to 1.5. In recent years, what does the NFT something to do with game?(add further analysis)
# United States - the public interest in exchange was 13% higher in short term compared to long term.
# it can be interpreted as the more public are looking for an actual trade as time goes by.
# The interest on economic sectors decreased 12.5% over time, as well as that on art, while
# the interest on collectibles slightly increased.

# Horizontal analysis(over countries)
sector_score_tb |>
  select(`sector`, `score_5y`, `score_sk_5y`, `score_us_5y`) |>
  knitr::kable(caption = "Interest Score in each segment by categories",
               col.names = c("Segment", "Worldwide(5Y)", "South Korea(5Y)", "United States(5Y)"),
               booktabs = TRUE,
               linesep = "")
# |> kable_styling(latext_options = "HOLD_position")

# Long term(5Y)
# By and large, in South Korea, the interest on NFT is biased on economics.
# Moreover, that on art and collectibles was significantly lower 
# than the world average and the States. The interest on exchange, store was also more than half
# lower than the world average and the States. 
# Rather, that on the metaverse were much higher in South Korea in comparison to the worldwide 
# and the States. In terms of game segment, there was no significant difference between two countries,
# , which took the lowest place among all sectors.

# Short term(1Y)
sector_score_tb |>
  select(`sector`, `score_1y`, `score_sk_1y`, `score_us_1y`) |>
  knitr::kable(caption = "Interest Score in each segment by categories",
               col.names = c("Segment", "Worldwide(1Y)", "South Korea(1Y)", "United States(1Y)"),
               booktabs = TRUE,
               linesep = "")
# |> kable_styling(latext_options = "HOLD_position")

# The public interest of South Korea in economics sectors was still dominant 
# over other segments in short term as well. It was over the score of worldwide as well as the States.
# On the otherhand, the public interest of the United States was relatively well distributed in
# different segments than South Korea. 
# The interest of South Korea in exchange and store was still way lower than the worldwide 
# and the States, and this tendency was shown clearer in art and collectibles sectors. 
# The one thing noticeable is that the public interest in game segment is higher in South Korea
# than the worldwide and the States. 
# and the States


# Insights
# The Public interest of South Korea in NFT is strongly biased on economic segments in comparison
# to the world and the United States. 
# Also, the public interest in exchange and store are less than a half of the world and the States.
# The paradox of high interest in economics and low interest exchange/store can mean 
# the public is not that interested in the actual trade and the purchase of the NFT
# products yet, cuz the market is not settled yet. It should be validated with the sales of the
# NFT products in South Korea.
# On the other hand, the public interest of South Korea in art and collectibles were remarkably
# lower than the worldwide and the States. This shows the NFT trend which differs depending on
# countries. 
# *The social/cultural context of South Korea should be illustrated here* 
# Therefore, in South Korea, NFT is treated as investment ends, more than collectible ends so far.


# [3.1] The customer statistics
# Insights from MORNING CONSULT (Americans)
# 1. 1 in 3 Americans consider themselves collectors of physical items
# 2. Men are twice as likely as women to identify as collectors
# (cf. Individuals engaging with both F1 Delta Time and Sorare are also overwhelmingly male)
# 3. About half of physical collectors said they're interested in trying NFTs.
# 4. Millenials were most likely to engage in collecting (some sort of physical item as either 
# a hobby or investment) with 42 percent saying they do so <-> compared to 37% Gen X, 29% baby boomers
# 20% Gen Z. (Even with regard to digital assets like NFTs, fewer Gen Z respondents said they were
# involoved than Millenials and Gen X.)
# 5. There is already significant crossover between physical and digital collectors.
# 6. NFT: appealed primarily to Millenials: 34% of users are between the ages of 25-34(millenials)
# <-> 27% between 34-54, 28% Gen Z.
# *O'Hagon theorized that the lack of engagement in collectibles among Gen Zers can be chalked up to their lack of disposable
# income relative to older peers, who are more likely to be gainfully employed.
# (But it doesn't mean that there is no demand among them)
# 7. In the Morning Consult survey, 45 percent of male respondents said they engage in some sort of
# physical collecting, roughly twice the share of women who identified as collectors.
# The disparity between men and women was even more pronounced in the nascent realm of digital collectibles
# with 15% of male respondents saying they collect NFTs, compared to 4% of female respondents
# (But again, it doesn't mean that there is no demand among them.) Well.. O'Hagon said
# the vast majority collectibles on today's market are male-centric, and that Sorare is in the
# process of creating NFTs featuring female atheletes. Dapper Labs, creators of NBA Top Shot, the
# internet's most popular NFT program to date, also said it is working with the WNBA to introduce
# women players into the Top Shot collection
# (However, it cannot be the ultimate solution to explode the demand of the female target.)
# Gender representation is ofcourse important producing NFT products, but the interest of
# female targets should be more refined elaborately. For instance, ...

# [2.4] Customer statistics[Demographics]
# Correlation test
# Limitation: Since NFT market is nascent, especially in South Korea, there is no statistics with regard to
# the customer demographics. 
sk_2y_interest <- readxl::read_excel("SK_customer_statistics_2y.xls", skip = 7,
                                  col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
sk_2y_interest <- sk_2y_interest[c(1:731),]

sk_2y_interest |>
  ggplot() +
  geom_line(aes(x = `일`, y= NFT, color = "NFT")) +
  geom_line(aes(x = `일`, y= `non-fungible token`, color = "non-fungible token")) +
  geom_line(aes(x = `일`, y= `Non-fungible token`, color = "Non-fungible token")) +
  geom_line(aes(x = `일`, y= `엔에프티`, color = "엔에프티")) +
  geom_line(aes(x = `일`, y= nft, color = "nft")) +
  labs(title = "Search Volume of five NFT queries over time",
       x = "Date", y ="Volume") +
  scale_fill_continuous()
# It turned out 'nft' is the most adequate search keyword representing NFT


sk_2y_interest_cleaned <- sk_2y_interest |>
  select(`일`, "Interest(/100)" = `nft`)
colnames(sk_2y_interest_cleaned) <- c("DateTime","Interest(/100)")
sk_2y_interest_cleaned$DateTime <- format(sk_2y_interest_cleaned$DateTime, "%Y-%m")
sk_2y_interest_cleaned$DateTime <- str_replace(sk_2y_interest_cleaned$DateTime,
                                                   "20", "")
sk_2y_interest_cleaned <- sk_2y_interest_cleaned |>
  group_by(DateTime) |>
  summarise("Interest(/100)" = max(`Interest(/100)`))
  
sk_2y_interest_by_gender <- readxl::read_excel("SK_customer_statistics_2y.xls", skip = 742)
sk_2y_interest_by_gender <- sk_2y_interest_by_gender[c(1:2),]

sk_2y_interest_by_age <- readxl::read_excel("SK_customer_statistics_2y.xls", skip = 748)
sk_2y_interest_by_age <- sk_2y_interest_by_age[c(1:6),]

# plot
world_interest_5y_cleaned |>
  ggplot(aes(x = `DateTime`, y = `Interest(/100)`))+
  geom_col()
# This graph shows the worldwide public interest in NFT started to burst from 21-02
nrow(world_interest_5y_cleaned)
which(world_interest_5y_cleaned == "20-04")
world_interest_5y_cleaned_compare <- world_interest_5y_cleaned[c(37:61),]
nrow(world_interest_5y_cleaned_compare)
nrow(sk_2y_interest_cleaned)

ww_wk_interest_comparison_tb <- merge(world_interest_5y_cleaned_compare,
                                      sk_2y_interest_cleaned,
                                      by = "DateTime")
colnames(ww_wk_interest_comparison_tb) <- c("DateTime", "Worldwide", "South Korea")
dat <- data.table::melt(ww_wk_interest_comparison_tb, id.var = "DateTime")
dat |>
  ggplot(aes(x=`DateTime`, y = value, color = variable, group = variable))+
  geom_line()
# The public interest in NFT of South Korea is in accordance with
# that of Worldwide. They started to increase in earnest from 21-0
# and both scored highest in 22-01.

# Demographic of the domestic potential targets
# by gender
sk_2y_interest_by_gender |>
  select(`성별`, NFT)
# The search volume of NFT keywords is biased among male users. 
# The disparity between men and women was even more pronounced in the nascent realm of digital collectibles
# with 15% of male respondents saying they collect NFTs, compared to 4% of female respondents


# by age
sk_2y_interest_by_age |>
  select(`연령`, NFT)
# The search volume of NFT keywords is biased among 40-50s. Even over 60s.
# This tendency is consistent with the US market in that the main
# customer of the market is millenials.

# It turned out there was no significant difference in demographic of the targets.
# Rather, there was a difference in sectors!!!!
