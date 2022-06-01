# load packages
library(data.table)
library(binancer)
library(rredis)



# establish connection with Redis
redisConnect()

# check all the keys in Redis
redisKeys('alisial_symbol:*')

# get the keys and the corresponding values stored in Redis
symbols <- redisMGet(redisKeys('alisial_symbol:*'))

# convert the list into a data table
symbols <- data.table(
  symbol = sub('^alisial_symbol:', '', names(symbols)),
  N = as.numeric(symbols))
symbols

# extract the 'from' currency
symbols[, from := substr(symbol, 1, 3)]

# group by from and sum the quantities
symbols[, .(quantity = sum(N)), by = from]

# get the real-time prices in USD
prices <- binance_coins_prices()

# merge the two tables
dt <- merge(symbols, prices, by.x = 'from', by.y = 'symbol', all.x = TRUE, all.y = FALSE)

# calculate value in USD
dt[, value := as.numeric(N) * usd]

# calculate overall USD value of transactions
output <- dt[, sum(value)]

# calculate overall USD value of transactions by coin
dt[, sum(value), by = from]

# save the time in Eastern European Time
s <- Sys.time()
s <- .POSIXct(s, "EET")

# Print the message
print(paste0('The overall value of Binance transactions at ', s, 
             ' is: ',scales::dollar(output)))

# get time of stream start from Redis
start_time_daemon <- redisGet("time_of_start")

library(botor)
botor(region = 'eu-west-1')
## better way to get the Slack token
token <- ssm_get_parameter('slack')

library(slackr)
slackr_setup(username = 'Ali Sial', token = token, icon_emoji = ':exploding_head:')

# Start off by sending an informative slack message
slackr_msg(text = paste0('The overall value of Binance transactions between ', 
                         start_time_daemon, ' EET',' and ', s, ' EET',
                         ' is: ',scales::dollar(output)), 
           channel = '#bots-final-project')


library(ggplot2)

#  Number of each coin traded
total_coin <- 
  dt[,sum(N),by = from] %>% 
  ggplot(aes(x = reorder(from, -V1), y = V1)) + 
  geom_col(aes(fill = V1)) + 
  scale_fill_viridis_c(direction = -1) +
  labs(x = "Coin", y = "Total Value", title = paste( "Total number of each Crypto traded betweem", start_time_daemon,
                                                     "and", s)) +
  geom_text(aes(label = round(V1)), size = 4, hjust = 0.5, vjust = -0.5, position = "stack") +
  theme_classic() +
  theme(legend.position="none",axis.text.y=element_blank(),axis.ticks.y=element_blank()) 

slackr_setup(username = 'Ali Sial', token = token, icon_emoji = ':exploding_head:')
ggslackr(plot = total_coin, channels = '#bots-final-project', width = 12, height = 8)


# create ggplot for outlying transactions
scatterplot <- ggplot(dt, aes(N, value)) +
  geom_point(aes(size = value, color = value)) +
  geom_label(aes(label = symbol), position = position_nudge(x = -300, y = 20000000)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_viridis_c(direction = -1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Number of transactions", y = "USD value of transactions",
       title = paste("Scatterplot of Binance transaction values betweem", start_time_daemon,
                     "and", s))

# send to slack
ggslackr(plot = scatterplot, channels = '#bots-final-project', width = 10, height = 8)
slackr_setup(username = 'Ali Sial', token = token, icon_emoji = ':exploding_head:')

library(scales)
library(treemapify) 

# Total transaction value per coin
tree_map <- dt[,.(sum(value),sum(N)),by = from] %>% 
  ggplot(aes(area = V2, fill = from, lable = paste(from,V1, sep = "\n"))) + 
  geom_treemap() +
  scale_fill_viridis_d(direction = -1) +
  geom_treemap_text(aes(label = paste(from, dollar(round(V1),sep = "\n"))),
                    colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position="none") +
  labs(title = paste("Treemap of Binance transaction total values betweem", start_time_daemon,
                     "and", s))

slackr_setup(username = 'Ali Sial', token = token, icon_emoji = ':exploding_head:')
ggslackr(plot = tree_map, channels = '#bots-final-project', width = 12, height = 8)
