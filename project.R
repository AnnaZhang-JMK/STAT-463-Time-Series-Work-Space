install.packages("gmailr")
library('gmailr')
use_secret_file('stat463.json')

# HI j

sender = "psu.forecasting.group.3@gmail.com"
receiver = "psu.forecasting.group.3@gmail.com"

send_message(mime(
  To = receiver,
  From = sender,
  Subject = "Hi",
  body = "Hello"))

install.packages('openssl')
library(openssl)

library(pageviews)
library(simts)
wiki_ts = project_pageviews(granularity = "daily", start = "2018090100", end = "2018102100")

Xt = gts(wiki_ts$views)
plot(Xt)

gts_time(gts(wiki_ts$views))
