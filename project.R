#
#devtools::install_github("SMAC-Group/simts")
# install.packages("gmailr")
# install.packages('openssl')
# install.packages('sarima')
# install.packages('pageviews')
library(gmailr)
library(openssl)
library(pageviews)
library(simts)
library(sarima)

use_secret_file('stat463.json')

suppressPackageStartupMessages(library(gmailr))

send_prediction = function(group, prediction, to, from, key, date = Sys.Date()){
  send_message(mime(
    To = to,
    From = from,
    Subject = paste("[STAT 463] Group ", group, sep = ""),
    body = paste(key, date, paste(unlist(prediction), collapse = ","), sep = ";")))
}

group = 3
to = "psu.forecasting.instructors@gmail.com"
from = "psu.forecasting.group.3@gmail.com"
key_group = "dJuWKAEQdf6102W"
date = Sys.Date()

send_prediction(group = group, prediction = prediction, to = to, from = from, 
                key = key_group, date = date)

### model fitting
# Xt = gts(wiki_ts$views)
# plot(Xt)
#fit.lm <- lm(Xt~gts_time(gts(wiki_ts$views)))

#how to fit a polynomial regression and get residuals?

#mobile apps
mobile = article_pageviews(platform = "mobile-app", start = "2018090100", end = "2018102100")
Xt.mobile = gts(mobile$views)
#linear regression
# mobile.time = gts_time(gts(mobile$views))
# lm.mobile = lm(as.vector(Xt.mobile)~mobile.time)
# check(lm.mobile)
mod.mobile = estimate(AR(1), Xt.mobile, demean = FALSE)
mobile.pred = predict(mod.mobile)
mobile_pred = mean(mobile.pred$pred) #point estimate
mobile_ci = as.matrix(cbind(mean(mobile.pred$CI0.95[,1]), mean(mobile.pred$CI0.95[,2])))
mob_forecasts = list(mobile_pred = mobile_pred, mobile_ci = mobile_ci)
mob_forecasts

#desktops
desktop = article_pageviews(platform = "desktop", start = "2018090100", end = "2018102100")
Xt.desktop = gts(desktop$views)
mod.desktop = estimate(AR(1), Xt.desktop, demean = FALSE)
desktop.pred = predict(mod.desktop)
desktop_pred = mean(desktop.pred$pred)
desktop_ci = as.matrix(cbind(mean(desktop.pred$CI0.95[,1]), mean(desktop.pred$CI0.95[,1])))
desk_forecasts = list(desktop_pred = desktop_pred, desktop_ci = desktop_ci)
desk_forecasts

#Silvio Berlusconi
sb_pageviews <- article_pageviews(article = "Silvio_Berlusconi", start = "2018090100", end = "2018102100")
Xt.sb = gts(sb_pageviews$views)
mod.AR1 = estimate(AR(1), Xt.sb, demean = FALSE)
#check(mod.AR1)
pred.sb = predict(mod.AR1)
pred_sb = mean(pred.sb$pred)
silvio_ci = as.matrix(cbind(mean(pred.sb$CI0.95[,1]), mean(pred.sb$CI0.95[,2])))
silvio_forecasts = list(silvio_pred = pred_sb, silvio_ci = silvio_ci)
silvio_forecasts

#Beyonce
bey_pageviews <- article_pageviews(article = "Beyonce", start = "2018090100", end = "2018102100")
bey.Xt = gts(bey_pageviews$views)
mod.AR1 = estimate(AR(1), bey.Xt, demean = FALSE)
pred.bey = predict(mod.AR1)
pred_bey = mean(pred.bey$pred)
bey_ci = as.matrix(cbind(mean(pred.bey$CI0.95[,1]), mean(pred.bey$CI0.95[,2])))
bey_forecasts = list(bey_pred = pred_bey, bey_ci = bey_ci)
bey_forecasts

#Noam Chomsky
nc_pageviews <- article_pageviews(article = "Noam_Chomsky", start = "2018090100", end = "2018102100")
Xt.nc = gts(nc_pageviews$views)
mod.AR1 = estimate(AR(1), Xt.nc, demean = FALSE)
pred.nc = predict(mod.AR1)
pred_nc = mean(pred.nc$pred)
nc_ci = as.matrix(cbind(mean(pred.nc$CI0.95[,1]), mean(pred.nc$CI0.95[,2])))
chom_forecasts = list(nc_pred = pred_nc, nc_ci = nc_ci)
chom_forecasts

#SS_Lazio
lazio_pageviews <- article_pageviews(article = "SS_Lazio", start = "2018090100", end = "2018102100")
Xt.lazio = gts(lazio_pageviews$views)
mod.AR1 = estimate(AR(1), Xt.lazio, demean = FALSE)
pred.lazio = predict(mod.AR1)
pred_lazio = mean(pred.lazio$pred)
lazio_ci = as.matrix(cbind(mean(pred.lazio$CI0.95[,1]), mean(pred.lazio$CI0.95[,2])))
lazio_forecasts = list(lazio_pred = pred_lazio, lazio_ci = lazio_ci)
lazio_forecasts

#SS_Lazio
lazio_pageviews <- article_pageviews(article = "SS_Lazio", start = "2018090100", end = "2018102100")
Xt.lazio = gts(lazio_pageviews$views)
mod.AR1 = estimate(AR(1), Xt.lazio, demean = FALSE)
pred.lazio = predict(mod.AR1)
pred_lazio = mean(pred.lazio$pred)
lazio_ci = as.matrix(cbind(mean(pred.lazio$CI0.95[,1]), mean(pred.lazio$CI0.95[,2])))
lazio_forecasts = list(lazio_pred = pred_lazio, lazio_ci = lazio_ci)
lazio_forecasts


#Thanks
thanks_pageviews <- article_pageviews(article = "Thanksgiving", start = "2018090100", end = "2018102100")
Xt.thanks = gts(thanks_pageviews$views)
mod.AR1 = estimate(AR(1), Xt.thanks, demean = FALSE)
pred.thanks = predict(mod.AR1)
pred_thanks = mean(pred.thanks$pred)
thanks_ci = as.matrix(cbind(mean(pred.thanks$CI0.95[,1]), mean(pred.thanks$CI0.95[,2])))
thanks_forecasts = list(thanks_pred = pred_thanks, thanks_ci = thanks_ci)
thanks_forecasts


# intergrate predictions
prediction = list(mobile = mob_forecasts, desktop = desk_forecasts,
                  silvio = silvio_forecasts, beyonce = bey_forecasts,
                  chomsky = chom_forecasts, lazio = lazio_forecasts,
                  thanks = thanks_forecasts)

# mod.AR2 = estimate(AR(2), Xt, demean = FALSE, method = 'yule-walker')
# check(mod.AR2)
# predict(mod.AR2)
# 
# mod.SARIMA = estimate(SARIMA(ar = 1, i = 0, ma = 1, sar = 1, si = 0, sigma = 0.5, s = 1), 
#                       Xt, method = "gmwm")
# check(mod.SARIMA) #not supported
