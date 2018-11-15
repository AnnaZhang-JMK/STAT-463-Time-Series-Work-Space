#devtools::install_github("SMAC-Group/simts")
devtools::install_github("SMAC-Group/forecast463")
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

send_prediction(group = group, 
                prediction = prediction, 
                to = to, 
                from = from, 
                key = key_group, 
                date = date)

#mobile apps
mobile = article_pageviews(platform = "mobile-app", start = "2018090100", end = "2020102100")
Xt.mobile = gts(mobile$views)
#linear regression
# mobile.time = gts_time(gts(mobile$views))
# lm.mobile = lm(as.vector(Xt.mobile)~mobile.time)
# check(lm.mobile)
mod.mobile = estimate(AR(1), Xt.mobile, demean = FALSE)
mobile.pred = predict(mod.mobile)
mobile_pred = mobile.pred$pred[1]
mobile_ci = as.matrix(cbind(mobile.pred$CI0.95[1, ]))
mob_forecasts = list(mobile_pred = mobile_pred, mobile_ci = mobile_ci)
mob_forecasts

#desktops
desktop = article_pageviews(platform = "desktop", start = "2018090100", end = "2020102100")
Xt.desktop = gts(desktop$views)
mod.desktop = estimate(AR(1), Xt.desktop, demean = FALSE)
desktop.pred = predict(mod.desktop)
desktop_ci = as.matrix(cbind(desktop.pred$CI0.95[1, ]))
desk_forecasts = list(desktop_pred = desktop.pred$pred[1], desktop_ci = desktop_ci)
desk_forecasts

#Silvio Berlusconi
sb_pageviews <- article_pageviews(article = "Silvio_Berlusconi", start = "2018090100", end = "2020102100")
Xt.sb = gts(sb_pageviews$views)
mod.AR1 = estimate(AR(1), Xt.sb, demean = FALSE)
pred.sb = predict(mod.AR1)
pred_sb = pred.sb$pred[1]
silvio_ci = as.matrix(cbind(pred.sb$CI0.95[1, ]))
silvio_forecasts = list(silvio_pred = pred_sb, silvio_ci = silvio_ci)
silvio_forecasts

#Beyonce
bey_pageviews <- article_pageviews(article = "Beyonce", start = "2018090100", end = "2020102100")
bey.Xt = gts(bey_pageviews$views)
mod.AR1 = estimate(AR(1), bey.Xt, demean = FALSE)
pred.bey = predict(mod.AR1)
pred_bey = pred.bey$pred[1]
bey_ci = as.matrix(cbind(pred.bey$CI0.95[1, ]))
bey_forecasts = list(bey_pred = pred_bey, bey_ci = bey_ci)
bey_forecasts

#Noam Chomsky
nc_pageviews <- article_pageviews(article = "Noam_Chomsky", start = "2018090100", end = "2020102100")
Xt.nc = gts(nc_pageviews$views)
mod.AR1 = estimate(AR(1), Xt.nc, demean = FALSE)
pred.nc = predict(mod.AR1)
pred_nc = mean(pred.nc$pred)
nc_ci = as.matrix(cbind(pred.nc$CI0.95[1, ]))
chom_forecasts = list(nc_pred = pred_nc, nc_ci = nc_ci)
chom_forecasts

#SS_Lazio
lazio_pageviews <- article_pageviews(article = "SS_Lazio", start = "2018090100", end = "2020102100")
Xt.lazio = gts(lazio_pageviews$views)
mod.AR1 = estimate(AR(1), Xt.lazio, demean = FALSE)
pred.lazio = predict(mod.AR1)
pred_lazio = pred.lazio$pred[1]
lazio_ci = as.matrix(cbind(pred.lazio$CI0.95[1, ]))
lazio_forecasts = list(lazio_pred = pred_lazio, lazio_ci = lazio_ci)
lazio_forecasts

#Thanks
thanks_pageviews <- article_pageviews(article = "Thanksgiving", start = "2018090100", end = "2020102100")
Xt.thanks = gts(thanks_pageviews$views)
mod.AR1 = estimate(AR(1), Xt.thanks, demean = FALSE)
pred.thanks = predict(mod.AR1)
pred_thanks = pred.thanks$pred[1]
thanks_ci = as.matrix(cbind(pred.thanks$CI0.95[1,]))
thanks_forecasts = list(thanks_pred = pred_thanks, thanks_ci = thanks_ci)
thanks_forecasts


# intergrate predictions
prediction = list(mobile = mob_forecasts, 
                  desktop = desk_forecasts,
                  silvio = silvio_forecasts, 
                  beyonce = bey_forecasts,
                  chomsky = chom_forecasts, 
                  lazio = lazio_forecasts,
                  thanks = thanks_forecasts)
prediction_OK = check_prediction(prediction = prediction)

credential_OK = check_credentials(group = group, from = from, key = key)
credential_OK

# mod.AR2 = estimate(AR(2), Xt, demean = FALSE, method = 'yule-walker')
# check(mod.AR2)
# predict(mod.AR2)
# 
# mod.SARIMA = estimate(SARIMA(ar = 1, i = 0, ma = 1, sar = 1, si = 0, sigma = 0.5, s = 1), 
#                       Xt, method = "gmwm")
# check(mod.SARIMA) #not supported
