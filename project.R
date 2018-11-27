#devtools::install_github("SMAC-Group/simts")
#devtools::install_github("SMAC-Group/forecast463") #?
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
plot(Xt.mobile)
corr_analysis(Xt.mobile)
#select(AR(10), Xt.mobile) #model selection
#select(ARMA(10, 4), Xt.mobile)
#HQ: AR(7)
#AIC, BIC: White Noise
mod.mobile = estimate(AR(7), Xt.mobile, demean = FALSE)
check(mod.mobile)
mobile.pred = predict(mod.mobile, n.ahead = 30)
mobile_pred = mobile.pred$pred[1]
mobile_ci = as.matrix(cbind(mobile.pred$CI0.95[1, ]))
mob_forecasts = list(mobile_pred = mobile_pred, mobile_ci = mobile_ci)
mob_forecasts

#desktops
desktop = article_pageviews(platform = "desktop", start = "2018090100", end = "2020102100")
Xt.desktop = gts(desktop$views)
corr_analysis(Xt.desktop)
#select(ARMA(6, 6), Xt.desktop)
mod.desktop = estimate(ARMA(6, 6), Xt.desktop, demean = FALSE)
check(mod.desktop)
desktop.pred = predict(mod.desktop)
desktop_ci = as.matrix(cbind(desktop.pred$CI0.95[1, ]))
desk_forecasts = list(desktop_pred = desktop.pred$pred[1], desktop_ci = desktop_ci)
desk_forecasts

#Silvio Berlusconi
sb_pageviews <- article_pageviews(article = "Silvio_Berlusconi", start = "2018090100", end = "2020102100")
Xt.sb = gts(sb_pageviews$views)
plot(Xt.sb)
corr_analysis(Xt.sb)
# select(MA(10), Xt.sb)
# select(ARMA(5,5), Xt.sb) #MA(1)
mod_sb = estimate(MA(1), Xt.sb, demean = FALSE)
pred.sb = predict(mod_sb, n.ahead = 30)
pred_sb = pred.sb$pred[1]
silvio_ci = as.matrix(cbind(pred.sb$CI0.95[1, ]))
silvio_ci[1,1]=0
silvio_forecasts = list(silvio_pred = pred_sb, silvio_ci = silvio_ci)
silvio_forecasts

#Beyonce
bey_pageviews <- article_pageviews(article = "Beyonce", start = "2018090100", end = "2020102100")
bey.Xt = gts(bey_pageviews$views)
plot(bey.Xt)
corr_analysis(bey.Xt)
# length(bey.Xt)
bey.Xt = gts(bey.Xt[-c(1:10)])
plot(bey.Xt)
# select(AR(10), bey.Xt)
# select(ARMA(5,5), bey.Xt)
mod.bey = estimate(AR(2), bey.Xt, demean = FALSE)
pred.bey = predict(mod.bey)
pred_bey = pred.bey$pred[1]
bey_ci = as.matrix(cbind(pred.bey$CI0.95[1, ]))
bey_forecasts = list(bey_pred = pred_bey, bey_ci = bey_ci)
bey_forecasts

#Noam Chomsky
nc_pageviews <- article_pageviews(article = "Noam_Chomsky", start = "2018090100", end = "2020102100")
Xt.nc = gts(nc_pageviews$views)
plot(Xt.nc)
corr_analysis(Xt.nc)
# select(MA(10), Xt.nc)
# select(ARMA(2,8), Xt.nc)
evaluate(list(AR(1), MA(2)), Xt.nc, criterion = 'MAPE')
#AR(1) is the final model
# mod.nc = estimate(MA(3), Xt.nc)
mod.nc = estimate(AR(1), Xt.nc)
pred.nc = predict(mod.nc)
pred_nc = mean(pred.nc$pred)
nc_ci = as.matrix(cbind(pred.nc$CI0.95[1, ]))
chom_forecasts = list(nc_pred = pred_nc, nc_ci = nc_ci)
chom_forecasts

#SS_Lazio
lazio_pageviews <- article_pageviews(article = "SS_Lazio", start = "2018090100", end = "2020102100")
Xt.lazio = gts(lazio_pageviews$views)
plot(Xt.lazio)
corr_analysis(Xt.lazio)
# select(ARMA(5,5), Xt.lazio)
# select(MA(10), Xt.lazio)
# select(AR(10), Xt.lazio)
mod.lazio = estimate(AR(1), Xt.lazio, demean = FALSE)
pred.lazio = predict(mod.lazio)
pred_lazio = pred.lazio$pred[1]
lazio_ci = as.matrix(cbind(pred.lazio$CI0.95[1, ]))
# lazio_ci[1,1]=0
lazio_forecasts = list(lazio_pred = pred_lazio, lazio_ci = lazio_ci)
lazio_forecasts

#Thanks
thanks_pageviews <- article_pageviews(article = "Thanksgiving", start = "2018090100", end = "2020102100")
Xt.thanks = gts(thanks_pageviews$views)
plot(Xt.thanks)
corr_analysis(Xt.thanks)
select(ARMA(3,3), Xt.thanks) #non-stationary
mod.AR1 = estimate(AR(1), Xt.thanks, demean = FALSE)
pred.thanks = predict(mod.AR1)
pred_thanks = pred.thanks$pred[1]
thanks_ci = as.matrix(cbind(pred.thanks$CI0.95[1,]))
thanks_ci[1,1]=0
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

# mod.AR2 = estimate(AR(2), Xt, demean = FALSE, method = 'yule-walker')
# check(mod.AR2)
# predict(mod.AR2)
# 
# mod.SARIMA = estimate(SARIMA(ar = 1, i = 0, ma = 1, sar = 1, si = 0, sigma = 0.5, s = 1), 
#                       Xt, method = "gmwm")
# check(mod.SARIMA) #not supported
