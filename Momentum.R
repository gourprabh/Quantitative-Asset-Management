library(tidyverse)
library(data.table)
library(lubridate)
require(moments)
library(dplyr)
library(ggplot2)
detach("package:tidyverse", unload=TRUE)

PS3_Q1 = function(CRSP_Stocks){
  input = CRSP_Stocks
  input[,date := as.Date(date)]
  input = input[SHRCD %in% c(10,11)]
  input = input[EXCHCD %in% c(1,2,3)]
  #input = na.omit(input,cols = 'PRC')
  input$PRC = abs(input$PRC)
  input[RET %in% c('C',-66.0,-77.0,-88.0,-99.0)]$RET = '0'
  input$RET = as.numeric(input$RET)
  input$DLRET = as.numeric(input$DLRET)
  input$RET[is.na(input$RET)] = 0
  input$DLRET[is.na(input$DLRET)] = 0
  #input = input[SHROUT != 0]
  input = input[,Mkt_eq := PRC * SHROUT]
  input = input[,tot_ret := (1 +  RET)*(1 + DLRET) - 1]
  
  input[,Year := year(date)]
  input[,Month := month(date)]
  input[,YrMo := 12*Year + Month]
  year = as.matrix(rep(1927,12),12,1)
  for (i in 1928:2019) {
    year = rbind(year,as.matrix(rep(i,12),12,1))
  }
  
  months = c(rep(seq(1,12,1),93))
  output = data.table(year,months)
  colnames(output) = c('Year','Month')
  
  month = as.Date(unique(input$date))
  month = month[order(month)]
  
  setorder(input,PERMNO, date)
  input[,lagged_me := shift(Mkt_eq,type = 'lag'),by = PERMNO]
  
  # Checking if market value(t-1) is not missing
  input[, prev_yrmo := shift(YrMo),by = PERMNO]
  input[, Valid_lag := YrMo == (prev_yrmo + 1)]
  input = input[Valid_lag == T & !(is.na(lagged_me))]
  
  # Checking if price(t-13) is not missing
  input[,price_t13 := shift(PRC,n=13),by = PERMNO]
  input[,prc_yrmo := shift(YrMo,n=13),by = PERMNO]
  input[,Valid_lag := (YrMo == (prc_yrmo + 13)) & (!is.na(price_t13))]
  
  # Checking if return(t-2) is not missing
  input[,ret_t2 := shift(RET,n=2),by = PERMNO]
  input[,ret_yrmo := shift(YrMo,n=2),by = PERMNO]
  input[,Valid_lag1 := (YrMo == (ret_yrmo + 2)) & (!is.na(ret_t2))]
  
  output[,yrmo := 12*Year + Month]
  
  input[,log_ret := log(1 + tot_ret)]
  input = input[!(is.na(log_ret))]
  
  input[, Ranking_ret :=  Reduce('+',shift(log_ret,2:12)),by = PERMNO]
  input[Valid_lag == T & Valid_lag1 == T]
  setorder(input, PERMNO,date)
  
  s = output$yrmo
  input = input[YrMo %in% s]
  input = input[!(is.na(Ranking_ret))]
  
  input = subset(input,select = c('Year','Month','PERMNO','EXCHCD','lagged_me','tot_ret','Ranking_ret'))
  setorder(input, Year, Month)
  colnames(input)[5:6] = c('lag_Mkt_Cap','Ret')
  return(input)
}

PS3_Q2 = function(CRSP_Stocks_Momentum){
  temp = CRSP_Stocks_Momentum
  temp[,yrmo := 12*Year + Month]
  DM_decile = temp[,quantile(Ranking_ret,probs = seq(0,1,0.1)),by = yrmo]
  KRF_decile = temp[EXCHCD == 1,quantile(Ranking_ret,
                                         probs = seq(0,1,0.1)),by = yrmo]
  
  yrmo = temp[,unique(yrmo)]
  temp1 = data.table(0,0,0,0,0,0,0,0,0,0)
  colnames(temp1) = c('Year','Month','PERMNO','EXCHCD','lag_Mkt_Cap',
                      'Ret','Ranking_ret','yrmo','KRF_decile',
                      'DM_decile')
  for (i in yrmo) {
    temp2 = temp[yrmo == i]
    temp2[,KRF_decile := findInterval(Ranking_ret, vec = KRF_decile[yrmo == i,V1],
                                      left.open = TRUE,all.inside = TRUE)]
    temp2[,DM_decile := findInterval(Ranking_ret, vec = DM_decile[yrmo == i,V1],
                                     left.open = TRUE,all.inside = TRUE)]
    temp1 = rbind(temp1,temp2)
  }
  temp1 = temp1[-1,]
  CRSP_Stocks_Momentum_decile = subset(temp1,
                                       select = c('Year','Month','PERMNO',
                                                  'lag_Mkt_Cap','Ret','DM_decile'
                                                  ,'KRF_decile'))
  return(CRSP_Stocks_Momentum_decile)
}

PS3_Q3 = function(CRSP_Stocks_Momentum_decile,FF_mkt){
  FF_mkt[,yrmo := 12*year + month]
  temp = CRSP_Stocks_Momentum_decile
  temp[,yrmo := 12*Year + Month]
  
  yrmo = temp[,unique(yrmo)]
  temp_ret = data.table(0,0,0,0,0)
  colnames(temp_ret) = c('yrmo','decile','KRF_ret','DM_ret','Rf')
  for (i in yrmo) {
    temp2 = temp[yrmo == i]
    temp2[,krf_wt := lag_Mkt_Cap/sum(lag_Mkt_Cap,na.rm = TRUE),
          by = KRF_decile]
    temp_ret_1 = temp2[,list(KRF_ret = sum(Ret*krf_wt,na.rm = TRUE)),by = KRF_decile]
    setorder(temp_ret_1,KRF_decile)
    colnames(temp_ret_1)[1] = c('decile')
    
    temp_ret_1 = cbind(yrmo = rep(i,10), temp_ret_1)
    temp2[,DM_wt := lag_Mkt_Cap/sum(lag_Mkt_Cap,na.rm = TRUE),
          by = DM_decile]
    temp_ret_dm = temp2[,list(DM_ret = sum(Ret*DM_wt,na.rm = TRUE)),by = DM_decile]
    setorder(temp_ret_dm,DM_decile)
    temp_ret_1 = cbind(temp_ret_1,DM_ret = temp_ret_dm$DM_ret)
    
    Rf = (FF_mkt[yrmo == i,Rf])/100
    temp_ret_1 = cbind(temp_ret_1, Rf = rep(Rf,10))
    temp_ret = rbind(temp_ret,temp_ret_1)
  }
  temp_ret = temp_ret[-1,]
  temp_ret[,Year := floor(yrmo/12)]
  temp_ret[,Month := yrmo%%12]
  temp_ret[Month == 0, Year := Year - 1]
  temp_ret[Month == 0, Month := Month + 12]
  temp_ret = subset(temp_ret,select = c('Year','Month','decile','DM_ret','KRF_ret','Rf'))
  
  colnames(temp_ret)[4:5] = c('DM_Ret','KRF_Ret')
  return(temp_ret)
}

PS3_Q4 = function(CRSP_Stocks_Momentum_returns){
  temp = CRSP_Stocks_Momentum_returns
  temp[,ex_ret := DM_Ret - Rf]
  
  output = data.frame(matrix(0,4,11))
  colnames(output) = c('Decile 1','Decile 2','Decile 3','Decile 4',
                       'Decile 5','Decile 6','Decile 7','Decile 8',
                       'Decile 9','Decile 10','WML')
  rownames(output) = c('r - rf','sigma','SR','sk(m)')
  
  temp[,yrmo := 12*Year + Month]
  val = 12*2013 + 3
  index = temp[,.I[yrmo == val]][10]
  temp = temp[1:index,]
  
  mean_ret = temp[,list(mean_ret = mean(ex_ret)),by = decile]
  mean_ret[,Annualized_ret := mean_ret*12*100]
  output[1,1:10] = mean_ret[,Annualized_ret]
  
  sd_ret = temp[,list(sigma = sd(ex_ret)),by = decile]
  sd_ret[,Annualized_SR := sigma*100*sqrt(12)] 
  
  ret = cbind(mean_ret[,c('decile','Annualized_ret')],sigma = sd_ret[,Annualized_SR])
  ret[,SR := Annualized_ret/sigma]
  output[2,1:10] = sd_ret[,Annualized_SR]  
  
  output[3,1:10] =  ret[,SR]
  
  temp[,log_ret := log(1 + DM_Ret)]
  skew = temp[,list(skm = skewness(log_ret)),by = decile]
  output[4,1:10] = skew[,skm]  
  
  temp[,yrmo := 12*Year + Month]  
  dec10 = temp[decile == 10] 
  colnames(dec10)[7] = 'Ret_10'
  dec10 = subset(dec10, select = c('Year','Month','Ret_10','yrmo'))
  dec1 = temp[decile == 1]  
  colnames(dec1)[7] = 'Ret_1'
  dec1 = subset(dec1, select = c('Year','Month','Ret_1','yrmo'))
  
  WML = merge(dec1,dec10,by.x = 'yrmo',by.y = 'yrmo')  
  WML[,wml_ret := Ret_10 - Ret_1]
  output[1,11] = WML[,mean(wml_ret)*12*100]
  
  output[2,11] = WML[,sd(wml_ret)*sqrt(12)*100]
  
  output[3,11] = output[1,11]/output[2,11]
  output[4,11] = WML[,skewness(wml_ret)]
  return(output)
}

PS3_Q5 = function(CRSP_Stocks_Momentum_returns,DM_Returns,KRF_Returns){
  temp = CRSP_Stocks_Momentum_returns
  temp[,yrmo := 12*Year + Month]
  val = 12*2016 + 12
  index = temp[,.I[yrmo == val]][10]
  temp = temp[1:index,]
  
  correlation = data.frame(matrix(0,2,11))
  colnames(correlation) = c('Decile 1','Decile 2','Decile 3','Decile 4',
                            'Decile 5','Decile 6','Decile 7','Decile 8',
                            'Decile 9','Decile 10','WML')
  rownames(correlation) = c('DM correlation','KRF correlation')
  
  for (i in 1:10) {
    correlation[1,i] = cor(temp[decile == i,DM_Ret],DM_Returns[decile == i,DM_Ret])
    correlation[2,i] = cor(temp[decile == i,KRF_Ret],KRF_Returns[decile == i,KRF_Ret])
  }
  dec10 = temp[decile == 10] 
  colnames(dec10)[4:5] = c('Ret_10_DM','Ret_10_KRF')
  dec10 = subset(dec10, select = c('Year','Month','Ret_10_DM','Ret_10_KRF','yrmo'))
  dec1 = temp[decile == 1]  
  colnames(dec1)[4:5] = c('Ret_1_DM','Ret_1_KRF')
  dec1 = subset(dec1, select = c('Year','Month','Ret_1_DM','Ret_1_KRF','yrmo'))
  
  WML = merge(dec1,dec10,by.x = 'yrmo',by.y = 'yrmo')  
  WML[,wml_ret_DM := Ret_10_DM - Ret_1_DM]
  WML[,wml_ret_KRF := Ret_10_KRF - Ret_1_KRF]
  
  dec10 = DM_Returns[decile == 10]
  colnames(dec10)[4] = 'Ret_10'
  dec10[,yrmo := 12*Year + Month]
  dec10 = subset(dec10, select = c('Year','Month','Ret_10','yrmo'))
  dec1 = DM_Returns[decile == 1]  
  colnames(dec1)[4] = 'Ret_1'
  dec1[,yrmo := 12*Year + Month]
  dec1 = subset(dec1, select = c('Year','Month','Ret_1','yrmo'))
  WML_DM = merge(dec1,dec10,by.x = 'yrmo',by.y = 'yrmo')  
  WML_DM[,wml_ret := Ret_10 - Ret_1]
  
  dec10 = KRF_Returns[decile == 10]
  colnames(dec10)[4] = 'Ret_10'
  dec10[,yrmo := 12*Year + Month]
  dec10 = subset(dec10, select = c('Year','Month','Ret_10','yrmo'))
  dec1 = KRF_Returns[decile == 1]  
  colnames(dec1)[4] = 'Ret_1'
  dec1[,yrmo := 12*Year + Month]
  dec1 = subset(dec1, select = c('Year','Month','Ret_1','yrmo'))
  WML_KRF = merge(dec1,dec10,by.x = 'yrmo',by.y = 'yrmo')  
  WML_KRF[,wml_ret := Ret_10 - Ret_1]
  
  correlation[1,11] = cor(WML$wml_ret_DM,WML_DM$wml_ret)
  correlation[2,11] = cor(WML$wml_ret_KRF,WML_KRF$wml_ret)
  return(correlation)
}
