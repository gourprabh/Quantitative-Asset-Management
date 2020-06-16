library(data.table)
library(lubridate)
library(moments)
library(DataAnalytics)


PS2_Q1 = function(CRSP_Bonds){
  CRSP_Bonds[,MCALDT := as.Date(MCALDT)]
  
  CRSP_Bonds = subset(CRSP_Bonds,select = c('KYCRSPID','MCALDT','TMRETNUA','TMTOTOUT'))
  setorder(CRSP_Bonds,KYCRSPID,MCALDT)
  CRSP_Bonds[is.na(TMRETNUA)]$TMRETNUA = 0
  CRSP_Bonds[is.na(TMTOTOUT)]$TMTOTOUT = 0
  
  year = as.matrix(rep(1926,12),12,1)
  for (i in 1927:2019) {
    year = rbind(year,as.matrix(rep(i,12),12,1))
  }
  
  months = c(rep(seq(1,12,1),94))
  output = data.table(year,months)
  colnames(output)[1] = 'year'
  
  setorder(CRSP_Bonds,KYCRSPID,MCALDT)
  CRSP_Bonds[,lagged_mv := shift(TMTOTOUT,fill= 0,type = 'lag'), by = KYCRSPID]
  #CRSP_Bonds = CRSP_Bonds[!(lagged_mv == 0)]
  setorder(CRSP_Bonds,KYCRSPID,MCALDT)
  
  bond_lag_mv = CRSP_Bonds[,list(Bond_lag_MV = sum(TMTOTOUT)), by = MCALDT]
  setorder(bond_lag_mv,MCALDT)
  
  output = cbind(output,Bond_lag_MV = bond_lag_mv[-1,]$Bond_lag_MV)
  
  bond_ew_ret = CRSP_Bonds[,list(Bond_Ew_Ret = mean(TMRETNUA)),by = MCALDT]
  setorder(bond_ew_ret,MCALDT)
  output = cbind(output,Bond_Ew_Ret = bond_ew_ret[-1,]$Bond_Ew_Ret)
  
  CRSP_Bonds[,MEwt := lagged_mv/sum(lagged_mv),by = MCALDT]
  CRSP_Bonds[is.na(MEwt)]$MEwt = 0
  
  bond_vw_ret = CRSP_Bonds[,list(Bond_Vw_Ret = sum(TMRETNUA*MEwt,na.rm = TRUE)),by = MCALDT]
  output = cbind(output,Bond_Vw_Ret = bond_vw_ret[-1,]$Bond_Vw_Ret)
  colnames(output)[1:2] = c('Year','Month')
  return(output)
}


## QUESTION 2(In the code, I am removing the last year of Monthly_CRSP_Stocks(2019) as the treasury
# data I downloaded from CRSP was until 2018. Please consider this 
#before running if your output does not contain code from 2019)

PS2_Q2 = function(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds, Monthly_CRSP_Riskless){
  Monthly_CRSP_Riskless$caldt = ymd(Monthly_CRSP_Riskless$caldt)
  output_Q2 = cbind(Monthly_CRSP_Stocks,r = Monthly_CRSP_Riskless$t30ret)
  output_Q2 = cbind(output_Q2, Monthly_CRSP_Bonds[-(1117:1128),c('Bond_lag_MV','Bond_Vw_Ret')])
  output_Q2[,Stock_Excess_Vw_Ret:= Stock_Vw_Ret - r]
  output_Q2[,Bond_Excess_Vw_Ret:= Bond_Vw_Ret - r]
  output_Q2 = subset(output_Q2, select = c('Year','Month','Stock_lag_MV',
                                           'Stock_Excess_Vw_Ret','Bond_lag_MV',
                                           'Bond_Excess_Vw_Ret'))
}

## QUESTION 3

PS2_Q3 = function(Monthly_CRSP_universe){
  temp_stock_universe = Monthly_CRSP_universe
  vol_stock = numeric((length(temp_stock_universe$Year) - 36))
  for (i in 1:(length(temp_stock_universe$Year) - 36)){
    vol_stock[i] = sqrt(var(temp_stock_universe[i:(i+35),Stock_Excess_Vw_Ret]))
  }
  
  temp_stock_universe = cbind(temp_stock_universe,vol_stock = c(rep(0,36),vol_stock))
  
  vol_bond = numeric((length(temp_stock_universe$Year) - 36))
  for (i in 1:(length(temp_stock_universe$Year) - 36)){
    vol_bond[i] = sqrt(var(temp_stock_universe[i:(i+35),Bond_Excess_Vw_Ret]))
  }
  
  temp_stock_universe = cbind(temp_stock_universe,vol_bond = c(rep(0,36),vol_bond))
  temp_stock_universe[, vol_stock_inv := 1/vol_stock]
  temp_stock_universe[,vol_bond_inv := 1/vol_bond]
  
  temp_stock_universe[,Stock_lag_MV := Stock_lag_MV * 1000]
  temp_stock_universe[,Bond_lag_MV := Bond_lag_MV * 1000000]
  temp_stock_universe[,total_mkt_val := Stock_lag_MV + Bond_lag_MV]
  temp_stock_universe[,stock_wt := Stock_lag_MV/total_mkt_val]
  temp_stock_universe[,bond_wt := Bond_lag_MV/total_mkt_val]
  
  temp_stock_universe[,Excess_Vw_Ret := stock_wt*Stock_Excess_Vw_Ret + bond_wt*Bond_Excess_Vw_Ret]
  temp_stock_universe[,Excess_60_40_Ret := 0.6*Stock_Excess_Vw_Ret + 0.4*Bond_Excess_Vw_Ret]
  
  
  vol_mkt_portfolio = numeric((length(temp_stock_universe$Year) - 36))
  for (i in 1:(length(temp_stock_universe$Year) - 36)){
    vol_mkt_portfolio[i] = sqrt(var(temp_stock_universe[i:(i+35),Excess_Vw_Ret]))
  }
  
  
  temp_stock_universe = cbind(temp_stock_universe,vol_mkt_portfolio = c(rep(0,36),vol_mkt_portfolio))
  temp_stock_universe = temp_stock_universe[!(is.infinite(vol_stock_inv)),]
  temp_unlevered_rp_stock_universe = subset(temp_stock_universe, select = c('Year','Month','Stock_lag_MV',
                                                                            'Stock_Excess_Vw_Ret','Bond_lag_MV',
                                                                            'Bond_Excess_Vw_Ret','Excess_Vw_Ret',
                                                                            'Excess_60_40_Ret','vol_stock_inv'
                                                                            ,'vol_bond_inv',
                                                                            'vol_mkt_portfolio','vol_stock','vol_bond'))
  
  temp_unlevered_rp_stock_universe[,stock_wt := vol_stock_inv/(vol_bond_inv + vol_stock_inv)]
  temp_unlevered_rp_stock_universe[,bond_wt := vol_bond_inv/(vol_bond_inv + vol_stock_inv)]
  temp_unlevered_rp_stock_universe[,Unlevered_k := 1/(vol_bond_inv + vol_stock_inv)]
  temp_unlevered_rp_stock_universe[,Excess_Unlevered_RP_Ret := stock_wt*Stock_Excess_Vw_Ret + bond_wt*Bond_Excess_Vw_Ret]
  
  temp_levered_rp_stock_universe = subset(temp_unlevered_rp_stock_universe, select = c('Year','Month','Stock_lag_MV',
                                                                                       'Stock_Excess_Vw_Ret','Bond_lag_MV',
                                                                                       'Bond_Excess_Vw_Ret','Excess_Vw_Ret',
                                                                                       'Excess_60_40_Ret','vol_stock_inv'
                                                                                       ,'vol_bond_inv','Excess_Unlevered_RP_Ret',
                                                                                       'vol_mkt_portfolio','vol_stock','vol_bond','Unlevered_k'
  ))
  

  sd1 = sd((temp_levered_rp_stock_universe[,Stock_Excess_Vw_Ret]*temp_levered_rp_stock_universe[,vol_stock_inv])
          + (temp_levered_rp_stock_universe[,Bond_Excess_Vw_Ret]*temp_levered_rp_stock_universe[,vol_bond_inv]))
  sd2 = sd(temp_levered_rp_stock_universe[,Excess_Vw_Ret])
  Levered_k = sd2/sd1
  Excess_levered_RP_Ret = numeric(length(temp_levered_rp_stock_universe$Year))
  for(i in 1:length(temp_levered_rp_stock_universe$Year)){
    Excess_levered_RP_Ret[i] = Levered_k*(temp_levered_rp_stock_universe$vol_stock_inv[i]*
                                            temp_levered_rp_stock_universe$Stock_Excess_Vw_Ret[i] + 
                                            temp_levered_rp_stock_universe$vol_bond_inv[i]*temp_levered_rp_stock_universe$Bond_Excess_Vw_Ret[i])
  }
  temp_levered_rp_stock_universe = cbind(temp_levered_rp_stock_universe,Excess_levered_RP_Ret = Excess_levered_RP_Ret)
  temp_levered_rp_stock_universe[,Levered_k := Levered_k]
  Port_Res = subset(temp_levered_rp_stock_universe, select = c('Year','Month',
                                                               'Stock_Excess_Vw_Ret','Bond_Excess_Vw_Ret',
                                                               'Excess_Vw_Ret',
                                                               'Excess_60_40_Ret',
                                                               'vol_stock_inv',
                                                               'vol_bond_inv','Unlevered_k',
                                                               'Excess_Unlevered_RP_Ret',
                                                               'Levered_k','Excess_levered_RP_Ret'))
  
  colnames(Port_Res)[7:8] = c('Stock_inverse_sigma_hat','Bond_inverse_sigma_hat')
  return(Port_Res)
}  

## QUESTION 4 (Here in my code I am removing some rows of data considering that the input is 
#from 1929 to 2018). Please check this before running your data.

PS2_Q4 = function(Port_Res){
  output_Q4 = data.frame(matrix(0,6,6))
  rownames(output_Q4) = c('CRSP stocks','CRSP bonds','Value-weighted portfolio',
                          '60/40 portfolio','unlevered RP','levered RP')
  colnames(output_Q4) = c('Annualized Mean','t-stat of Annualized Mean',
                          'Annualized Standard Deviation','Annualized Sharpe Ratio',
                          'Skewness','Excess Kurtosis')
  temp_port_res = Port_Res[1:978,]
  temp_port_res = temp_port_res[-(1:12),]
  output_Q4[1,1] = temp_port_res[,mean(Stock_Excess_Vw_Ret)]*12*100
  output_Q4[1,2] = (temp_port_res[,mean(Stock_Excess_Vw_Ret)])*sqrt(length(temp_port_res$Year) - 1)/temp_port_res[,sqrt(var(Stock_Excess_Vw_Ret))]
  output_Q4[1,3] = temp_port_res[,sqrt(var(Stock_Excess_Vw_Ret))]*sqrt(12)*100
  output_Q4[1,4] = output_Q4[1,1]/(100*sqrt(12)*temp_port_res[,sqrt(var(Stock_Excess_Vw_Ret))])
  output_Q4[1,5] = skewness(temp_port_res[,Stock_Excess_Vw_Ret])
  output_Q4[1,6] = kurtosis(temp_port_res[,Stock_Excess_Vw_Ret]) - 3
  
  output_Q4[2,1] = temp_port_res[,mean(Bond_Excess_Vw_Ret)]*12*100 
  output_Q4[2,2] = temp_port_res[,mean(Bond_Excess_Vw_Ret)]*sqrt(length(temp_port_res$Year) - 1)/(temp_port_res[,sqrt(var(Bond_Excess_Vw_Ret))])
  output_Q4[2,3] = (sqrt(12)*temp_port_res[,sqrt(var(Bond_Excess_Vw_Ret))])*100
  output_Q4[2,4] = output_Q4[2,1]/(100*sqrt(12)*temp_port_res[,sqrt(var(Bond_Excess_Vw_Ret))])
  output_Q4[2,5] = skewness(temp_port_res[,Bond_Excess_Vw_Ret])
  output_Q4[2,6] = kurtosis(temp_port_res[,Bond_Excess_Vw_Ret]) - 3
  
  output_Q4[3,1] = temp_port_res[,mean(Excess_Vw_Ret)]*12*100
  output_Q4[3,2] = temp_port_res[,mean(Excess_Vw_Ret)]*sqrt(length(temp_port_res$Year) - 1)/(temp_port_res[,sqrt(var(Excess_Vw_Ret))])
  output_Q4[3,3] = (sqrt(12)*temp_port_res[,sqrt(var(Excess_Vw_Ret))])*100
  output_Q4[3,4] = output_Q4[3,1]/(100*sqrt(12)*temp_port_res[,sqrt(var(Excess_Vw_Ret))])
  output_Q4[3,5] = skewness(temp_port_res[,Excess_Vw_Ret])
  output_Q4[3,6] = kurtosis(temp_port_res[,Excess_Vw_Ret]) - 3
  
  output_Q4[4,1] = temp_port_res[,mean(Excess_60_40_Ret)]*12*100 
  output_Q4[4,2] = temp_port_res[,mean(Excess_60_40_Ret)]*sqrt(length(temp_port_res$Year)-1)/(temp_port_res[,sqrt(var(Excess_60_40_Ret))])
  output_Q4[4,3] = (sqrt(12)*temp_port_res[,sqrt(var(Excess_60_40_Ret))])*100
  output_Q4[4,4] = output_Q4[4,1]/(100*sqrt(12)*temp_port_res[,sqrt(var(Excess_60_40_Ret))])
  output_Q4[4,5] = skewness(temp_port_res[,Excess_60_40_Ret])
  output_Q4[4,6] = kurtosis(temp_port_res[,Excess_60_40_Ret]) - 3
  
  output_Q4[5,1] = temp_port_res[,mean(Excess_Unlevered_RP_Ret)]*12 *100 
  output_Q4[5,2] = temp_port_res[,mean(Excess_Unlevered_RP_Ret)]*sqrt(length(temp_port_res$Year) - 1)/(temp_port_res[,sqrt(var(Excess_Unlevered_RP_Ret))])
  output_Q4[5,3] = (sqrt(12)*temp_port_res[,sqrt(var(Excess_Unlevered_RP_Ret))])*100
  output_Q4[5,4] = output_Q4[5,1]/(100*sqrt(12)*temp_port_res[,sqrt(var(Excess_Unlevered_RP_Ret))])
  output_Q4[5,5] = skewness(temp_port_res[,Excess_Unlevered_RP_Ret])
  output_Q4[5,6] = kurtosis(temp_port_res[,Excess_Unlevered_RP_Ret]) - 3
  
  output_Q4[6,1] = temp_port_res[,mean(Excess_levered_RP_Ret)]*12 *100 
  output_Q4[6,2] = temp_port_res[,mean(Excess_levered_RP_Ret)]*sqrt(length(temp_port_res$Year) - 1)/(temp_port_res[,sqrt(var(Excess_levered_RP_Ret))])
  output_Q4[6,3] = (sqrt(12)*temp_port_res[,sqrt(var(Excess_levered_RP_Ret))])*100
  output_Q4[6,4] = output_Q4[6,1]/(100*sqrt(12)*temp_port_res[,sqrt(var(Excess_levered_RP_Ret))])
  output_Q4[6,5] = skewness(temp_port_res[,Excess_levered_RP_Ret])
  output_Q4[6,6] = kurtosis(temp_port_res[,Excess_levered_RP_Ret]) - 3
  
  return(output_Q4)
}



