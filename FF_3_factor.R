library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(moments)
library(ggplot2)

####### CRSP data (1970-2019)
CRSP_input = fread('/Users/gourprabh/Desktop/Documents/Spring/QAM/Week 7/PS4_data/CRSP_data.csv')
####### Compustat data - (1970-2019)
COMPUSTAT_input = fread('/Users/gourprabh/Desktop/Documents/Spring/QAM/Week 7/PS4_data/Compustat_data.csv')

####### Compustat Pension data
COMPUSTAT_pension = fread('/Users/gourprabh/Desktop/Documents/Spring/QAM/Week 7/PS4_data/COMPUSTAT_pension_data.csv')

####### CRSP COMPUSTAT linking table
link_CRSP_COMPUSTAT = fread('/Users/gourprabh/Desktop/Documents/Spring/QAM/Week 7/PS4_data/Link_CRSP_COMPUSTAT.csv')


historical_BE = read.table('/Users/gourprabh/Desktop/Documents/Spring/QAM/Week 7/PS4_data/DFF_BE_With_Nonindust.txt')

###################### Linking pension data with Compustat

temp_pension = COMPUSTAT_pension[(is.na(prba)),prba := 0]
temp_pension[,year := year(datadate)]
temp_pension = subset(temp_pension,select = c('gvkey','year','prba'))
temp_pension = unique(temp_pension, by = c('gvkey','year','prba'))
temp_pension = unique(temp_pension, by = c('gvkey','year'))
COMPUSTAT_input[,year := year(as.Date(datadate))]
temp_merged = merge(COMPUSTAT_input,temp_pension,by.x = c('gvkey','year'),
                    by.y = c('gvkey','year'),all.x = TRUE)

## Computing book value of equity

# Shareholder's Equity
temp_merged[,SHE := seq]
#temp_merged[,SHE2 := ceq + pstk]
#temp_merged[,SHE3 := at - lt - mib]
temp_merged[is.na(SHE), SHE := ceq + pstk]
temp_merged[is.na(SHE), SHE := at - lt - mib]
temp_merged[is.na(SHE), SHE := at - lt]

# Deferred taxes and investment tax credit 
temp_merged[,DT := txditc]
temp_merged[,DT2 := itcb + txdb]
temp_merged[is.na(DT2),DT2 := itcb]
temp_merged[is.na(DT2),DT2 := txdb]
temp_merged[is.na(DT),DT := DT2]

# Book value of preferred stock
temp_merged[,PS := pstkrv]
temp_merged[is.na(PS),PS := pstkl]
temp_merged[is.na(PS),PS := pstk]
temp_merged[,PS := -PS]

temp_merged[,prba := -prba]
# Calculating final value
#temp_merged[!(is.na(SHE)),BE := sum(SHE,-PS,DT,-prba,na.rm = TRUE),by = 1:nrow(temp_merged)]
temp_merged = temp_merged[!(is.na(SHE))]
temp_merged[, BE := sum(SHE,-PS,DT,-prba,na.rm = TRUE), by=1:nrow(temp_merged)]

COMPUSTAT = subset(temp_merged,select = c('gvkey','fyear','datadate',
                                           'BE'))

################################# Adjusting CRSP

temp_CRSP = CRSP_input
temp_CRSP[,date := as.Date(date)]
temp_CRSP = temp_CRSP[SHRCD %in% c(10,11)]
temp_CRSP = temp_CRSP[EXCHCD %in% c(1,2,3)]
temp_CRSP$PRC = abs(temp_CRSP$PRC)
temp_CRSP[RET %in% c('C',-66.0,-77.0,-88.0,-99.0)]$RET = '0'
temp_CRSP[RETX %in% c('C',-66.0,-77.0,-88.0,-99.0)]$RETX = '0'
temp_CRSP$RET = as.numeric(temp_CRSP$RET)
temp_CRSP$DLRET = as.numeric(temp_CRSP$DLRET)
temp_CRSP$RETX = as.numeric(temp_CRSP$RETX)
temp_CRSP$DLRETX = as.numeric(temp_CRSP$DLRETX)
temp_CRSP$RET[is.na(temp_CRSP$RET)] = 0
temp_CRSP$DLRET[is.na(temp_CRSP$DLRET)] = 0
temp_CRSP$RETX[is.na(temp_CRSP$RETX)] = 0
temp_CRSP$DLRETX[is.na(temp_CRSP$DLRETX)] = 0
#input = input[SHROUT != 0]

## Creating lagged market equity
temp_CRSP = temp_CRSP[,Mkt_eq := PRC * SHROUT]
setorder(temp_CRSP,PERMNO,date)
temp_CRSP[,lagged_mkt_cap := shift(Mkt_eq), by = PERMNO]

## Checking validity of the shifted data
temp_CRSP[,Year := year(date)]
temp_CRSP[,Month := month(date)]
temp_CRSP[,YrMo := 12*Year + Month]
temp_CRSP[, prev_yrmo := shift(YrMo),by = PERMNO]
temp_CRSP[, Valid_lag := YrMo == (prev_yrmo + 1)]
temp_CRSP = temp_CRSP[Valid_lag == T & !(is.na(lagged_mkt_cap))]

## Calculating total return

temp_CRSP = temp_CRSP[,tot_ret := (1 +  RET)*(1 + DLRET) - 1]
temp_CRSP = temp_CRSP[,tot_retX := (1 +  RETX)*(1 + DLRETX) - 1]
temp_CRSP[is.na(tot_retX),tot_retX := tot_ret]

## Value-weighted return of each PERMCO
temp_CRSP[,PERMNO_weight := lagged_mkt_cap/sum(lagged_mkt_cap),by = c('PERMCO','date')]
temp_CRSP[,tot_ret := sum(PERMNO_weight*tot_ret), by = c('PERMCO','date')]
temp_CRSP[,tot_retX := sum(PERMNO_weight*tot_ret), by = c('PERMCO','date')]
temp_CRSP[,lagged_mkt_cap := sum(lagged_mkt_cap), by = c('PERMCO','date')]
temp_CRSP = unique(temp_CRSP[,.(PERMCO,EXCHCD,SHRCD,date,PRC,lagged_mkt_cap,tot_ret,tot_retX,
                                Mkt_eq)],
                   by = c('PERMCO','date'))
setorder(temp_CRSP,PERMCO,date)

############################ Linking CRSP with the linking table

# CLeaning the data type
temp_link_table = link_CRSP_COMPUSTAT
temp_link_table[,LINKDT := as.Date(as.character(LINKDT),"%Y%m%d")]
temp_link_table[,LINKENDDT := as.Date(as.character(LINKENDDT),"%Y%m%d")]
temp_link_table[is.na(LINKENDDT),LINKENDDT := as.Date('2020-01-31')]
temp_link_table = temp_link_table[LINKTYPE %in% c('LU','LC')]

# temp_link_table1 = temp_link_table[, .(year = seq(start_yr,end_yr,by = 1)), 
#    by=c(names(temp_link_table))]
# temp_link_table1 = temp_link_table1[,-(9:12)]
# temp_CRSP1 = temp_CRSP
# temp_CRSP1[,year := year(date)]

merged_CRSP_linking_table = merge(temp_CRSP,temp_link_table,by.x = c('PERMCO'),
                   by.y = c('LPERMCO'),all.x = TRUE,allow.cartesian = TRUE)

#merged_CRSP_linking_table[,LINKDT := as.Date(as.character(LINKDT),"%Y%m%d")]
#merged_CRSP_linking_table[,LINKENDDT := as.Date(as.character(LINKENDDT),"%Y%m%d")]
## Checking if date is within range for the merged data
merged_CRSP_linking_table = merged_CRSP_linking_table[(is.na(LINKDT) | date >= LINKDT) &
                                                        (is.na(LINKENDDT) | date <= LINKENDDT)]

setorder(merged_CRSP_linking_table,gvkey,date)

## Cleaning by LC
merged_CRSP_linking_table[,prob := .N > 1,by = .(PERMCO,date)]
merged_CRSP_linking_table[, Good_match := sum(LINKTYPE == 'LC'), by = .(PERMCO,date)]
merged_CRSP_linking_table = merged_CRSP_linking_table[!(prob == T & Good_match > 1
                                                        & LINKTYPE != 'LC')]

## Cleaning by Linkprim (keep P)
merged_CRSP_linking_table[,prob := .N > 1,by = .(PERMCO,date)]
merged_CRSP_linking_table[, Good_match := sum(LINKPRIM == 'P'), by = .(PERMCO,date)]
merged_CRSP_linking_table = merged_CRSP_linking_table[!(prob == T & Good_match > 1
                                                        & LINKPRIM != 'P')]

## Cleaning by Linkprim (keep C)
merged_CRSP_linking_table[,prob := .N > 1,by = .(PERMCO,date)]
merged_CRSP_linking_table[, Good_match := sum(LINKPRIM == 'C'), by = .(PERMCO,date)]
merged_CRSP_linking_table = merged_CRSP_linking_table[!(prob == T & Good_match > 1
                                                        & LINKPRIM != 'C')]

## Cleaning by liid (keep 1)
merged_CRSP_linking_table[,prob := .N > 1,by = .(PERMCO,date)]
merged_CRSP_linking_table[, Good_match := sum(LIID == 1), by = .(PERMCO,date)]
merged_CRSP_linking_table = merged_CRSP_linking_table[!(prob == T & Good_match > 1
                                                        & LIID != 1)]

## USing current link
merged_CRSP_linking_table[,prob := .N > 1,by = .(PERMCO,date)]
merged_CRSP_linking_table[, Good_match := sum(is.na(LINKENDDT)), by = .(PERMCO,date)]
merged_CRSP_linking_table = merged_CRSP_linking_table[!(prob == T & Good_match > 1
                                                        & !is.na(LINKENDDT))]

## USing link that has been around longest
merged_CRSP_linking_table[,prob := .N > 1,by = .(PERMCO,date)]
merged_CRSP_linking_table[, Good_match := NULL]
merged_CRSP_linking_table[is.na(LINKENDDT), LINKENDDT := as.Date('2020-12-31','%Y-%m-%d')]
merged_CRSP_linking_table[,Date_diff := as.integer(LINKENDDT - LINKDT)]
setorder(merged_CRSP_linking_table,PERMCO,date,Date_diff)
merged_CRSP_linking_table = merged_CRSP_linking_table[prob == T,
                                                        Good_match := Date_diff == Date_diff[.N],
                                                      by = .(PERMCO,date)]
merged_CRSP_linking_table = merged_CRSP_linking_table[!(prob == T & Good_match != T
                                                        )]

## USing the GVKEY that has been around longest
merged_CRSP_linking_table[,prob := .N > 1,by = .(PERMCO,date)]
merged_CRSP_linking_table[, Good_match := NULL]
setorder(merged_CRSP_linking_table,gvkey,LINKDT)
merged_CRSP_linking_table = merged_CRSP_linking_table[prob == T,
                                                      Start_date := LINKDT[.1],
                                                      by = .(gvkey)]
setorder(merged_CRSP_linking_table,gvkey,LINKENDDT)
merged_CRSP_linking_table[prob == T,End_date := LINKENDDT[.N],
                                      by = .(gvkey)]
merged_CRSP_linking_table[,Date_diff := as.integer(End_date - Start_date)]
setorder(merged_CRSP_linking_table,PERMCO,date, Date_diff)
merged_CRSP_linking_table = merged_CRSP_linking_table[prob == T,
                                                      Good_match := Date_diff == Date_diff[.N],
                                                      by = .(PERMCO,date)]
merged_CRSP_linking_table = merged_CRSP_linking_table[!(prob == T & Good_match != T
)]

## Using samller gvkey
setorder(merged_CRSP_linking_table,PERMCO,date,gvkey)
merged_CRSP_linking_table = unique(merged_CRSP_linking_table,by = c('PERMCO','date'))

##################################################### Merging CRSP with Compustat
temp_merged_CRSP = subset(merged_CRSP_linking_table,select = c('PERMCO','gvkey','LPERMNO','EXCHCD',
                                                          'SHRCD','date','PRC',
                                           'lagged_mkt_cap','tot_ret', 'tot_retX','Mkt_eq'))
temp_merged_CRSP[,year := year(date)]

CRSP_COMPUSTAT = merge(temp_merged_CRSP,COMPUSTAT_input,by.x = c('gvkey','year'),
                     by.y = c('gvkey','fyear'),all.x = TRUE)
CRSP_COMPUSTAT = CRSP_COMPUSTAT[!(is.na(LPERMNO))]
CRSP_COMPUSTAT[,Month := month(date)]

CRSP_COMPUSTAT[,yrmo := 12*year + Month]
setorder(CRSP_COMPUSTAT,LPERMNO,year)

setwd('/Users/gourprabh/Desktop/Documents/Spring/QAM/Final_project')
write.csv(CRSP_COMPUSTAT,'CRSP_COMPUSTAT.csv')
######################### Calculating BM for BM_ret based on BE and ME of Dec of t-1
patent_crsp_compustat = merge(CRSP_COMPUSTAT,temp_input, by.x = c('LPERMNO','year'),
                              by.y = c('permno','year'),all.x = TRUE)
patent_crsp_compustat[is.na(patent_count),patent_count := 0]
patent_crsp_compustat[is.na(cit_count),cit_count := 0]
patent_crsp_compustat[,no_of_cases := NULL]
setorder(patent_crsp_compustat,gvkey,year)
patent_crsp_compustat[,lag_pc := shift(patent_count),by = c('gvkey')]
patent_crsp_compustat[,lag_cit := shift(cit_count),by = c('gvkey')]
patent_crsp_compustat[is.na(lag_pc),lag_pc := 0]
patent_crsp_compustat[is.na(lag_cit),lag_cit := 0]
mkt_eq =  patent_crsp_compustat[Month == 12,list(ME = Mkt_eq),by = c('gvkey','year')]
setkey(mkt_eq,gvkey)
setkey(patent_crsp_compustat,gvkey)
BE_calc =  patent_crsp_compustat[Month == 12,list(BE = BE),by = c('gvkey','year')]
setkey(BE_calc,gvkey)

gvkey_BM = merge(mkt_eq,BE_calc,by.x = c('gvkey','year'),
                 by.y = c('gvkey','year'),all.x = TRUE,all.y = TRUE,
                 allow.cartesian = TRUE)
gvkey_BM[,BM := BE/ME]
mkt_eq1 = unique(gvkey_BM,by = c('gvkey','year'))
#temp_mkt_eq = mkt_eq1 %>% slice(rep(row_number(), 12))
temp_mkt_eq = mkt_eq1[rep(1:.N,12)]
#temp_mkt_eq[rep(row.names(temp_mkt_eq), 11), 1:5]
setorder(temp_mkt_eq,gvkey,year)
temp_mkt_eq = data.table(temp_mkt_eq)
temp_mkt_eq[,yrmo := seq((year+1)*12 + 7,(year+1)*12 + 18,1),by = c('gvkey','year')]

temp_final_output1 = left_join(patent_crsp_compustat,temp_mkt_eq,by = c('gvkey','yrmo'))
temp_final_output1 = unique(temp_final_output1, by = c('gvkey','PERMCO',
                                                       'EXCHCD','date','tot_ret',
                                                       'tot_retX','Mkt_eq','datadate','BM',
                                                       'lag_pc','lag_cit'))

##################################### Creating BM portfolio
CRSP_COMPUSTAT_BM = subset(temp_final_output1,select = c('gvkey','PERMCO',
                                                             'year.x','EXCHCD',
                                                             'Month','date',
                                                             'tot_ret','tot_retX','BM',
                                                             'datadate','Mkt_eq','yrmo',
                                                         'lagged_mkt_cap','patent_count','cit_count'))
range = seq((1975*12 + 7),(2019*12 + 12),1)
CRSP_COMPUSTAT_BM = data.table(CRSP_COMPUSTAT_BM)
#CRSP_COMPUSTAT_BM[,yrmo := 12*year.x + Month]
CRSP_COMPUSTAT_BM = CRSP_COMPUSTAT_BM[yrmo %in% range]
temp_BM = CRSP_COMPUSTAT_BM[Month == 7]
temp_BM = temp_BM[!(is.na(BM))]
temp_BM = temp_BM[BM > 0]
temp_BM[,BM_new := (BM - mean(BM))/sd(BM),by = yrmo]
temp_BM[,pc_new := (patent_count - mean(patent_count))/sd(patent_count),by = yrmo]
temp_BM[,cit_new := (cit_count - mean(cit_count))/sd(cit_count),by = yrmo]
temp_BM = temp_BM[year.x %in% seq(1975,2014,1)]
temp_BM[,new_val_factor := BM_new + pc_new + cit_new]
BM_decile = temp_BM[EXCHCD == 1,quantile(new_val_factor,
                                       probs = seq(0,1,0.1)),by = yrmo]
BM_yrmo = temp_BM[,unique(yrmo)]
temp_BM = subset(temp_BM,select = c('gvkey','new_val_factor','yrmo'))
temp1 = data.table(0,0,0,0)

colnames(temp1) = c('gvkey','new_val_factor','yrmo','BM_decile')
for (i in BM_yrmo) {
  temp2 = temp_BM[yrmo == i]
  temp2[,BM_decile := findInterval(new_val_factor, vec = BM_decile[yrmo == i,V1],
                                    left.open = TRUE,all.inside = TRUE)]
  temp1 = rbind(temp1,temp2)
}
temp1 = temp1[-1,]
setorder(temp1,gvkey,yrmo)
temp1[,id := 1:nrow(temp1)]
#temp2 = temp1 %>% slice(rep(row_number(), 12))
temp2 = temp1[rep(1:.N,12)]
setorder(temp2,gvkey,yrmo)
temp2 = data.table(temp2)
temp2[,temp_yrmo := seq(yrmo[1],yrmo[1] + 11,1),by = c('gvkey','id')]

temp2 = subset(temp2, select = c('gvkey','BM_decile','temp_yrmo'))
CRSP_COMPUSTAT_BM = merge(CRSP_COMPUSTAT_BM,temp2,by.x = c('gvkey','yrmo'),
                          by.y = c('gvkey','temp_yrmo'))

BM_ret = data.table(0,0,0)
colnames(BM_ret) = c('yrmo','decile','BM_ret')
yrmo = CRSP_COMPUSTAT_BM[,unique(yrmo)]
for (i in yrmo) {
  temp = CRSP_COMPUSTAT_BM[yrmo == i]
  temp[,BM_wt := lagged_mkt_cap/sum(lagged_mkt_cap,na.rm = TRUE),
        by = BM_decile]
  temp_ret_1 = temp[,list(BM_ret = sum(tot_ret*BM_wt,na.rm = TRUE)),by = BM_decile]
  setorder(temp_ret_1,BM_decile)
  colnames(temp_ret_1)[1] = c('decile')
  temp_ret_1[,yrmo := i]
  BM_ret = rbind(BM_ret,temp_ret_1)
}
BM_ret = BM_ret[-1]
casted_BM_ret = dcast(BM_ret,yrmo ~ decile, value.var =  'BM_ret')
casted_BM_ret = data.table(casted_BM_ret)
casted_BM_ret[,Year := floor(yrmo/12)]
casted_BM_ret[,Month := yrmo%%12]
casted_BM_ret[Month == 0, Year := Year - 1]
casted_BM_ret[Month == 0, Month := Month + 12]

final_BM_ret = subset(casted_BM_ret, select = c('Year','Month',1,2,3,4,5,6,7,8,9,10))

############################### Creating Size portfolio
CRSP_COMPUSTAT_size = subset(temp_final_output1,select = c('gvkey','PERMCO',
                                                         'year.x','EXCHCD',
                                                         'Month','date',
                                                         'tot_ret','tot_retX','BM',
                                                         'datadate','Mkt_eq','yrmo',
                                                         'lagged_mkt_cap'))
range = seq((1972*12),(2019*12 + 12),1)
CRSP_COMPUSTAT_size = data.table(CRSP_COMPUSTAT_size)
#CRSP_COMPUSTAT_BM[,yrmo := 12*year.x + Month]
CRSP_COMPUSTAT_size = CRSP_COMPUSTAT_size[yrmo %in% range]
temp_size = CRSP_COMPUSTAT_size[Month == 7]
temp_size = temp_size[!(is.na(lagged_mkt_cap))]
size_decile = temp_size[EXCHCD == 1,quantile(lagged_mkt_cap,
                                         probs = seq(0,1,0.1)),by = yrmo]
size_yrmo = temp_size[,unique(yrmo)]
temp_size = subset(temp_size,select = c('gvkey','lagged_mkt_cap','yrmo'))
temp1 = data.table(0,0,0,0)

colnames(temp1) = c('gvkey','lagged_mkt_cap','yrmo','size_decile')
for (i in size_yrmo) {
  temp2 = temp_size[yrmo == i]
  temp2[,size_decile := findInterval(lagged_mkt_cap, vec = size_decile[yrmo == i,V1],
                                   left.open = TRUE,all.inside = TRUE)]
  temp1 = rbind(temp1,temp2)
}
temp1 = temp1[-1,]
setorder(temp1,gvkey,yrmo)
temp1[,id := 1:nrow(temp1)]
temp2 = temp1 %>% slice(rep(row_number(), 12))
setorder(temp2,gvkey,yrmo)
temp2 = data.table(temp2)
temp2[,temp_yrmo := seq(yrmo[1],yrmo[1] + 11,1),by = c('gvkey','id')]

temp2 = subset(temp2, select = c('gvkey','size_decile','temp_yrmo'))
CRSP_COMPUSTAT_size = merge(CRSP_COMPUSTAT_size,temp2,by.x = c('gvkey','yrmo'),
                          by.y = c('gvkey','temp_yrmo'))

size_ret = data.table(0,0,0)
colnames(size_ret) = c('yrmo','decile','size_ret')
yrmo = CRSP_COMPUSTAT_size[,unique(yrmo)]
for (i in yrmo) {
  temp = CRSP_COMPUSTAT_size[yrmo == i]
  temp[,size_wt := lagged_mkt_cap/sum(lagged_mkt_cap,na.rm = TRUE),
       by = size_decile]
  temp_ret_1 = temp[,list(size_ret = sum(tot_ret*size_wt,na.rm = TRUE)),by = size_decile]
  setorder(temp_ret_1,size_decile)
  colnames(temp_ret_1)[1] = c('decile')
  temp_ret_1[,yrmo := i]
  size_ret = rbind(size_ret,temp_ret_1)
}
size_ret = size_ret[-1]
casted_size_ret = dcast(size_ret,yrmo ~ decile, value.var =  'size_ret')
casted_size_ret = data.table(casted_size_ret)
casted_size_ret[,Year := floor(yrmo/12)]
casted_size_ret[,Month := yrmo%%12]
casted_size_ret[Month == 0, Year := Year - 1]
casted_size_ret[Month == 0, Month := Month + 12]

final_size_ret = subset(casted_size_ret, select = c('Year','Month',1,2,3,4,5,6,7,8,9,10))

############################## HML - SMB RETURNS


CRSP_COMPUSTAT_SMB_HML = subset(temp_final_output1,select =c('gvkey','PERMCO',
                                                             'year.x','EXCHCD',
                                                             'Month','date',
                                                             'tot_ret','tot_retX','BM',
                                                             'datadate','Mkt_eq','yrmo',
                                                             'lagged_mkt_cap','lag_pc','lag_cit'))

range = seq((1975*12 + 7),(2014*12 + 12),1)
CRSP_COMPUSTAT_SMB_HML = data.table(CRSP_COMPUSTAT_SMB_HML)
#CRSP_COMPUSTAT_BM[,yrmo := 12*year.x + Month]
CRSP_COMPUSTAT_SMB_HML = CRSP_COMPUSTAT_SMB_HML[yrmo %in% range]
temp_BM = CRSP_COMPUSTAT_SMB_HML[Month == 7]
temp_BM = temp_BM[!(is.na(BM))]
temp_BM = temp_BM[BM > 0]
temp_BM[,BM_new := (BM - mean(BM))/sd(BM),by = yrmo]
temp_BM[,pc_new := (lag_pc - mean(lag_pc))/sd(lag_pc),by = yrmo]
temp_BM[,cit_new := (lag_cit - mean(lag_cit))/sd(lag_cit),by = yrmo]
temp_BM[,new_val_factor := BM_new + pc_new + cit_new]
BM_decile = temp_BM[EXCHCD == 1,quantile(new_val_factor,
                                         probs = c(0,0.3,0.7,1)),by = yrmo]
BM_yrmo = temp_BM[,unique(yrmo)]
temp_BM = subset(temp_BM,select = c('gvkey','new_val_factor','yrmo'))
temp1 = data.table(0,0,0,0)

colnames(temp1) = c('gvkey','new_val_factor','yrmo','BM_decile')
for (i in BM_yrmo) {
  temp2 = temp_BM[yrmo == i]
  temp2[,BM_decile := findInterval(new_val_factor, vec = BM_decile[yrmo == i,V1],
                                   left.open = TRUE,all.inside = TRUE)]
  temp1 = rbind(temp1,temp2)
}
temp1 = temp1[-1,]
setorder(temp1,gvkey,yrmo)
temp1[,id := 1:nrow(temp1)]
#temp2 = temp1 %>% slice(rep(row_number(), 12))
temp2 = temp1[rep(1:.N,12)]
setorder(temp2,gvkey,yrmo)
temp2 = data.table(temp2)
temp2[,temp_yrmo := seq(yrmo[1],yrmo[1] + 11,1),by = c('gvkey','id')]

temp2 = subset(temp2, select = c('gvkey','BM_decile','temp_yrmo'))
colnames(temp2)[2] = 'HMB_portfolio'
CRSP_COMPUSTAT_SMB_HML = merge(CRSP_COMPUSTAT_SMB_HML,temp2,by.x = c('gvkey','yrmo'),
                          by.y = c('gvkey','temp_yrmo'))

temp_size = CRSP_COMPUSTAT_SMB_HML[Month == 7]
temp_size = temp_size[!(is.na(lagged_mkt_cap))]
temp_size = temp_size[!(is.na(BM))]
temp_size = temp_size[BM > 0]
size_decile = temp_size[EXCHCD == 1,quantile(lagged_mkt_cap,
                                             probs = c(0,0.5,1)),by = yrmo]
size_yrmo = temp_size[,unique(yrmo)]
temp_size = subset(temp_size,select = c('gvkey','lagged_mkt_cap','yrmo'))
temp1 = data.table(0,0,0,0)

colnames(temp1) = c('gvkey','lagged_mkt_cap','yrmo','size_decile')
for (i in size_yrmo) {
  temp2 = temp_size[yrmo == i]
  temp2[,size_decile := findInterval(lagged_mkt_cap, vec = size_decile[yrmo == i,V1],
                                     left.open = TRUE,all.inside = TRUE)]
  temp1 = rbind(temp1,temp2)
}
temp1 = temp1[-1,]
setorder(temp1,gvkey,yrmo)
temp1[,id := 1:nrow(temp1)]
#temp2 = temp1 %>% slice(rep(row_number(), 12))
temp2 = temp1[rep(1:.N,12)]
setorder(temp2,gvkey,yrmo)
temp2 = data.table(temp2)
temp2[,temp_yrmo := seq(yrmo[1],yrmo[1] + 11,1),by = c('gvkey','id')]

temp2 = subset(temp2, select = c('gvkey','size_decile','temp_yrmo'))
colnames(temp2)[2] = 'SMB_portfolio'
CRSP_COMPUSTAT_SMB_HML = merge(CRSP_COMPUSTAT_SMB_HML,temp2,by.x = c('gvkey','yrmo'),
                            by.y = c('gvkey','temp_yrmo'))

SMB_HML_ret = data.table(0,0,0)
colnames(SMB_HML_ret) = c('yrmo','SMB_ret','HML_ret')
yrmo = CRSP_COMPUSTAT_SMB_HML[,unique(yrmo)]
for (i in yrmo) {
  temp = CRSP_COMPUSTAT_SMB_HML[yrmo == i]
  temp[,SMB_wt := lagged_mkt_cap/sum(lagged_mkt_cap,na.rm = TRUE),
       by = c('SMB_portfolio','HMB_portfolio')]
  temp_ret_1 = temp[,list(Ret = sum(tot_ret*SMB_wt,na.rm = TRUE)),
                    by = c('SMB_portfolio','HMB_portfolio')]
  setorder(temp_ret_1,SMB_portfolio,HMB_portfolio)
  temp_ret_2 = temp_ret_1[,list(Ret = mean(Ret)), by = SMB_portfolio]
  temp_ret_2[,yrmo := i]
  temp_ret_3 = temp_ret_1[HMB_portfolio %in% c(1,3)]
  temp_ret_4 = temp_ret_3[,list(Ret = mean(Ret)), by = HMB_portfolio]
  temp_ret_4[,yrmo := i]
  temp_ret = data.table(0,0,0)
  colnames(temp_ret) = c('yrmo','SMB_ret','HML_ret')
  temp_ret$yrmo[1] = i
  temp_ret$SMB_ret[1] = temp_ret_2$Ret[1] - temp_ret_2$Ret[2]
  temp_ret$HML_ret[1] = temp_ret_4$Ret[2] - temp_ret_4$Ret[1]
  SMB_HML_ret = rbind(SMB_HML_ret,temp_ret)
}
SMB_HML_ret = SMB_HML_ret[-1]
SMB_HML_ret = data.table(SMB_HML_ret)
SMB_HML_ret[,Year := floor(yrmo/12)]
SMB_HML_ret[,Month := yrmo%%12]
SMB_HML_ret[Month == 0, Year := Year - 1]
SMB_HML_ret[Month == 0, Month := Month + 12]

final_SMB_HML_ret = subset(SMB_HML_ret, select = c('Year','Month','SMB_ret','HML_ret'))
Q1_size_deciles = final_size_ret
Q1_size_deciles = Q1_size_deciles[-(1:6),]
Q1_size_deciles[,long_short_ret := `10` - `1`]
Q1_BM_deciles = final_BM_ret
Q1_BM_deciles = Q1_BM_deciles[-(1:6),]
Q1_BM_deciles[,long_short_ret := `10` - `1`]

Q1_SMB_HML = final_SMB_HML_ret
############################ QUESTION 2

fama_french_SMB_HML = fread('/Users/gourprabh/Desktop/Documents/Spring/QAM/Week 7/PS4_data/F-F_Research_3_Factors.CSV')
fama_french_SMB_HML = fama_french_SMB_HML[1:1126,]

fama_french_size_portfolios = fread('/Users/gourprabh/Desktop/Documents/Spring/QAM/Week 7/PS4_data/Portfolios_Formed_on_ME.CSV')
fama_french_size_portfolios = subset(fama_french_size_portfolios,
                                     select = c('V1','Lo 10','Dec 2','Dec 3',
                                                'Dec 4','Dec 5','Dec 6',
                                                'Dec 7','Dec 8','Dec 9',
                                                'Hi 10'))
fama_french_size_portfolios[,Date := parse_date_time(as.character(V1),'%Y%m')]

fama_french_size_portfolios = cbind(fama_french_size_portfolios,
                                    Rf = fama_french_SMB_HML$RF)
fama_french_size_portfolios[,yrmo := 12 * year(Date) + month(Date)]
data_range= seq((1973*12 + 1),(2019*12 + 12),1)
fama_french_size_portfolios = fama_french_size_portfolios[yrmo %in% data_range]
fama_french_size_portfolios[,long_short := `Hi 10` - `Lo 10`]

########3 Extracting risk-free rate from the fama-french data
Rf = subset(fama_french_size_portfolios, select = c('yrmo','Rf'))

########## Creating the asked table
colnames(final_size_ret)[3:12] = c('Decile 1','Decile 2','Decile 3','Decile 4',
                                   'Decile 5','Decile 6','Decile 7','Decile 8',
                                   'Decile 9','Decile 10')
final_size_ret[,long_short_ret := `Decile 10` - `Decile 1`]
final_size_ret = final_size_ret[-(1:6),]
final_size_ret[,yrmo := 12*Year + Month]
final_size_ret = merge(final_size_ret,Rf,by.x = c('yrmo'), by.y = c('yrmo'),
                       all.x = TRUE)
final_size_ret[,Rf := as.numeric(Rf)]
final_size_ret[,Rf := Rf/100]
final_size_ret[, `Decile 1` := `Decile 1` - Rf]
final_size_ret[, `Decile 2` := `Decile 2` - Rf]
final_size_ret[, `Decile 3` := `Decile 3` - Rf]
final_size_ret[, `Decile 4` := `Decile 4` - Rf]

final_size_ret[, `Decile 5` := `Decile 5` - Rf]
final_size_ret[, `Decile 6` := `Decile 6` - Rf]
final_size_ret[, `Decile 7` := `Decile 7` - Rf]
final_size_ret[, `Decile 8` := `Decile 8` - Rf]
final_size_ret[, `Decile 9` := `Decile 9` - Rf]
final_size_ret[, `Decile 10` := `Decile 10` - Rf]
final_size_ret[,long_short_ret := long_short_ret - Rf]

Q2 = data.frame(matrix(0,11,5))
colnames(Q2) = c('Annualized Average Excess Return','Annualized Volatility',
                 'Sharpe Ratio','Skewness','Correlation')
rownames(Q2) = c('Decile 1','Decile 2','Decile 3','Decile 4',
                 'Decile 5','Decile 6','Decile 7','Decile 8',
                 'Decile 9','Decile 10','Long-short')

for (i in 1:11) {
  Q2[i,1] = mean(as.numeric(unlist(final_size_ret[,(i+3),with = FALSE])))*12
  Q2[i,2] = sd(as.numeric(unlist(final_size_ret[,(i+3),with = FALSE])))*sqrt(12)
  Q2[i,4] = skewness(as.numeric(unlist(final_size_ret[,(i+3),with = FALSE])))
  Q2[i,3] = Q2[i,1]/Q2[i,2]
}

for (i in 1:10) {
  Q2[i,5] = cor(as.numeric(unlist(Q1_size_deciles[,(i+2),with = FALSE])),
                as.numeric(unlist(fama_french_size_portfolios[,(i+1),with = FALSE])))
}

Q2[11,5] = cor(as.numeric(unlist(Q1_size_deciles[,(13),with = FALSE])),
              as.numeric(unlist(fama_french_size_portfolios[,(15),with = FALSE])))

############################ QUESTION 3

fama_french_BM_portfolios = fread('/Users/gourprabh/Desktop/Documents/Spring/QAM/Week 7/PS4_data/Portfolios_Formed_on_BE-ME.CSV')
fama_french_BM_portfolios = subset(fama_french_BM_portfolios,
                                     select = c('V1','Lo 10','Dec 2','Dec 3',
                                                'Dec 4','Dec 5','Dec 6',
                                                'Dec 7','Dec 8','Dec 9',
                                                'Hi 10'))
fama_french_BM_portfolios[,Date := parse_date_time(as.character(V1),'%Y%m')]

fama_french_BM_portfolios = cbind(fama_french_BM_portfolios,
                                    Rf = fama_french_SMB_HML$RF)
fama_french_BM_portfolios[,yrmo := 12 * year(Date) + month(Date)]
data_range= seq((1973*12 + 1),(2019*12 + 12),1)
fama_french_BM_portfolios = fama_french_BM_portfolios[yrmo %in% data_range]
fama_french_BM_portfolios[,long_short := `Hi 10` - `Lo 10`]

########## Creating the asked table
colnames(final_BM_ret)[3:12] = c('Decile 1','Decile 2','Decile 3','Decile 4',
                                   'Decile 5','Decile 6','Decile 7','Decile 8',
                                   'Decile 9','Decile 10')
final_BM_ret[,long_short_ret := `Decile 10` - `Decile 1`]
final_BM_ret = final_BM_ret[-(1:6),]
final_BM_ret[,yrmo := 12*Year + Month]
final_BM_ret = merge(final_BM_ret,Rf,by.x = c('yrmo'), by.y = c('yrmo'),
                       all.x = TRUE)
final_BM_ret[,Rf := as.numeric(Rf)]
final_BM_ret[,Rf := Rf/100]
final_BM_ret[, `Decile 1` := `Decile 1` - Rf]
final_BM_ret[, `Decile 2` := `Decile 2` - Rf]
final_BM_ret[, `Decile 3` := `Decile 3` - Rf]
final_BM_ret[, `Decile 4` := `Decile 4` - Rf]

final_BM_ret[, `Decile 5` := `Decile 5` - Rf]
final_BM_ret[, `Decile 6` := `Decile 6` - Rf]
final_BM_ret[, `Decile 7` := `Decile 7` - Rf]
final_BM_ret[, `Decile 8` := `Decile 8` - Rf]
final_BM_ret[, `Decile 9` := `Decile 9` - Rf]
final_BM_ret[, `Decile 10` := `Decile 10` - Rf]
final_BM_ret[,long_short_ret := long_short_ret - Rf]

Q3 = data.frame(matrix(0,11,5))
colnames(Q3) = c('Annualized Average Excess Return','Annualized Volatility',
                 'Sharpe Ratio','Skewness','Correlation')
rownames(Q3) = c('Decile 1','Decile 2','Decile 3','Decile 4',
                 'Decile 5','Decile 6','Decile 7','Decile 8',
                 'Decile 9','Decile 10','Long-short')

for (i in 1:11) {
  Q3[i,1] = mean(as.numeric(unlist(final_BM_ret[,(i+3),with = FALSE])))*12
  Q3[i,2] = sd(as.numeric(unlist(final_BM_ret[,(i+3),with = FALSE])))*sqrt(12)
  Q3[i,4] = skewness(as.numeric(unlist(final_BM_ret[,(i+3),with = FALSE])))
  Q3[i,3] = Q3[i,1]/Q3[i,2]
}

for (i in 1:10) {
  Q3[i,5] = cor(as.numeric(unlist(Q1_BM_deciles[,(i+2),with = FALSE])),
                as.numeric(unlist(fama_french_BM_portfolios[,(i+1),with = FALSE])))
}

Q3[11,5] = cor(as.numeric(unlist(Q1_BM_deciles[,(13),with = FALSE])),
               as.numeric(unlist(fama_french_BM_portfolios[,(15),with = FALSE])))

################################ QUESTION 5

fama_french_SMB_HML[,Date := parse_date_time(as.character(V1),'%Y%m')]

fama_french_SMB_HML[,yrmo := 12 * year(Date) + month(Date)]
data_range= seq((1975*12 + 7),(2014*12 + 12),1)
fama_french_SMB_HML = fama_french_SMB_HML[yrmo %in% data_range]

#final_SMB_HML_ret = final_SMB_HML_ret[-(1:6),]
final_SMB_HML_ret[,yrmo := 12*Year + Month]
final_SMB_HML_ret = merge(final_SMB_HML_ret,Rf,by.x = c('yrmo'), by.y = c('yrmo'),
                     all.x = TRUE)
final_SMB_HML_ret[,Rf := as.numeric(Rf)]
final_SMB_HML_ret[,Rf := Rf/100]
final_SMB_HML_ret[,SMB_ex_ret := SMB_ret - Rf]
final_SMB_HML_ret[,HML_ex_ret := HML_ret - Rf]

Q5 = data.frame(matrix(0,2,5))
colnames(Q5) = c('Annualized Average Excess Return','Annualized Volatility',
                 'Sharpe Ratio','Skewness','Correlation')
rownames(Q5) = c('SMB','HML')


Q5[1,1] = mean(as.numeric(unlist(final_SMB_HML_ret[,SMB_ex_ret])))*12
Q5[1,2] = sd(as.numeric(unlist(final_SMB_HML_ret[,SMB_ex_ret])))*sqrt(12)
Q5[1,4] = skewness(as.numeric(unlist(final_SMB_HML_ret[,SMB_ex_ret])))
Q5[1,3] = Q5[1,1]/Q5[1,2]
Q5[1,5] = cor(as.numeric(unlist(final_SMB_HML_ret[,SMB_ret])),
               as.numeric(unlist(fama_french_SMB_HML[,SMB])))

Q5[2,1] = mean(as.numeric(unlist(final_SMB_HML_ret[,HML_ex_ret])))*12
Q5[2,2] = sd(as.numeric(unlist(final_SMB_HML_ret[,HML_ex_ret])))*sqrt(12)
Q5[2,4] = skewness(as.numeric(unlist(final_SMB_HML_ret[,HML_ex_ret])))
Q5[2,3] = Q5[2,1]/Q5[2,2]
Q5[2,5] = cor(as.numeric(unlist(final_SMB_HML_ret[,HML_ret])),
              as.numeric(unlist(fama_french_SMB_HML[,HML])))


##################### Checking whether the value and size anomaly has worked in the past few years

forplot = size_ret[,cumret := log(cumprod(1 + size_ret)), by = c('decile')]
forplot[,Year := floor(yrmo/12)]
forplot[,Month := yrmo%%12]
forplot[Month == 0, Year := Year - 1]
forplot[Month == 0, Month := Month + 12]
forplot[, Dates := as.Date(paste0(Year,'-',Month,'-','01'),'%Y-%m-%d')]
(ggplot(forplot,aes(x = Dates, y = cumret)) + 
    geom_line(aes(col = factor(decile))) + theme_bw())

forplot = BM_ret[,cumret := log(cumprod(1 + BM_ret)), by = c('decile')]
forplot[,Year := floor(yrmo/12)]
forplot[,Month := yrmo%%12]
forplot[Month == 0, Year := Year - 1]
forplot[Month == 0, Month := Month + 12]
forplot[, Dates := as.Date(paste0(Year,'-',Month,'-','01'),'%Y-%m-%d')]
(ggplot(forplot,aes(x = Dates, y = cumret)) + 
    geom_line(aes(col = factor(decile))) + theme_bw())

##################### Checking whether the Fama French factorse have worked in the past few years
forplot = Q1_SMB_HML[,cum_SMB_ret := log(cumprod(1 + SMB_ret))]
forplot = Q1_SMB_HML[,cum_HML_ret := log(cumprod(1 + HML_ret))]
forplot[, Dates := as.Date(paste0(Year,'-',Month,'-','01'),'%Y-%m-%d')]
(ggplot(forplot,aes(x = Dates, y = cum_SMB_ret)) + 
    geom_line(aes(col = I('red'))) + geom_line(aes(x = Dates, y = cum_HML_ret,col = I('blue'))) 
  +  scale_color_discrete(name = "FF factors", labels = c("SMB", "HML")) + 
   ylab('Cumulative Return')  + theme_bw())

######################### Repeating the above procedure for actual Fama French returns from their website

temp_ff = subset(fama_french_size_portfolios,
                                        select = c('Date','Lo 10','Dec 2','Dec 3',
                                                          'Dec 4','Dec 5','Dec 6',
                                                          'Dec 7','Dec 8','Dec 9',
                                                          'Hi 10'))
forplot = melt(temp_ff,id.vars = c('Date'),measure.vars = c('Lo 10','Dec 2','Dec 3',
                                                            'Dec 4','Dec 5','Dec 6',
                                                            'Dec 7','Dec 8','Dec 9',
                                                            'Hi 10'))
forplot[,value := value/100]
forplot = forplot[,cumret := log(cumprod(1 + value)), by = c('variable')]
(ggplot(forplot,aes(x = Date, y = cumret)) + 
    geom_line(aes(col = factor(variable))) + theme_bw())

temp_ff = subset(fama_french_BM_portfolios,
                 select = c('Date','Lo 10','Dec 2','Dec 3',
                            'Dec 4','Dec 5','Dec 6',
                            'Dec 7','Dec 8','Dec 9',
                            'Hi 10'))
forplot = melt(temp_ff,id.vars = c('Date'),measure.vars = c('Lo 10','Dec 2','Dec 3',
                                                            'Dec 4','Dec 5','Dec 6',
                                                            'Dec 7','Dec 8','Dec 9',
                                                            'Hi 10'))
forplot[,value := value/100]
forplot = forplot[,cumret := log(cumprod(1 + value)), by = c('variable')]
(ggplot(forplot,aes(x = Date, y = cumret)) + 
    geom_line(aes(col = factor(variable))) + theme_bw())

temp_ff = subset(fama_french_SMB_HML,
                 select = c('Date','SMB','HML'))
temp_ff[,SMB := as.numeric(SMB)]
temp_ff[,HML := as.numeric(HML)]
temp_ff[,SMB := SMB/100]
temp_ff[,HML := HML/100]
forplot = temp_ff[,cum_SMB_ret := log(cumprod(1 + SMB))]
forplot = forplot[,cum_HML_ret := log(cumprod(1 + HML))]
(ggplot(forplot,aes(x = Date, y = cum_SMB_ret)) + 
    geom_line(aes(col = I('red'))) + geom_line(aes(x = Date, y = cum_HML_ret,col = I('blue'))) 
  +  scale_color_discrete(name = "FF factors", labels = c("SMB", "HML")) + theme_bw())

temp_HML = subset(final_SMB_HML_ret, select = c('yrmo','HML_ret'))
temp_ff = subset(fama_french_SMB_HML, select = c('Date','HML','yrmo'))
forplot1 = merge(temp_ff,temp_HML, by.x = c('yrmo'),
                 by.y = c('yrmo'),all.x = TRUE)
forplot1[,HML := as.numeric(HML)]
forplot1[,HML := HML/100]
forplot1 = forplot1[,cum_HML_ff := (cumprod(1 + HML))]
forplot1[,cum_HML_p := (cumprod(1 + HML_ret))]
(ggplot(forplot1,aes(x = Date, y = cum_HML_p)) + 
    geom_line(aes(col = I('red'))) + geom_line(aes(x = Date, y = cum_HML_ff,col = I('blue'))) 
  +  scale_color_discrete(name = "HML", labels = c("FF with patent", "Fama-French")) + 
    ylab('cumulative returns') + theme_bw())
