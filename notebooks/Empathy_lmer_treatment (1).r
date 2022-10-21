# Databricks notebook source
# library(alpaca)
# library(lfe)
# library(fixest)
# library(lme4)
library(lmerTest)

# COMMAND ----------

library(data.table)
library(dplyr)

# COMMAND ----------

# MAGIC %md # raw data

# COMMAND ----------

path='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/'

# final_df=pd.read_csv('%sempathy_imputed_raw_df.csv'%path)
# log_final_df=pd.read_csv('%sempathy_log_imputed_raw_df.csv'%path)
std_final_df=fread('/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_std_imputed_raw_df.csv')
# log_std_final_df=fread('/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_std_log_imputed_raw_df.csv')
std_final_df[1:3,1:4]

# COMMAND ----------

# log_std_final_df=std_final_df
unique(std_final_df$Time)

# COMMAND ----------

Time=std_final_df$Time
# Time[which(Time=='10min')]='0.167hr'
# Time[which(Time=='30min')]='0.5hr'
Time[which(Time=='End HI')]='0hr'
Time[which(Time=='BL')]='-4hr'
# Time[which(Time=='4hrLPS')]='-0.5hr'
Time=Time

# COMMAND ----------

library(stringr)
Hour=str_replace_all(Time,'hr','')
Hour=as.numeric(Hour)
print(unique(Hour))
std_final_df$Hour=Hour
# std_final_df$Hour=Hour

# COMMAND ----------

unique(std_final_df$Hour)

# COMMAND ----------

getLMER=function(m,df){
  one_data=df %>% dplyr::select(all_of(m),'Hour','Group','LWP')
  colnames(one_data)=c('molecule','Hour','Group','LWP')
  
  slope_df <- data.frame(matrix(nrow = 17, ncol = 1))
  rownames(slope_df)=c('GroupA','GroupB','GroupC','GroupD',
                       'Hour','HGroupB','HGroupC','HGroupD',
                       'GroupA_Pvalue','GroupB_Pvalue','GroupC_Pvalue','GroupD_Pvalue',
                       'Hour_Pvalue','HGroupB_Pvalue','HGroupC_Pvalue','HGroupD_Pvalue',
                       'AIC')
  colnames(slope_df)=m
#   print(colnames(one_data))
  model <- lmer(molecule ~ Group*Hour  + (1|LWP) , data=one_data)
#   model <- lmer(molecule ~ Group*Hour -1 + (1|LWP) , data=one_data)
  slope_df['GroupA',m]=summary(model)$coef[1,1]
  slope_df['GroupB',m]=summary(model)$coef[2,1]
  slope_df['GroupC',m]=summary(model)$coef[3,1]
  slope_df['GroupD',m]=summary(model)$coef[4,1]
  slope_df['Hour',m]=summary(model)$coef[5,1]
  slope_df['HGroupB',m]=summary(model)$coef[6,1]
  slope_df['HGroupC',m]=summary(model)$coef[7,1]
  slope_df['HGroupD',m]=summary(model)$coef[8,1]
  slope_df['GroupA_Pvalue',m]=summary(model)$coef[1,5]
  slope_df['GroupB_Pvalue',m]=summary(model)$coef[2,5]
  slope_df['GroupC_Pvalue',m]=summary(model)$coef[3,5]
  slope_df['GroupD_Pvalue',m]=summary(model)$coef[4,5]
  slope_df['Hour_Pvalue',m]=summary(model)$coef[5,5]
  slope_df['HGroupB_Pvalue',m]=summary(model)$coef[6,5]
  slope_df['HGroupC_Pvalue',m]=summary(model)$coef[7,5]
  slope_df['HGroupD_Pvalue',m]=summary(model)$coef[8,5]
  slope_df['AIC',m]=AIC(model)
  slope_df
}

bar <- function(m,one_data) tryCatch(getLMER(m,one_data), error = function(e) {})

# COMMAND ----------

time_df=std_final_df[std_final_df$Hour>0]
print(dim(time_df))
molecules=colnames(time_df)[startsWith(colnames(time_df), 'rLC')]
print(length(molecules))

# COMMAND ----------

unique(time_df$Hour)

# COMMAND ----------

# MAGIC %md ## check with one example

# COMMAND ----------

ma_data=time_df %>% dplyr::select(molecules[1],'Hour','Group','LWP')
colnames(ma_data)=c('molecule','Hour','Group','LWP')
ma_data

# COMMAND ----------

# model <- lmer(molecule ~ Group*Hour  + (1|LWP), data=ma_data)
model <-lmer(molecule ~ Group*Hour + (1|LWP) , data=ma_data)

# COMMAND ----------

summary(model)
coef(summary(model))

# COMMAND ----------

summary(model)
coef(summary(model))

# COMMAND ----------

lmer_df = lapply(molecules[1:3], function(mtb)
    bar(mtb,time_df))
                 
lmer_df

# COMMAND ----------

lmer_df = lapply(molecules[1:3], function(mtb)
    bar(mtb,time_df))
                 
lmer_df

# COMMAND ----------

# MAGIC %md ## run all data

# COMMAND ----------

length(molecules)

# COMMAND ----------


lmer_df_1 = lapply(molecules[1:10000], function(mtb)
    bar(mtb,time_df))
                 

# COMMAND ----------

lmer_df_1  <-  as.data.frame(matrix(unlist(lmer_df_1), nrow=length(unlist(lmer_df_1[1]))))
colnames(lmer_df_1)=molecules[1:10000]
# lmer_df
rownames(lmer_df_1)=c('GroupA','GroupB','GroupC','GroupD',
                       'Hour','HGroupB','HGroupC','HGroupD',
                       'GroupA_Pvalue','GroupB_Pvalue','GroupC_Pvalue','GroupD_Pvalue',
                       'Hour_Pvalue','HGroupB_Pvalue','HGroupC_Pvalue','HGroupD_Pvalue',
                       'AIC')


# COMMAND ----------

# fwrite(lmer_df_1,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_log_df_1.csv')
fwrite(lmer_df_1,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_std_df_1.csv')

# COMMAND ----------


lmer_df_2 = lapply(molecules[10001:20000], function(mtb)
    bar(mtb,time_df))

# COMMAND ----------

lmer_df_2  <-  as.data.frame(matrix(unlist(lmer_df_2), nrow=length(unlist(lmer_df_2[1]))))
colnames(lmer_df_2)=molecules[10001:20000]
# lmer_df
rownames(lmer_df_2)=c('GroupA','GroupB','GroupC','GroupD',
                       'Hour','HGroupB','HGroupC','HGroupD',
                       'GroupA_Pvalue','GroupB_Pvalue','GroupC_Pvalue','GroupD_Pvalue',
                       'Hour_Pvalue','HGroupB_Pvalue','HGroupC_Pvalue','HGroupD_Pvalue',
                       'AIC')
# fwrite(lmer_df_2,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_log_df_2.csv')
fwrite(lmer_df_2,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_std_df_2.csv')

# COMMAND ----------


lmer_df_3 = lapply(molecules[20001:30000], function(mtb)
    bar(mtb,time_df))

# COMMAND ----------

lmer_df_3  <-  as.data.frame(matrix(unlist(lmer_df_3), nrow=length(unlist(lmer_df_3[1]))))
colnames(lmer_df_3)=molecules[20001:30000]
# lmer_df
rownames(lmer_df_3)=c('GroupA','GroupB','GroupC','GroupD',
                       'Hour','HGroupB','HGroupC','HGroupD',
                       'GroupA_Pvalue','GroupB_Pvalue','GroupC_Pvalue','GroupD_Pvalue',
                       'Hour_Pvalue','HGroupB_Pvalue','HGroupC_Pvalue','HGroupD_Pvalue',
                       'AIC')
# fwrite(lmer_df_3,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_log_df_3.csv')
fwrite(lmer_df_3,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_std_df_3.csv')

# COMMAND ----------

lmer_df_4 = lapply(molecules[30001:40000], function(mtb)
    bar(mtb,time_df))

# COMMAND ----------

lmer_df_4  <-  as.data.frame(matrix(unlist(lmer_df_4), nrow=length(unlist(lmer_df_4[1]))))
colnames(lmer_df_4)=molecules[30001:40000]
# lmer_df
rownames(lmer_df_4)=c('GroupA','GroupB','GroupC','GroupD',
                       'Hour','HGroupB','HGroupC','HGroupD',
                       'GroupA_Pvalue','GroupB_Pvalue','GroupC_Pvalue','GroupD_Pvalue',
                       'Hour_Pvalue','HGroupB_Pvalue','HGroupC_Pvalue','HGroupD_Pvalue',
                       'AIC')
# fwrite(lmer_df_4,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_log_df_4.csv')
fwrite(lmer_df_4,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_std_df_4.csv')

# COMMAND ----------

lmer_df_5 = lapply(molecules[40001:50092], function(mtb)
    bar(mtb,time_df))

# COMMAND ----------

lmer_df_5  <-  as.data.frame(matrix(unlist(lmer_df_5), nrow=length(unlist(lmer_df_5[1]))))
colnames(lmer_df_5)=molecules[40001:50092]
# lmer_df
rownames(lmer_df_5)=c('GroupA','GroupB','GroupC','GroupD',
                       'Hour','HGroupB','HGroupC','HGroupD',
                       'GroupA_Pvalue','GroupB_Pvalue','GroupC_Pvalue','GroupD_Pvalue',
                       'Hour_Pvalue','HGroupB_Pvalue','HGroupC_Pvalue','HGroupD_Pvalue',
                       'AIC')
# fwrite(lmer_df_5,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_log_df_5.csv')
fwrite(lmer_df_5,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_std_df_5.csv')

# COMMAND ----------

lmer_df=cbind(lmer_df_1,lmer_df_2,lmer_df_3,lmer_df_4,lmer_df_5)
rownames(lmer_df)=c('GroupA','GroupB','GroupC','GroupD',
                       'Hour','HGroupB','HGroupC','HGroupD',
                       'GroupA_Pvalue','GroupB_Pvalue','GroupC_Pvalue','GroupD_Pvalue',
                       'Hour_Pvalue','HGroupB_Pvalue','HGroupC_Pvalue','HGroupD_Pvalue',
                       'AIC')
dim(lmer_df)

# COMMAND ----------

# fwrite(lmer_df,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_log_df.csv')
fwrite(lmer_df,file='/dbfs/mnt/client-002sap21p024-paint/04_data_analysis/raw_data/empathy_lmer_std_df.csv')

# COMMAND ----------


