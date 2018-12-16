# Databricks notebook source
gc(reset=T)
caminho = '/dbfs/FileStore/tables/'
files = list.files(path=caminho, pattern="*.csv")
df_desafio = do.call(rbind, lapply(files, function(x) read.csv(paste(caminho,x,sep = ''), stringsAsFactors = FALSE)))
#write.csv2(df_desafio,file = paste(caminho,'df_full.csv',sep = ''),row.names = FALSE)

# COMMAND ----------

head(df_desafio)

# COMMAND ----------

sum(is.na(df_desafio))

# COMMAND ----------

fastDummies::dummy_cols(fastDummies_example)
fastDummies::dummy_cols(fastDummies_example, select_columns = "numbers")

# COMMAND ----------



# COMMAND ----------



# COMMAND ----------



# COMMAND ----------



# COMMAND ----------


