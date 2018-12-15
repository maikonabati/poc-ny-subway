# LIBRARYS ----------------------------------------

#Tempo
start.time <- Sys.time()

#Limpar memória
rm(list=ls())
gc(reset=T)

# Salvar workspace
# load(file = "poc-ny-subway.RData")
# save.image(file = "poc-ny-subway.RData")

#Diretório do projeto
setwd(paste("C:/Users/",Sys.info()[["user"]],"/Documents/GitHub/poc-ny-subway/code/r/", sep = ""))

#Carregar pacotes do R
library("data.table")
library("ggplot2")
library("stringr")
library("reshape2")
library("factoextra")
library(stringi)
library('bigmemory')

# Funções para obtero triângulo de correlação da matriz
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Merge CSV's -------------------------------------------------------------
# files = list.files(path='../../dist/', pattern="*.csv")
# merge_df = do.call(rbind, lapply(files, function(x) read.csv(paste('../../dist/',x,sep = ''), stringsAsFactors = FALSE)))
# write.csv2(merge_df,file = "../../dist/merge_df.csv",row.names = FALSE)

# Load data - bigmemory ---------------------------------------------------
gc(reset=T)
df_bm <- read.big.matrix('../../dist/merge_df.csv', 
                     header = F,
                     # type = "integer",
                     sep = ";",
                     backingfile = "data.bin", 
                     descriptor = "poc.ny.subway",
                     col.names = c("time", "ca", "unit", "scp", "station", "linename", "division", "desc", "entries", "exits"), 
                     shared=TRUE)

# Salvar workspace - bigmemory
# load(file = "poc-ny-subway-bigmemory.RData")
# save.image(file = "poc-ny-subway-bigmemory.RData")



