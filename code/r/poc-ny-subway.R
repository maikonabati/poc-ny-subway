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







# DATA SETS ------------------------------------------------------
ds_plantio <- read_excel("dist/dataset_qualidade_plantio_v1_original.xlsx",1)


hist(ds_plantio_falhas$altura_da_cobricao_cm)

# PLANTIO - CRIAÇÃO DOS QUANTIS -------------------------------------------

#~MELHORIA: FUNÇÃO PARA GERAR QUANTIL/CLUSTER AUTOMÁTICO?

ds_plantio_falhas_v4 <- ds_plantio_falhas_v3
nomes_adicionais<-c()

#Primeira etapa - cria quantil/var
for(item in lista_plantio){
  for(num in 2:5){
    quant<-quantile(na.omit(eval(parse(text = paste("ds_plantio_falhas_v4$",item,sep = "")))))[[num]]
    quant_ant<-quantile(na.omit(eval(parse(text = paste("ds_plantio_falhas_v4$",item,sep = "")))))[[num-1]]
      
    ds_plantio_falhas_v4[paste(item,"_q",num-1,sep = "")]<-
      ifelse(
        eval(parse(text = paste("ds_plantio_falhas_v4$",item,sep = "")))>=quant_ant & eval(parse(text = paste("ds_plantio_falhas_v4$",item,sep = "")))<quant,
        1,
        0
        )
    
    nomes_adicionais<-c(nomes_adicionais,paste(item,"_q",num-1,sep = ""))
  }
}

ds_agregacao <- ds_plantio_falhas_v4[,c("safra","bloco",nomes_adicionais)]
ds_agregacao <- ds_agregacao[,c(names(ds_agregacao))] %>% group_by(safra,bloco) %>% summarise_all(funs(sum))

#Novas colunas para modelo RF
ds_agregacao <- dplyr::left_join(ds_agregacao,ds_plantio_falhas_v4[,c(lista_plantio,'safra','bloco')], by = c("safra"="safra","bloco"="bloco"))
ds_agregacao <- ds_agregacao[,c(names(ds_agregacao))] %>% group_by(safra,bloco) %>% summarise_all(funs(mean))

#Segunda etapa - Consolida quantil/var
for(item in lista_plantio){
  ds_agregacao[paste(item,"_total",sep = "")]<-
    eval(parse(text = paste("ds_agregacao$",item,"_q1",sep = "")))+
    eval(parse(text = paste("ds_agregacao$",item,"_q2",sep = "")))+
    eval(parse(text = paste("ds_agregacao$",item,"_q3",sep = "")))+
    eval(parse(text = paste("ds_agregacao$",item,"_q4",sep = "")))
}

#Terceira etapa - Cria indicador percentual/var
for(item in lista_plantio){
  for(num in 1:4){
    ds_agregacao[paste(item,"_q",num,"_perc",sep = "")]<-
      eval(parse(text = paste("ds_agregacao$",item,"_q",num,sep = "")))/
      eval(parse(text = paste("ds_agregacao$",item,"_total",sep = "")))
  }
}

write.csv2(ds_plantio_falhas_v6,file = "ds_plantio_falhas_v6.csv",row.names = FALSE)

# PLANTIO - RELACIONA TABELAS COMPLEMENTARES ---------------------------------
ds_plantio_falhas_v5 <- dplyr::left_join(ds_agregacao,ds_falhas[,names(ds_falhas)], by = c("safra"="safra","bloco"="bloco"))
ds_plantio_falhas_v5 <- dplyr::left_join(ds_plantio_falhas_v5,ds_ar_bloco[,
                                                                          c(
                                                                            'safra'
                                                                            ,'bloco'
                                                                            ,'unidade'
                                                                            ,'ciclo_corte'
                                                                            ,'variedade'
                                                                            ,'area_tl'
                                                                            ,'cd_ambiente'
                                                                            ,'corte_plus'
                                                                            )], by = c("safra"="safra","bloco"="bloco"))
ds_plantio_falhas_v5 <- dplyr::left_join(ds_plantio_falhas_v5,ds_ambientes[,c(names(ds_ambientes))], by = c("bloco"="bloco"))

#write.csv2(ds_plantio_falhas_v5,file = "ds_plantio_falhas_v5.csv",row.names = FALSE)

#Ajusta ambiente vazio
ds_plantio_falhas_v5$cd_ambiente<-ifelse(
  is.na(ds_plantio_falhas_v5$cd_ambiente),
  ds_plantio_falhas_v5$ambiente,
  ds_plantio_falhas_v5$cd_ambiente
  )
ds_plantio_falhas_v5$ambiente<-NULL

#Validação de ambientes - possíveis bloc de fornecedor
# valida_ambientes <- ds_plantio_falhas_v5[,c('safra','bloco','cd_ambiente','ambiente')]
# nrow(subset(valida_ambientes,is.na(valida_ambientes$cd_ambiente)))

# PLANTIO - CRIAÇÃO DE DUMMY's --------------------------------------------

#Tipo das colunas
# for(item in names(ds_plantio_falhas_v5)){
#   print(paste(item,"-",class(eval(parse(text = paste("ds_plantio_falhas_v5$",item,sep = ""))))))
# }
#safra, unidade, variedade, cd_ambiente, dt_inicio_ciclo/dt_fim_ciclo

ds_plantio_falhas_v5<-CriarDummies(tabela = ds_plantio_falhas_v5,coluna = ds_plantio_falhas_v5$safra,nome = "safra")
ds_plantio_falhas_v5<-CriarDummies(tabela = ds_plantio_falhas_v5,coluna = ds_plantio_falhas_v5$unidade,nome = "unidade")
ds_plantio_falhas_v5<-CriarDummies(tabela = ds_plantio_falhas_v5,coluna = ds_plantio_falhas_v5$cd_ambiente,nome = "cd_ambiente")

ds_plantio_falhas_v5$safra<-NULL
ds_plantio_falhas_v5$unidade<-NULL
ds_plantio_falhas_v5$cd_ambiente<-NULL

ds_plantio_falhas_v5$variedade<-as.factor(ds_plantio_falhas_v5$variedade)

# PLANTIO - RANDOM FOREST: FALHAS vs VAR ----------------------------------

ds_plantio_falhas_v6<-ds_plantio_falhas_v5

#Validar excesso de 'null'
#write.csv2(ds_plantio_falhas_v6,file = "ds_plantio_falhas_v6.csv",row.names = FALSE)

#SEED RF
set.seed(1234)

for(action in c("na.omit","na.roughfix")){
  
  model_clus<-randomForest(falha~., data=ds_plantio_falhas_v6, importance=TRUE, proximity=TRUE, na.action=action, ntree=300)
  
  #summary(model_clus)
  #model_clus$importance
  #model_clus
  
  for(item in lista_plantio){
    
    ###Partial Dependence Plot
    dependb = partial(model_clus,
                      pred.data = ds_plantio_falhas_v6,
                      pred.var = c('falha',item),
                      grid.resolution = 50)# onde y é a variável do insumo de interesse
    
    ggsave(
      ggplot(data=dependb, aes(x=eval(parse(text = item)), y=yhat))+geom_line()+
        scale_y_continuous(limit = c(.07, .15), breaks = seq(.01,1,.025),labels = scales::percent)+
        ggtitle(item)+
        labs(x=item,y="Falha")
      ,file=parse(text=paste("dist/resultados/plantio/pdp/",action,"/PDP_falha_vs_",item,".png",sep="")),width = 4.5, height = 3, units = "in")
    
  }
  
}


# PLANTIO - LOOP DE GRÁFICOS ~ HIST e LR ----------------------------------

ds_plantio_falhas_v2 <- dplyr::left_join(ds_plantio_falhas_v2,ds_falhas[,names(ds_falhas)], by = c("safra"="safra","bloco"="bloco"))

ds_plantio_falhas_v2<-subset(ds_plantio_falhas_v2,
                             ds_plantio_falhas_v2$altura_da_cobricao_cm<=20 
                             & ds_plantio_falhas_v2$consumo_de_mudas_tonha<=30 #18 - Valida com Jahiz
                             & ((ds_plantio_falhas_v2$em_50_toletes_n_toletes_com_nota_0+
                                   ds_plantio_falhas_v2$em_50_toletes_n_toletes_com_nota_2+
                                   ds_plantio_falhas_v2$em_50_toletes_n_toletes_com_nota_4+
                                   ds_plantio_falhas_v2$em_50_toletes_n_toletes_com_nota_6+
                                   ds_plantio_falhas_v2$em_50_toletes_n_toletes_com_nota_8)<=50)
                             & ds_plantio_falhas_v2$espaçamento_m<=2
                             & ds_plantio_falhas_v2$falha_de_cana_no_sulco_m5m<=5
                             & ds_plantio_falhas_v2$largura_do_sulco_cm<=75
                             & ds_plantio_falhas_v2$n_gemas_danificadas_em_50_toletes<=200
                             & ds_plantio_falhas_v2$n_gemas_danificadas_por_metro<=50 #10 - Valida com Jahiz
                             & ds_plantio_falhas_v2$n_gemas_sadias_em_50_toletes<=200
                             & ds_plantio_falhas_v2$n_gemas_sadias_por_metro<=45
                             & ds_plantio_falhas_v2$profundidade_da_sulcacao_cm<=45
                             & ds_plantio_falhas_v2$falha_de_cobricao_em_5_metros<=5
                             & ds_plantio_falhas_v2$tamanho_médio_em_10_toletes<=100
                             #& ds_plantio_falhas_v2$falha<=1
)


for(item in lista_plantio){
  min<-min(na.omit(eval(parse(text = paste("ds_plantio_falhas_v2$",item,sep = "")))))
  max<-max(na.omit(eval(parse(text = paste("ds_plantio_falhas_v2$",item,sep = "")))))
  media<-mean(na.omit(eval(parse(text = paste("ds_plantio_falhas_v2$",item,sep = "")))))
  mediana<-median(na.omit(eval(parse(text = paste("ds_plantio_falhas_v2$",item,sep = "")))))
  desvio<-sd(na.omit(eval(parse(text = paste("ds_plantio_falhas_v2$",item,sep = "")))))

  print(paste(item,mediana))

  #Histograma
  png(file=parse(text=paste("dist/resultados/plantio/",item,"_Histograma.png",sep="")), width=1024, height=768)
  hist(na.omit(eval(parse(text = paste("ds_plantio_falhas_v2$",item,sep = "")))), main = item,xlab=item,ylab="Qtd. eventos",col="grey",col.main="black",cex.lab=1.5, cex.axis=1.5, cex.main=3, cex.sub=2)
  abline(v = mediana,col="black",lwd=2)
  abline(v = media, lty=2, col="black",lwd=2)
  abline(v = (mediana-desvio),col="red",lwd=2)
  abline(v = (mediana+desvio),col="blue",lwd=2)
  dev.off()

  for(item2 in lista_plantio){
    #RL Simples
    ggsave(
      ggplot(ds_plantio_falhas_v2, aes(eval(parse(text = paste(item,sep = ""))),eval(parse(text = paste(item2,sep = ""))))) +
        geom_point(alpha = 0.25) +
        geom_smooth(method="lm") +
        ggtitle(paste(item2,"vs",item)) +
        labs(x=item,y=item2)
      ,file=parse(text=paste("dist/resultados/plantio/",item2,"_vs_",item,".png",sep="")),width = 4.5, height = 3, units = "in")
  }
}


# PLANTIO - MATRIZ DE CORRELAÇÃO ------------------------------------------

lista_plantio<-c(lista_plantio,"falha")

for(num_matriz in c(17,18)){

  amostras <- nrow(na.omit(ds_plantio_falhas_v2_cor[,lista_plantio[1:num_matriz]]))

  cormat <- round(cor(na.omit(ds_plantio_falhas_v2_cor[,lista_plantio[1:num_matriz]])),2)
  #head(cormat)

  #Heatmap da correlação
  melted_cormat <- melt(cormat)

  #Triânglo abaixo - Desordenado
  upper_tri <- get_upper_tri(cormat)
  melted_cormat_v2 <- melt(upper_tri, na.rm = TRUE)

  # Gráfico - Correlação
  ggsave(
    ggplot(melted_cormat_v2, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "red", high = "#009E73", mid = "white",
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name="Pearson\nCorrelation") +
      theme_minimal()+ # minimal theme
      labs(x="",y="")+
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12, hjust = 1),
        plot.title = element_text(size=20)
      )+
      ggtitle(paste("Plantio - Matriz de Correlação",ifelse(num_matriz==17,"(sem falha)","(com falha)"),"-",amostras,"amostras")) +
      coord_fixed()+
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank())
    ,file=parse(text=paste("dist/resultados/plantio/A_Plantio_Matriz_correlação",num_matriz,".png",sep = "")),width = 30, height = 25, units = "cm")
}


#Time difference of 8.426548 mins
#Tempo de execução
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# COLHEITA - DEPOIS... ----------------------------------------------------------------
  # ds_colheita_falhas = dplyr::left_join(ds_colheita,ds_falhas[,c("safra","bloco","falha")], by = c("safra"="safra","bloco"="bloco"))
  # ds_colheita_falhas_v1 = dplyr::left_join(ds_colheita_falhas,ds_ar_bloco[,names(ds_ar_bloco)], by = c("safra"="safra","bloco"="bloco"))
  # 
  # #### Cenário - Plantio vs Falha
  # lista_colheita<-names(ds_colheita_falhas)[10:length(names(ds_colheita_falhas))]
  # ds_colheita_falhas_v2<-ds_colheita_falhas[,lista_colheita]
  # 
  # #Filtros de seguraça por variável
  # # ds_colheita_falhas_v3<-subset(ds_colheita_falhas_v2,
  # #                               0==0
  # # )
  # 
  # ################### LAÇO PARA GRÁFICOS
  # 
  # for(item in lista_colheita[1:7]){
  #   min<-min(na.omit(eval(parse(text = paste("ds_colheita_falhas_v2$",item,sep = "")))))
  #   max<-max(na.omit(eval(parse(text = paste("ds_colheita_falhas_v2$",item,sep = "")))))
  #   media<-mean(na.omit(eval(parse(text = paste("ds_colheita_falhas_v2$",item,sep = "")))))
  #   mediana<-median(na.omit(eval(parse(text = paste("ds_colheita_falhas_v2$",item,sep = "")))))
  #   desvio<-sd(na.omit(eval(parse(text = paste("ds_colheita_falhas_v2$",item,sep = "")))))
  #   
  #   print(paste(item,mediana))
  #   
  #   #Histograma
  #   png(file=parse(text=paste("dist/resultados/colheita/",item,"_Histograma.png",sep="")), width=1024, height=768)
  #   hist(na.omit(eval(parse(text = paste("ds_colheita_falhas_v2$",item,sep = "")))), main = item,xlab=item,ylab="Qtd. eventos",col="grey",col.main="black",cex.lab=1.5, cex.axis=1.5, cex.main=3, cex.sub=2)
  #   abline(v = mediana,col="black",lwd=2)
  #   abline(v = media, lty=2, col="black",lwd=2)
  #   abline(v = (mediana-desvio),col="red",lwd=2)
  #   abline(v = (mediana+desvio),col="blue",lwd=2)
  #   dev.off()
  #   
  #   for(item2 in lista_colheita){
  #     #RL Simples
  #     ggsave(
  #       ggplot(ds_colheita_falhas_v2, aes(eval(parse(text = paste(item,sep = ""))),eval(parse(text = paste(item2,sep = ""))))) +
  #         geom_point(alpha = 0.25) +
  #         geom_smooth(method="lm") +
  #         ggtitle(paste(item2,"vs",item)) +
  #         labs(x=item,y=item2)
  #       ,file=parse(text=paste("dist/resultados/colheita/",item2,"_vs_",item,".png",sep="")),width = 4.5, height = 3, units = "in")
  #   }
  # }
  # 
  # #write.csv2(x = ds_colheita_falhas_v2,file = "ds_colheita_falhas_v2.csv", sep = ";",dec = ".",row.names = FALSE)
  # 
  # 
  # ################### CURVAS PARA COLHEITA - RF: FALHAS vs VAR
  # #SEED RF
  # set.seed(1234)
  # 
  # for(action in c("na.roughfix","na.omit")){
  #   
  #   model_clus<-randomForest(falha~., data=ds_colheita_falhas_v2, importance=TRUE, proximity=TRUE, na.action=action)
  #   #summary(model_clus)
  #   #model_clus$importance
  #   #model_clus
  #   
  #   for(item in lista_colheita){
  #     
  #     ###Partial Dependence Plot
  #     dependb = partial(model_clus,
  #                       pred.data = ds_colheita_falhas_v2,
  #                       pred.var = c(item),
  #                       grid.resolution = 50)# onde y é a variável do insumo de interesse
  #     
  #     ggsave(
  #       ggplot(data=dependb, aes(x=eval(parse(text = item)), y=yhat))+geom_line()+
  #         ggtitle(item)+
  #         labs(x=item,y="% Falha")
  #       ,file=parse(text=paste("dist/resultados/colheita/pdp/",action,"/PDP_falha_vs_",item,".png",sep="")),width = 4.5, height = 3, units = "in")
  #     
  #   }
  #   
  # }
  # 
  # #Matriz de correlação
  # for(num_matriz in c(7,10,11)){
  # 
  #   amostras <- nrow(na.omit(ds_colheita_falhas_v2[,lista_colheita[1:num_matriz]]))
  #   
  #   cormat <- round(cor(na.omit(ds_colheita_falhas_v2[,lista_colheita[1:num_matriz]])),2)
  #   #head(cormat)
  #   
  #   #Heatmap da correlação
  #   melted_cormat <- melt(cormat)
  #   
  #   #Triânglo abaixo - Desordenado
  #   upper_tri <- get_upper_tri(cormat)
  #   melted_cormat_v2 <- melt(upper_tri, na.rm = TRUE)
  #   
  #   # Gráfico - Correlação
  #   ggsave(
  #     ggplot(melted_cormat_v2, aes(Var2, Var1, fill = value))+
  #       geom_tile(color = "white")+
  #       scale_fill_gradient2(low = "red", high = "#009E73", mid = "white", 
  #                            midpoint = 0, limit = c(-1,1), space = "Lab", 
  #                            name="Pearson\nCorrelation") +
  #       theme_minimal()+ # minimal theme
  #       labs(x="",y="")+
  #       theme(
  #         axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
  #         axis.text.y = element_text(size = 12, hjust = 1),
  #         plot.title = element_text(size=20)
  #       )+
  #       ggtitle(paste("Colheita - Matriz de Correlação",ifelse(num_matriz==24,"(sem falha)","(com falha)"),"-",amostras,"amostras")) +
  #       coord_fixed()+
  #       geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  #       theme(
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank(),
  #         panel.grid.major = element_blank(),
  #         panel.border = element_blank(),
  #         panel.background = element_blank(),
  #         axis.ticks = element_blank())
  #     ,file=parse(text=paste("dist/resultados/colheita/A_Colheita_Matriz_correlação",num_matriz,".png",sep = "")),width = 30, height = 25, units = "cm")
  # }