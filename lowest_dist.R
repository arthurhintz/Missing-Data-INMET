rm(list = ls())
# First, I selected just the estations nearest 
library(geosphere)

estacoes <- read.csv2("estacoesRS.csv")

estacoes$VL_LONGITUDE <- as.numeric(gsub(",", ".", estacoes$VL_LONGITUDE))
estacoes$VL_LATITUDE <- as.numeric(gsub(",", ".", estacoes$VL_LATITUDE))

dados <- as.matrix(estacoes[,c(4,3)])

distancias <- distm(dados, fun = distGeo)


lowest_ind <- function(vetor) {
  return(order(vetor)[1:3])
}


ind_estac <- apply(distancias, 1, lowest_ind)

print("Índices dos três menores valores por linha:")
print(ind_estac)

valor_dist <- apply(distancias, 1, function(x) sort(x)[1:3])


print("\nValores dos três menores por linha:")
print(valor_dist)


rm(valor_dist, dados, distancias)

## agora preciso relacionar o indice com o número do cod da estacão
library(tidyverse)

ind_estac <- t(ind_estac)

dataframe <- data.frame(coluna1 = ind_estac[,1],
                        coluna2 = ind_estac[,2],
                        coluna3 = ind_estac[,3])

parte1 <- left_join(dataframe, estacoes, by = c("coluna1" = "X")) |> 
          select(coluna1, DC_NOME, CD_ESTACAO)
  
parte2 <- left_join(dataframe, estacoes, by = c("coluna2" = "X")) |> 
          select(coluna2, DC_NOME, CD_ESTACAO)

parte3 <- left_join(dataframe, estacoes, by = c("coluna3" = "X")) |> 
          select(coluna3, DC_NOME, CD_ESTACAO)


end <- cbind(parte1, parte2, parte3)
end


rm(parte1, parte2, parte3, dataframe)
#____________________________________________________

# Now I need to list and to fill the NAs

library(imputeTS)

names(end) <- c("coluna1", "NOME_EST_1" , "COD_EST_1", "coluna2", "NOME_EST_2" , "COD_EST_2", "coluna3", "NOME_EST_3" , "COD_EST_3")



lista_vetor <- as.character(paste(end$COD_EST_1, "_", end$NOME_EST_1, sep = ""))
lista_vetor


indices = seq_along(end$COD_EST_1)

arq <- "_H_2019-06-01_2023-06-01.csv" 
diretorio <- "dados_met_h/dados_"

for (j in indices){
  
  print(j)
  
  a = end$COD_EST_1[j]  
  b = end$COD_EST_2[j]
  c = end$COD_EST_3[j]  
  
  
  arquivo1 <- paste(diretorio, a, arq, sep = "")  
  
  arquivo2 <- paste(diretorio, b, arq, sep = "") 
  
  arquivo3 <- paste(diretorio, c, arq, sep = "") 
  
  #______________Lendo os dados________________________
  data1 <- tibble::as_tibble(read.csv(arquivo1, 
                   header = T, 
                   skip = 10,
                   sep = ";",
                   colClasses = c("Date", "character", rep("numeric", 8)),
                   na.strings = "null"))
  
  data2 <- tibble::as_tibble(read.csv(arquivo2, 
                   header = T, 
                   skip = 10,
                   sep = ";",
                   colClasses = c("Date", "character", rep("numeric", 8)),
                   na.strings = "null"))
  
  data3 <- tibble::as_tibble(read.csv(arquivo3, 
                   header = T, 
                   skip = 10,
                   sep = ";",
                   colClasses = c("Date", "character", rep("numeric", 8)),
                   na.strings = "null"))

  
  data1 <- data1 |> 
    select(c(1:10))
  names(data1) <- c("Data_1", "Hora_1", "Preciptacao_Total_h_1", "Radiacao_1", "Temp_AR_Bulbo_1", "Temp_PO_1", "Temp_max_1", "Temp_min_1",  "Umidade_Relativa_1", "Vel_Vento_1")
  data1$timestemp <- as.POSIXct(paste(data1$Data_1, data1$Hora_1), format="%Y-%m-%d %H%M")
  
  
  data2 <- data2 |> 
    select(c(1:10))
  names(data2) <- c("Data_2", "Hora_2", "Preciptacao_Total_h_2", "Radiacao_2", "Temp_AR_Bulbo_2", "Temp_PO_2", "Temp_max_2", "Temp_min_2",  "Umidade_Relativa_2", "Vel_Vento_2")
  data2$timestemp <- as.POSIXct(paste(data2$Data_2, data2$Hora_2), format="%Y-%m-%d %H%M")
  
  
  data3 <- data3 |> 
    select(c(1:10))
  names(data3) <- c("Data_3", "Hora_3", "Preciptacao_Total_h_3", "Radiacao_3", "Temp_AR_Bulbo_3", "Temp_PO_3", "Temp_max_3", "Temp_min_3",  "Umidade_Relativa_3", "Vel_Vento_3")
  data3$timestemp <- as.POSIXct(paste(data3$Data_3, data3$Hora_3), format="%Y-%m-%d %H%M")
  
  
  tudo <- left_join(data1, data2, by = c("timestemp"))
  tudo <- left_join(tudo, data3, by = c("timestemp"))
  
  
  # Verificando NAs
  # which(is.na(data1$Preciptacao_Total_h_1))

  # linhas_com_NA <- data1[apply(is.na(data1), 1, any), ]
  
#_______________________________________________________________________________  
  
  #______Verificando e aplicando em Preciptacao Total
  # For precipitation I think it's better not to interpolate
  
  # na_count_per_day <- data1 %>%
  #   select(Data_1, Preciptacao_Total_h_1) %>%
  #   group_by(Data_1) %>%
  #   summarise(na_count = sum(is.na(Preciptacao_Total_h_1)))
  #   
  # print(na_count_per_day)
  # 
  # filtered_days <- na_count_per_day %>%
  #   filter(na_count >= 24)
  # 
  # Aplica as transformações apenas nos dias filtrados
  tudo <- tudo %>%
    mutate(
      Preciptacao_Total_h_1 = case_when(
        is.na(Preciptacao_Total_h_1) ~ rowMeans(select(., c("Preciptacao_Total_h_2", "Preciptacao_Total_h_3")), na.rm = TRUE),
        TRUE ~ Preciptacao_Total_h_1
      )
    )
  
  tudo$Preciptacao_Total_h_1 <- na.interpolation(tudo$Preciptacao_Total_h_1, option = "linear") 
  
  
#____________Radiação______________________________________
  {  
  na_count_per_day <- data1 %>%
    select(Data_1, Radiacao_1) %>%
    group_by(Data_1) %>%
    summarise(na_count = sum(is.na(Radiacao_1)))
  
  filtered_days <- na_count_per_day %>%
    filter(na_count >= 24)
  
  # Aplica as transformações apenas nos dias filtrados
  tudo <- tudo %>%
    mutate(
      Radiacao_1 = case_when(
        is.na(Radiacao_1) & Data_1 %in% filtered_days$Data_1 ~ rowMeans(select(., c("Radiacao_2", "Radiacao_3")), na.rm = TRUE),
        TRUE ~ Radiacao_1
      )
    )
  
  
  # Interpolação
  tudo$Radiacao_1 <- na.interpolation(tudo$Radiacao_1, option = "linear") 
  }
#____________Temperatura do Ar no Bulbo______________________________________
  {
  na_count_per_day <- data1 %>%
    select(Data_1, Temp_AR_Bulbo_1) %>%
    group_by(Data_1) %>%
    summarise(na_count = sum(is.na(Temp_AR_Bulbo_1)))
  
  filtered_days <- na_count_per_day %>%
    filter(na_count >= 24)
  
  # Aplica as transformações apenas nos dias filtrados
  tudo <- tudo %>%
    mutate(
      Temp_AR_Bulbo_1 = case_when(
        is.na(Temp_AR_Bulbo_1) & Data_1 %in% filtered_days$Data_1 ~ rowMeans(select(., c("Temp_AR_Bulbo_2", "Temp_AR_Bulbo_3")), na.rm = TRUE),
        TRUE ~ Temp_AR_Bulbo_1
      )
    )
  
  # Interpolação
  tudo$Temp_AR_Bulbo_1 <- na.interpolation(tudo$Temp_AR_Bulbo_1, option = "linear") 
  }
#____________Temperatura Ponto de Orvalho______________________________________
  {
  na_count_per_day <- data1 %>%
    select(Data_1, Temp_PO_1) %>%
    group_by(Data_1) %>%
    summarise(na_count = sum(is.na(Temp_PO_1)))
  
  filtered_days <- na_count_per_day %>%
    filter(na_count >= 24)
  
  # Aplica as transformações apenas nos dias filtrados
  tudo <- tudo %>%
    mutate(
      Temp_PO_1 = case_when(
        is.na(Temp_PO_1) & Data_1 %in% filtered_days$Data_1 ~ rowMeans(select(., c("Temp_PO_2", "Temp_PO_3")), na.rm = TRUE),
        TRUE ~ Temp_PO_1
      )
    )
  # Interpolação
  tudo$Temp_PO_1 <- na.interpolation(tudo$Temp_PO_1, option = "linear") 
  }
  
  #____________Temperatura Máxima______________________________________
  {
  na_count_per_day <- data1 %>%
    select(Data_1, Temp_max_1) %>%
    group_by(Data_1) %>%
    summarise(na_count = sum(is.na(Temp_max_1)))
  
  filtered_days <- na_count_per_day %>%
    filter(na_count >= 24)
  
  # Aplica as transformações apenas nos dias filtrados
  tudo <- tudo %>%
    mutate(
      Temp_max_1 = case_when(
        is.na(Temp_max_1) & Data_1 %in% filtered_days$Data_1 ~ rowMeans(select(., c("Temp_max_2", "Temp_max_3")), na.rm = TRUE),
        TRUE ~ Temp_max_1
      )
    )
  
  # Interpolação
  tudo$Temp_max_1 <- na.interpolation(tudo$Temp_max_1, option = "linear") 
  }
  
  #____________Temperatura Mínima______________________________________
  {
  na_count_per_day <- data1 %>%
    select(Data_1, Temp_min_1) %>%
    group_by(Data_1) %>%
    summarise(na_count = sum(is.na(Temp_min_1)))
  
  filtered_days <- na_count_per_day %>%
    filter(na_count >= 24)
  
  # Aplica as transformações apenas nos dias filtrados
  tudo <- tudo %>%
    mutate(
      Temp_min_1 = case_when(
        is.na(Temp_min_1) & Data_1 %in% filtered_days$Data_1 ~ rowMeans(select(., c("Temp_min_2", "Temp_min_3")), na.rm = TRUE),
        TRUE ~ Temp_min_1
      )
    )  
  
  # Interpolação
  tudo$Temp_min_1 <- na.interpolation(tudo$Temp_min_1, option = "linear") 
  }
  #____________Umidade Relativa ______________________________________
  {
  na_count_per_day <- data1 %>%
    select(Data_1, Umidade_Relativa_1) %>%
    group_by(Data_1) %>%
    summarise(na_count = sum(is.na(Umidade_Relativa_1)))
  
  filtered_days <- na_count_per_day %>%
    filter(na_count >= 24)
  
  # Aplica as transformações apenas nos dias filtrados
  tudo <- tudo %>%
    mutate(
      Umidade_Relativa_1 = case_when(
        is.na(Umidade_Relativa_1) & Data_1 %in% filtered_days$Data_1 ~ rowMeans(select(., c("Umidade_Relativa_2", "Umidade_Relativa_3")), na.rm = TRUE),
        TRUE ~ Umidade_Relativa_1
      )
    )  
  
  # Interpolação
  tudo$Umidade_Relativa_1 <- na.interpolation(tudo$Umidade_Relativa_1, option = "linear") 
  }
  #____________Velocidade do Vento______________________________________
  {
  na_count_per_day <- data1 %>%
    select(Data_1, Vel_Vento_1) %>%
    group_by(Data_1) %>%
    summarise(na_count = sum(is.na(Vel_Vento_1)))
  
  filtered_days <- na_count_per_day %>%
    filter(na_count >= 24)
  
  # Aplica as transformações apenas nos dias filtrados
  tudo <- tudo %>%
    mutate(
      Vel_Vento_1 = case_when(
        is.na(Vel_Vento_1) & Data_1 %in% filtered_days$Data_1 ~ rowMeans(select(., c("Vel_Vento_2", "Vel_Vento_3")), na.rm = TRUE),
        TRUE ~ Vel_Vento_1
      )
    )  
  
  # Interpolação
  tudo$Vel_Vento_1 <- na.interpolation(tudo$Vel_Vento_1, option = "linear") 
  }
  colSums(is.na(tudo))
 #_____________________________________________________________________ 
  # Now, I need to agroup for days
  # First I will plot some graphs for analyse performace
  
  # plot.ts(tudo$Temp_max_1)
  # plot.ts(data1$Temp_max_1)
  # 
  # plot.ts(tudo$Temp_AR_Bulbo_1)
  # plot.ts(data1$Temp_AR_Bulbo_1)
  # 
  # plot.ts(tudo$Temp_min_1)
  # plot.ts(data1$Temp_min_1)
  # 
  
  #___________ Agrupando os dados por dia __________________
  last_data <- tudo |> 
    select(1:10) |> 
    group_by(Data_1) |> 
    summarise(Temp_max = mean(Temp_max_1, na.rm = T),
              Temp_min = mean(Temp_min_1, na.rm = T),
              Media_vento = mean(Vel_Vento_1, na.rm = T),
              Temp_AR_Bulbo = mean(Temp_AR_Bulbo_1, na.rm = T),
              Temp_PO = mean(Temp_PO_1, na.rm = T),
              Umidade_med = mean(Umidade_Relativa_1, na.rm = T),
              Radiacao = sum(Radiacao_1, na.rm = T),
              Preciptacao_total = sum(Preciptacao_Total_h_1, na.rm = T))
  
  
  # Some plots
  
  par(mfrow = c(2,2))
  plot.ts(last_data$Temp_max)
  plot.ts(last_data$Media_vento)
  plot(last_data$Preciptacao_total)
  plot.ts(last_data$Temp_min)
  
last_data$cod <- a
  
write.csv2(last_data,  paste("ajust_met/",lista_vetor[j],".csv", sep = ""))

}

  