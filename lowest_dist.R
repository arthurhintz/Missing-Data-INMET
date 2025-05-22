# Não tem dados em TOrres esse ano de 2025

rm(list = ls())
# First, I selected just the estations nearest 
library(geosphere)
library(tidyverse)

estacoes <- read.csv2("estacoesRS.csv")

#estacoes$VL_LONGITUDE <- as.numeric(gsub(",", ".", estacoes$VL_LONGITUDE))
#estacoes$VL_LATITUDE <- as.numeric(gsub(",", ".", estacoes$VL_LATITUDE))


# Estou tirando Torres (40) pq não tem no ano de 2025 dados
# Mostarda tbm não tem nenhum valor 
# Filtrando estações inválidas
estacoes_validas <- estacoes[!(estacoes$DC_NOME %in% c("TORRES", "MOSTARDAS")), ]
idx_validos <- which(!(estacoes$DC_NOME %in% c("TORRES", "MOSTARDAS")))

dados <- as.matrix(estacoes_validas[, c(4, 3)])

distancias <- distm(dados, fun = distGeo)

lowest_ind <- function(vetor) {
  order(vetor)[2:3] 
}

ind_estac <- apply(distancias, 1, lowest_ind)


print("Índices dos três menores valores por linha:")
print(ind_estac)

valor_dist <- apply(distancias, 1, function(x) sort(x)[2:3])

print("\nValores dos três menores por linha:")
print(valor_dist)

ind_estac <- t(ind_estac)


dataframe <- data.frame(
  origem = idx_validos,  # índice original da estação de origem
  coluna1 = idx_validos[ind_estac[,1]],
  coluna2 = idx_validos[ind_estac[,2]]
)

# Parte da estação de origem
parte_origem <- left_join(dataframe, estacoes, by = c("origem" = "X")) |> 
  select(origem, DC_NOME_ORIGEM = DC_NOME, CD_ESTACAO_ORIGEM = CD_ESTACAO)

# Parte da primeira vizinha
parte1 <- left_join(dataframe, estacoes, by = c("coluna1" = "X")) |> 
  select(DC_NOME_1 = DC_NOME, CD_ESTACAO_1 = CD_ESTACAO)

# Parte da segunda vizinha
parte2 <- left_join(dataframe, estacoes, by = c("coluna2" = "X")) |> 
  select(DC_NOME_2 = DC_NOME, CD_ESTACAO_2 = CD_ESTACAO)

# Combinando tudo
end <- cbind(parte_origem, parte1, parte2)
end
# Limpando objetos
rm(valor_dist, dados, distancias, parte_origem, parte2, parte1, dataframe)
#____________________________________________________

# Now I need to list and to fill the NAs

library(imputeTS)

names(end) <- c("cod", "NOME_EST_1" , "COD_EST_1", "NOME_EST_2" , "COD_EST_2", "NOME_EST_3" , "COD_EST_3")



lista_vetor <- as.character(paste(end$COD_EST_1, "_", end$NOME_EST_1, sep = ""))
lista_vetor


indices = seq_along(end$COD_EST_1)

arq <- "_H_2024-06-01_2025-05-20.csv" 
diretorio <- "inmet_24-25/dados_"

for (j in indices){
  
  print(j)
  
  a = end$COD_EST_1[j]  
  b = end$COD_EST_2[j]
  c = end$COD_EST_3[j]  
  
  cat("EST 1:", a, "EST 2:", b, "EST 3:", c)
  
  
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
  
  tudo_safe <- tudo
  
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
    summarise(Temp_max = round(max(Temp_max_1, na.rm = T), 3),
              Temp_min = round(min(Temp_min_1, na.rm = T), 3),
              Media_vento = round(mean(Vel_Vento_1, na.rm = T), 3),
              Temp_AR_Bulbo = round(mean(Temp_AR_Bulbo_1, na.rm = T), 3),
              Temp_PO = round(mean(Temp_PO_1, na.rm = T), 3), 
              Umidade_med = round(mean(Umidade_Relativa_1, na.rm = T), 3),
              Radiacao = round(sum(ifelse(Radiacao_1 < 0, 0, Radiacao_1), na.rm = TRUE), 3),
              Preciptacao_total = sum(Preciptacao_Total_h_1, na.rm = T))
  
  #_______________________Mexendo ainda_____Possivel erro_______
 
  # Criar uma coluna indicadora para valores preenchidos
png(file = paste("graficos/", a, ".png", sep = ""),  width = 600, height = 500)
  {  
  ver_data <- tudo_safe |> 
    group_by(Data_1) |> 
    summarise(Temp_max_1 = mean(Temp_max_1, na.rm = TRUE)) |> 
    mutate(situacao = ifelse(!is.na(Temp_max_1), "Completo", "Missing"))
  
  teste <- left_join(last_data, ver_data, by = c("Data_1")) |> 
    select(c("Data_1", "Temp_max", "situacao"))
  
#   # Plotar os valores
#   plot(teste$Data_1[teste$situacao == "Completo"], teste$Temp_max[teste$situacao == "Completo"], 
#        col = "blue",
#        lwd = 1, lty = 1,
#        type = "l",
#        xlab = "Tempo", ylab = "Temperatura",
#        ylim = range(c(data1$Temp_max_1, tudo$Temp_max_1), 
#                     na.rm = TRUE))
#   points(teste$Data_1[teste$situacao == "Missing"], teste$Temp_max[teste$situacao == "Missing"], col = "red", lwd = 1, lty = 8)
#   legend("topright", legend = c("Completo", "Missing"), col = c("blue", "red"), lty = c(1,2), lwd = 2)
# #___________________________________  
#   # Plotar os valores
  plot(teste$Data_1, y = teste$Temp_max, 
       col = "blue",
       type = "l", lwd = 2, lty = 1,
       xlab = "Tempo", ylab = "Temperatura",
       ylim = range(c(data1$Temp_max_1, tudo$Temp_max_1), na.rm = TRUE))
  
  # Adicionar pontos coloridos para destacar os valores "Missing"
  points(teste$Data_1[teste$situacao == "Missing"], teste$Temp_max[teste$situacao == "Missing"],
         col = "red", pch = 16, cex = 1, type = "p")
  
  
  # Adicionar legendas específicas para as linhas "Completo" e "Missing"
  legend("topright", legend = c("Completo", "Missing"), col = c("blue", "red"), lty = 1, lwd = 2)
  

  
 }
dev.off()
  
    # Some plots
  
  
  # par(mfrow = c(2,2))
  # plot.ts(last_data$Temp_max)
  # plot.ts(last_data$Media_vento)
  # plot(last_data$Preciptacao_total)
  # plot.ts(last_data$Temp_min)

print(a)

# Exemplo para temperatura máxima
rmse <- sqrt(mean((tudo$Temp_max_1 - data1$Temp_max_1)^2, na.rm = TRUE))

if(rmse != 0){
  
  cat("O valor RMSE =", rmse)
}


last_data$cod <- a
  
write.csv2(last_data,  paste("ajust_met/",lista_vetor[j],".csv", sep = ""))

}

  