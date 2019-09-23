#### ATP_Analysis ####

setwd("CAMINHO_DO_ARQUIVO")

library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)
library(lubridate)


##### Tabela "jogador" #####

# Importando os dados
jogador <- read.csv("player.csv", stringsAsFactors = FALSE) %>%
  as_tibble(jogador)

names(jogador)

# Formatando a data de nascimento
jogador$birthdate <- jogador$birthdate %>% ymd()

head(jogador$birthdate)

# Verificando a existência de redundâncias
num_jogadores <- length(jogador$player_id)

num_jogadores

n_distinct(jogador)

jogador <- distinct(jogador, player_id, .keep_all = TRUE)


# Criando a coluna id_jogador
jogador$id_jogador <- seq_along(jogador$player_id)


# Extraindo apenas o primeiro nome da cidade (não é a melhor solução,
# mas deixei como ilustração)
jogador$cidade_nasc <- word(jogador$birthplace, 1) %>% 
  str_replace(",", "")


# Criando coluna com o nome completo
jogador$nome <- paste0(jogador$first_name,"_", jogador$last_name)


# Índices que serão utilizados para substituir "player_id" em
# outras tabelas

indices_1 <- as_tibble(jogador$player_id)

indices_2 <- as_tibble(jogador$id_jogador)


indices <- bind_cols(indices_1, indices_2)

colnames(indices) <- c("player_id", "id_jogador")


#Verificando a existência de nomes repetidos
n_distinct(jogador$nome)


# Vetor lógico referente aos nomes repetidos
jogador$nome_log <- duplicated(jogador$nome)

referencia <- filter(jogador, jogador$nome_log == TRUE)


# Jogadores com nome repetido
nome_ref <- referencia$nome


# Verificando se são pessoas diferentes
filter(jogador, jogador$nome == nome_ref[33])


# Apagando colunas "desnecessárias"
jogador[ , c(1, 2, 3, 4, 5, 8, 10, 11, 12, 14, 16, 17, 24)] <- NULL

jogador <- jogador[ ,c("id_jogador", "nome", "flag_code", "cidade_nasc", "birthdate", "residence", "turned_pro", "height_cm", "weight_kg", "handedness", "backhand")]

names(jogador)




###### Tabela "torneio" #######

# Importando dados
torneio <- read.csv("tournaments_1877-2017.csv", stringsAsFactors = FALSE) %>%
  as_tibble(torneio)

names(torneio)

head(torneio)


# Formatando a data do torneio
torneio$tourney_dates <- ymd(torneio$tourney_dates)

head(torneio$tourney_dates)


# Inserindo os id's ("foreign keys")

torneio <- left_join(torneio, indices, by = c("singles_winner_player_id" = "player_id"))

colnames(indices) <- c("player_id", "id_jogador1")

torneio <- left_join(torneio, indices, by = c("doubles_winner_1_player_id" = "player_id"))

colnames(indices) <- c("player_id", "id_jogador2")

torneio <- left_join(torneio, indices, by = c("doubles_winner_2_player_id" = "player_id"))






########## Investigando a base: missing values, distinct id's


# Buscando o número de linhas correto
n_distinct(torneio$tourney_year_id)

nrow(torneio)


# Comparando o tamanho de tourney_year_id com o de tourney_id
torneio_year_split <- separate(torneio, tourney_year_id, into = c("year_id", "tour_id"), sep = "-")

n_distinct(torneio_year_split$tour_id)

n_distinct(torneio_year_split$tourney_id)


# Comparando os NA's dos id's

filter(torneio_year_split, torneio_year_split$tour_id == "")

torneio_filter <- filter(torneio_year_split, torneio_year_split$tour_id == "")

torneio_names_na <- torneio_filter$tourney_name

torneio_names_na

filter(torneio, torneio$tourney_name == "Nice")

filter(torneio, torneio$tourney_name == "Tokyo WCT")



# Configurando o id_torneio

torneio_id <- torneio_year_split$tour_id






















