library(curl)
library(dplyr)
library(jsonlite)
library(readr)
library(stringr)

NOME_APOSTADOR_STR <- "Nome do(a) palpitedor(a) *"

load_palpites <- function(file = "palpites.csv") {
  palpites <- read_csv(file, col_types = "ccici")
  return(palpites)
}

load_siglas_paises <- function(file = "dados/siglas_paises_copa.csv") {
  sigla_paises <- read_csv(file, col_types = "cc")
  return(sigla_paises)
}

parse_palpites <- function(input_file, output_file = "palpites.csv") {
  palpites_input <- read_lines(file)
  palpites <- data_frame()
  palpitedor <- NULL
  remove_lines <- palpites_input[1:3]
  i <- 4
  
  while(i <= length(palpites_input)) {
    if ((line <- palpites_input[i]) == NOME_APOSTADOR_STR) {
      i <- i + 1
      palpitedor <- palpites_input[i]
    } else if (!(line %in% remove_lines)) {
      times <- str_split(line, " x ", simplify = TRUE) %>% str_remove_all("[*\f]") %>% str_trim()
      times <- times[times != ""]
      if (length(times) == 2) {
        time1 <- times[1]
        time2 <- times[2]
        i <- i + 1
        line <- palpites_input[i]
        resultado <- str_split(line, "x", simplify = TRUE) %>% str_trim()
        gols_time1 <- as.integer(resultado[1])
        gols_time2 <- as.integer(resultado[2])
        palpites <- bind_rows(palpites, data_frame(palpitedor, time1, gols_time1, time2, gols_time2))
      }
    }
    i <- i + 1
  }
  write_csv(palpites, output_file)
  return(palpites)
}

get_placares <- function(file = "dados/placares_copa.csv", update_file = FALSE) {
  if (update_file) {
    placares <- update_placares_file(file)
  } else {
    placares <- read_csv(file, col_types = "Dicci")
  }
  return(placares)
}

update_placares_file <- function(
    file = "dados/placares_copa.csv",
    url = "https://api.fifa.com/api/v1/calendar/matches?idseason=254645&idcompetition=17") {
  
  placares <- fromJSON(url, flatten = TRUE) %>%
    getElement("Results") %>%
    jsonlite::flatten() %>%
    select(Date, Home.Score, Home.IdCountry, Away.IdCountry, Away.Score) %>%
    mutate(Date = as.Date(Date))
  write_csv(placares, file)
  return(placares)
}

merge_palpites_placares <- function(palpites, placares, siglas) {
  df <- palpites %>%
    left_join(siglas, by = c("time1" = "nome")) %>%
    rename(Home.IdCountry = sigla) %>%
    left_join(siglas, by = c("time2" = "nome")) %>%
    rename(Away.IdCountry = sigla) %>%
    left_join(placares, by = c("Home.IdCountry", "Away.IdCountry"))
  return(df)
}

get_resultados <- function(palpites_file = "dados/palpites_3a_rodada.csv",
                           update_placares = FALSE) {
  palpites <- load_palpites(palpites_file)
  placares <- get_placares(update_file = update_placares)
  siglas <- load_siglas_paises()
  resultados <- merge_palpites_placares(palpites, placares, siglas) %>%
    mutate(pontos = calcula_pontuacao(gols_time1, gols_time2, Home.Score, Away.Score))
  return(resultados)
}

acertou_em_cheio <- function(palpite_gols_t1, palpite_gols_t2, gols_t1, gols_t2) {
  return(palpite_gols_t1 == gols_t1 & palpite_gols_t2 == gols_t2) 
}

acertou_vencedor <- function(palpite_gols_t1, palpite_gols_t2, gols_t1, gols_t2) {
  vencedor_t1 <- ((palpite_gols_t1 > palpite_gols_t2) & (gols_t1 > gols_t2))
  vencedor_t2 <- ((palpite_gols_t2 > palpite_gols_t1) & (gols_t2 > gols_t1))
  empate <- ((palpite_gols_t2 == palpite_gols_t1) & (gols_t2 == gols_t1))
  return(vencedor_t1 | vencedor_t2 | empate) 
}

calcula_pontuacao <- function(palpite_gols_t1, palpite_gols_t2, gols_t1, gols_t2) {
  pontos <- if_else(acertou_em_cheio(palpite_gols_t1, palpite_gols_t2, gols_t1, gols_t2), 3,
                    if_else(acertou_vencedor(palpite_gols_t1, palpite_gols_t2, gols_t1, gols_t2),
                    1, 0))
  return(pontos)
}


  

main <- function() {
  palpites <- load_palpites("dados/palpites_3a_rodada.csv")
  placares <- get_placares()
  siglas <- load_siglas_paises()
  resultados <- merge_palpites_placares(palpites, placares, siglas)
}
