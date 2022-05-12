# Ler bibliotecas ---------------------------------------------------------

library(tidyverse)

# Criar df com amostra randomizada ----------------------------------------
acordaos_sorteados <- data.frame(n = sample.int(9305, 369))

acordaos_sorteados <- acordaos_sorteados %>%
  arrange(n)

# Numero dos acordãos por mes analisado no site do TJMG
meses <- c(143, 1328, 990, 1231, 887, 683, 470, 577, 392, 336, 621, 638, 495, 514)
meses <- as.integer(meses)

# Nova localização
for (i in 1:length(meses)) {
  if (i > 1) {
    meses[i] <- meses[i] + meses[i-1]
  }
  
}

# Organizar os números sorteados por mês e posição no site do TJMG
acordaos_sorteados <- acordaos_sorteados %>%
  mutate(`Mês` = case_when(n < meses[1] ~ "Abril de 2020",
                           n %in% c((meses[1]+1):meses[2]) ~ "Maio de 2020",
                           n %in% c((meses[2]+1):meses[3]) ~ "Junho de 2020",
                           n %in% c((meses[3]+1):meses[4]) ~ "Julho de 2020",
                           n %in% c((meses[4]+1):meses[5]) ~ "Agosto de 2020",
                           n %in% c((meses[5]+1):meses[6]) ~ "Setembro de 2020",
                           n %in% c((meses[6]+1):meses[7]) ~ "Outubro de 2020",
                           n %in% c((meses[7]+1):meses[8]) ~ "Novembro de 2020",
                           n %in% c((meses[8]+1):meses[9]) ~ "Dezembro de 2020",
                           n %in% c((meses[9]+1):meses[10]) ~ "Janeiro de 2021",
                           n %in% c((meses[10]+1):meses[11]) ~ "Fevereiro de 2021",
                           n %in% c((meses[11]+1):meses[12]) ~ "Março de 2021",
                           n %in% c((meses[12]+1):meses[13]) ~ "Abril de 2021",
                           n > meses[13] ~ "Junho de 2021"),
         `Posição` = case_when(n < meses[1] ~ n,
                               n %in% c((meses[1]+1):meses[2]) ~ n - meses[1],
                               n %in% c((meses[2]+1):meses[3]) ~ n - meses[2],
                               n %in% c((meses[3]+1):meses[4]) ~ n - meses[3],
                               n %in% c((meses[4]+1):meses[5]) ~ n - meses[4],
                               n %in% c((meses[5]+1):meses[6]) ~ n - meses[5],
                               n %in% c((meses[6]+1):meses[7]) ~ n - meses[6],
                               n %in% c((meses[7]+1):meses[8]) ~ n - meses[7],
                               n %in% c((meses[8]+1):meses[9]) ~ n - meses[8],
                               n %in% c((meses[9]+1):meses[10]) ~ n - meses[9],
                               n %in% c((meses[10]+1):meses[11]) ~ n - meses[10],
                               n %in% c((meses[11]+1):meses[12]) ~ n - meses[11],
                               n %in% c((meses[12]+1):meses[13]) ~ n - meses[12],
                               n > meses[13] ~ n - meses[13]), 
         `Número do acordão` = NA,
         Analisado = NA,
         ) %>%
  rename(`Número sorteado` = n) %>%
  select(`Mês`, everything())


# Escrever xlsx com os acordãos sorteados ---------------------------------

write.xlsx(acordaos_sorteados, "analise_acordaos.xlsx", asTable = FALSE)
