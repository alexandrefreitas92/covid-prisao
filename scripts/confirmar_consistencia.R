# Ler bibliotecas ---------------------------------------------------------

library(tidyverse)
library(openxlsx)

# Ler base de dados -------------------------------------------------------
acordaos <- read.xlsx("data/analise_acordaos.xlsx")
decisoes <- read.xlsx("data/Decisões em habeas corpus - TJMG.xlsx", startRow = 2)

# * Casos repetidos -------------------------------------------------------
repetidos <- decisoes %>%
  mutate(`Número.do.acórdão`= str_trim(`Número.do.acórdão`),
         `Número.do.acórdão` = str_remove_all(`Número.do.acórdão`, "[^0-9]")) %>%
  group_by(`Número.do.acórdão`) %>%
  summarise(total = n())

# Limpar dados para juntar as bases
decisoes <- decisoes %>%
  mutate(`Número.do.acórdão`= str_trim(`Número.do.acórdão`),
         `Número.do.acórdão` = str_remove_all(`Número.do.acórdão`, "[^0-9]"))

acordaos <- acordaos %>%
  mutate(`Número.do.acordão` = str_trim(`Número.do.acordão`),
         `Número.do.acordão` = str_remove_all(`Número.do.acordão`, "[^0-9]"))

# Juntar as bases procurando casos não encontrados
nao_encontrado <- acordaos %>%
  anti_join(decisoes, by = c("Número.do.acordão" = "Número.do.acórdão"))

# Feitos, porém que nao fazem parte da lista dos que precisavam ser analisados
encontrados_n_pertencentes <- decisoes %>%
  anti_join(acordaos, by = c("Número.do.acórdão" = "Número.do.acordão")) %>%
  mutate(nao_encontradas = TRUE) %>%
  select(`Número.do.acórdão`, nao_encontradas)

