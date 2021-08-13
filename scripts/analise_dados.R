# Ler bibliotecas ---------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)

# Preparar cores
cores <- c("#CBAACB", "#ABDEE6", "#F3B0C3", "#8FCACA", "#FFC8A2", "#FFFFB5",  "#CCE2CB", "#ECD5E3", "#FEE1E8", "#D4F0F0")

# Not in
`%!in%` <- Negate(`%in%`)
theme_set(theme_classic(base_family = 'serif', base_size = 12))

# Ler base de dados -------------------------------------------------------
decisoes <- read.xlsx("data/Decisões em habeas corpus - TJMG.xlsx", startRow = 2)

# Geral -------------------------------------------------------------------

# * Divisao por câmara criminal -------------------------------------------
camara_criminal <- decisoes %>%
  count(Câmara.Criminal.julgadora) %>%
  mutate(total = sum(n),
         prop = n / total)

g_camara_criminal <- camara_criminal %>%
  ggplot(aes(x = "", y = prop, fill = Câmara.Criminal.julgadora)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1L)), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "serif")) +
  scale_fill_manual(values=cores) +
  labs(title = "Câmara Criminal julgadora")
  
g_camara_criminal  

ggsave(plot = g_camara_criminal, "produtos/camara_criminal.png")

# * Divisao por mês ---------------------------------------------------------
# Mudar para número de acordãos por mes
mes <- decisoes %>%
  mutate(Data.de.publicação.do.acórdão = ifelse(Data.de.publicação.do.acórdão == "0201-04-15", "2021-04-15", Data.de.publicação.do.acórdão),
         Data.de.publicação.do.acórdão = ifelse(Data.de.publicação.do.acórdão == "0020-10-28", "2020-10-28", Data.de.publicação.do.acórdão),
         Data.de.publicação.do.acórdão = ifelse(Data.de.publicação.do.acórdão == "2020-01-27", "2021-01-27", Data.de.publicação.do.acórdão),
         Data.de.publicação.do.acórdão = ifelse(Data.de.publicação.do.acórdão == "2020-02-25", "2021-02-25", Data.de.publicação.do.acórdão),
         Data.de.publicação.do.acórdão = ifelse(Data.de.publicação.do.acórdão == "2020-01-28", "2021-01-28", Data.de.publicação.do.acórdão),
         Data.de.publicação.do.acórdão = as.Date(Data.de.publicação.do.acórdão, formate = "%Y-%m-%d"),
         mes_publicacao = str_to_title(month(Data.de.publicação.do.acórdão, label = TRUE, abbr = FALSE)),
         ano_publicacao = year(Data.de.publicação.do.acórdão),
         data_publicacao = paste(mes_publicacao, "de", ano_publicacao),
         data_publicacao = factor(data_publicacao, 
                                  levels = c("Abril de 2020", "Maio de 2020", "Junho de 2020", "Julho de 2020",
                                             "Julho de 2020", "Agosto de 2020", "Setembro de 2020", "Outubro de 2020",
                                             "Novembro de 2020", "Dezembro de 2020", "Janeiro de 2021",
                                             "Fevereiro de 2021", "Março de 2021", "Abril de 2021", "Maio de 2021"),
                                  labels = c("Abril de 2020", "Maio de 2020", "Junho de 2020", "Julho de 2020",
                                             "Julho de 2020", "Agosto de 2020", "Setembro de 2020", "Outubro de 2020",
                                             "Novembro de 2020", "Dezembro de 2020", "Janeiro de 2021",
                                             "Fevereiro de 2021", "Março de 2021", "Abril de 2021", "Maio de 2021")
                                  )) %>%
  count(data_publicacao)

g_mes <- mes %>%
  ggplot(aes(x = data_publicacao, y = n, group = 1, label = n)) +
  geom_line() +
  geom_area(fill="#CBAACB", alpha=0.4) +
  geom_point(size=3, color="#CBAACB") +
  scale_y_continuous(limits = c(0,60)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 270),
        text = element_text(family = "serif")) +
  geom_label(position = position_dodge(0.9), vjust = -0.5, size = 3.5) +
  labs(x = "", y = "Total")

g_mes

#> Meses total acordaos
meses_n <- c(143, 1328, 990, 1231, 887, 683, 470, 577, 392, 336, 621, 638, 495, 514)
meses_nome <- c("Abr / 2020", "Mai / 2020", "Jun / 2020", "Jul / 2020",
                "Ago / 2020", "Set / 2020", "Out / 2020",
                "Nov / 2020", "Dez / 2020", "Jan / 2021",
                "Fev / 2021", "Mar / 2021", "Abr / 2021", "Mai / 2021")
meses <- data.frame(meses_nome, meses_n) %>%
  mutate(meses_nome = factor(meses_nome, label = c("Abr / 2020", "Mai / 2020", "Jun / 2020", "Jul / 2020",
                                                   "Ago / 2020", "Set / 2020", "Out / 2020",
                                                   "Nov / 2020", "Dez / 2020", "Jan / 2021",
                                                   "Fev / 2021", "Mar / 2021", "Abr / 2021", "Mai / 2021"),
                             levels = c("Abr / 2020", "Mai / 2020", "Jun / 2020", "Jul / 2020",
                                        "Ago / 2020", "Set / 2020", "Out / 2020",
                                        "Nov / 2020", "Dez / 2020", "Jan / 2021",
                                        "Fev / 2021", "Mar / 2021", "Abr / 2021", "Mai / 2021")))

g_meses_acordaos <- meses %>%
  ggplot(aes(x = meses_nome, y = meses_n, group = 1, label = meses_n)) +
  geom_line() +
  geom_area(fill="#CBAACB", alpha=0.4) +
  geom_point(size=3, color="#CBAACB") +
  scale_y_continuous(limits = c(0,1500)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 270),
        text = element_text(family = "serif")) +
  geom_label(position = position_dodge(0.9), vjust = -0.5, size = 3.5) +
  labs(x = "", y = "Total", title = expression(paste("Quantidade de ", italic("habeas corpus "), "por mês")))

g_meses_acordaos

ggsave("produtos/decisoes_total_por_mes.png", plot = g_meses_acordaos, width = 6, height = 5)

# * Resultado Habeas Corpus ----------------------------------------------------------
decisoes <- decisoes %>%
  mutate(Resultado.do.habeas.corpus_2 = ifelse(Resultado.do.habeas.corpus == "Prejudicado (liberdade concedida no juízo de origem)", 
                                             "Prejudicado", Resultado.do.habeas.corpus),
         Resultado.do.habeas.corpus_2 = case_when(Resultado.do.habeas.corpus_2 %in% c("Prejudicado", "Não conhecido") ~ "Prejudicado / NC",
                                                Resultado.do.habeas.corpus_2 %in% c("Ordem concedida", "Ordem parcialmente concedida") ~ "Ordem concedida / PC",
                                                TRUE ~ Resultado.do.habeas.corpus_2),
         Resultado.do.habeas.corpus_2 = factor(Resultado.do.habeas.corpus_2, levels = c("Prejudicado / NC", "Ordem denegada","Ordem concedida / PC"),
                                             labels = c("Prejudicado / Não conhecido", "Denegado", "Concedido / Parcialmente Concedido")))

resultado_geral <- decisoes %>%
  count(Resultado.do.habeas.corpus_2) %>%
  mutate(total = sum(n),
         Percentual = n / total)


g_resultado_geral <- resultado_geral %>%
  ggplot(aes(x = Resultado.do.habeas.corpus_2, y = Percentual, fill = Resultado.do.habeas.corpus_2)) +
  geom_bar(stat = "identity", width = 0.65) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x = "", y = "", title = expression(paste("Resultado dos ", italic("habeas corpus")))) +
  geom_text(aes(label = scales::percent(Percentual, accuracy = 1L)), vjust = -1) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif"))

g_resultado_geral

ggsave("produtos/Resultado_habeas_corpus.png", plot = g_resultado_geral)

#> Outro
camara_criminal_denegados_ou_concedidos <- decisoes %>%
  filter(Resultado.do.habeas.corpus_2 %in% c("Concedido / Parcialmente Concedido", "Denegado")) %>%
  count(Câmara.Criminal.julgadora) %>%
  mutate(total = sum(n),
         prop = n / total)

# * Sexo --------------------------------------------------------------------
sexo <- decisoes %>%
  count(Gênero.do.paciente) %>%
  mutate(total = sum(n),
         Percentual = n / total)

g_sexo <- sexo %>%
  ggplot(aes(x = Gênero.do.paciente, y = Percentual, fill = Gênero.do.paciente)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x = "", y = "", title = "Gênero do impetrante") +
  geom_text(aes(label = scales::percent(Percentual, accuracy = 1L)), vjust = -1) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif"))
  
g_sexo  

ggsave("produtos/sexo_impetrante.png", plot = g_sexo)

# * Natureza da prisão ----------------------------------------------------

natureza_prisao <- decisoes %>%
  mutate(Natureza.da.prisão_2 = case_when(Natureza.da.prisão %in% c("Prisão temporária", 
                                                                    "Prisão preventiva decretada", 
                                                                    "Prisão em flagrante convertida em preventiva") ~ "Prisão provisória",
                                          TRUE ~ Natureza.da.prisão)) %>%
  count(Natureza.da.prisão_2) %>%
  mutate(total = sum(n),
         Percentual = n / total)

g_natureza_prisao <- natureza_prisao %>%
  ggplot(aes(x = Natureza.da.prisão_2, y = Percentual, fill = Natureza.da.prisão_2)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x = "", y = "", title = "Natureza da prisão") +
  geom_text(aes(label = scales::percent(Percentual, accuracy = 1L)), vjust = -1) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif"))

g_natureza_prisao

ggsave("produtos/natureza_prisão.png", plot = g_natureza_prisao, width = 5, height = 4)

# * Crimes cometidos --------------------------------------------------------

#crime <- decisoes %>%
#  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
#  select(crime_1, crime_2, crime_3) %>%
#  pivot_longer(everything(), names_to = "Crime_Cometido", values_to = "Total") %>%
#  group_by(Total)

crimes_1 <- decisoes %>%
  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
  count(crime_1)

crimes_2 <- decisoes %>%
  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
  count(crime_2)

crimes_3 <- decisoes %>%
  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
  count(crime_3)

crimes <- full_join(crimes_1, crimes_2, by = c("crime_1" = "crime_2"), suffix = c("_1", "_2")) %>%
  full_join(crimes_3, by = c("crime_1" = "crime_3"), suffix = c("_1", "_3")) %>%
  filter(!is.na(crime_1)) %>%
  group_by(crime_1) %>%
  summarise(Total = sum(n, n_1, n_2, na.rm = TRUE))

rm(crimes_1, crimes_2, crimes_3)

crimes_2 <- crimes %>%
  mutate(Crime_Cometido = ifelse(Total <= 5, "Outros", crime_1),
         Crime_Cometido = factor(Crime_Cometido, 
                                 levels = c("Estupro", "Furto", "Homicídio", "Porte/posse de armas",
                                                            "Receptação", "Roubo", "Tráfico de drogas", "Violência doméstica",  
                                                            "Outros", "Não informado"),
                                 labels = c("Estupro", "Furto", "Homicídio", "Porte/posse de armas",
                                            "Receptação", "Roubo", "Tráfico de drogas", "Violência doméstica",  
                                            "Outros", "Não informado"))) %>%
  group_by(Crime_Cometido) %>%
  summarise(Total = sum(Total)) %>%
  mutate(total = sum(Total),
         Percentual = Total / total,
         Crime_Cometido = fct_reorder(Crime_Cometido, Percentual))

# Lista com os crimes que se tornaram "Outros"
crimes_x <- crimes %>%
  filter(Total <= 5)
crimes_outros <- crimes_x$crime_1
rm(crimes_x)

# Grafico
g_crimes <- crimes_2 %>%
  ggplot(aes(x = Crime_Cometido, y = Percentual, fill = Crime_Cometido)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,0.6)) +
  labs(x = "", y = "", title = "Crimes cometidos") +
  geom_text(aes(label = scales::percent(Percentual, accuracy = 1L)), hjust = -0.3) +
  coord_flip() +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif"))


g_crimes

ggsave("produtos/crimes_cometidos.png", plot = g_crimes)

# Tabela
t_crimes_1 <- decisoes %>%
  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
  count(crime_1, Resultado.do.habeas.corpus_2)

t_crimes_2 <- decisoes %>%
  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
  count(crime_2, Resultado.do.habeas.corpus_2)

t_crimes_3 <- decisoes %>%
  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
  count(crime_3, Resultado.do.habeas.corpus_2)

t_crimes <- bind_rows(t_crimes_1, t_crimes_2, t_crimes_3) %>%
  filter(!is.na(crime_1)) %>%
  mutate(Crime_Cometido = ifelse(crime_1 %in% crimes_outros, "Outros", crime_1),
         Crime_Cometido = factor(Crime_Cometido, 
                                 levels = c("Estupro", "Furto", "Homicídio", "Porte/posse de armas",
                                            "Receptação", "Roubo", "Tráfico de drogas", "Violência doméstica",  
                                            "Outros", "Não informado"),
                                 labels = c("Estupro", "Furto", "Homicídio", "Porte/posse de armas",
                                            "Receptação", "Roubo", "Tráfico de drogas", "Violência doméstica",  
                                            "Outros", "Não informado"))) %>%
  
  group_by(Crime_Cometido, Resultado.do.habeas.corpus_2) %>%
  summarise(total = sum(n, na.rm = TRUE)) %>%
  mutate(total = ifelse(is.na(total), 0, total)) %>%
  pivot_wider(names_from = Resultado.do.habeas.corpus_2, values_from = total)

t_crimes[is.na(t_crimes)] <- 0

t_crimes <- t_crimes %>%
  mutate(percentual_concedidos = round(`Concedido / Parcialmente Concedido` / sum(`Prejudicado / Não conhecido`, Denegado, `Concedido / Parcialmente Concedido`) * 100, 2),
         percentual_prejudicados = round(`Prejudicado / Não conhecido` / sum(`Prejudicado / Não conhecido`, Denegado, `Concedido / Parcialmente Concedido`) * 100, 2),
         percentual_denegado = round(Denegado / sum(`Prejudicado / Não conhecido`, Denegado, `Concedido / Parcialmente Concedido`) * 100, 2))




# * Comorbidades ------------------------------------------------------------
n_decisoes_comorbidade <- decisoes %>%
  filter(!is.na(`Caso.seja.por.comodidade,.qual?`)) %>%
  nrow()

comorbidades <- decisoes %>%
  filter(!is.na(`Caso.seja.por.comodidade,.qual?`)) %>%
  separate(`Caso.seja.por.comodidade,.qual?`, sep = ";", into = c("comorbidade_1", 'comorbidade_2', "comorbidade_3", "comorbidade_4"), fill = "right") %>%
  select(starts_with("comorbidade")) %>%
  pivot_longer(starts_with("comorbidade"), names_to = "coluna", values_to = "comorbidade") %>%
  select(comorbidade) %>%
  count(comorbidade) %>%
  filter(!is.na(comorbidade)) %>%
  mutate(total = n_decisoes_comorbidade,
         percentual = n / total * 100) %>%
  mutate(comorbidade = ifelse(n < 4, "Outros", comorbidade)) %>%
  group_by(comorbidade) %>%
  summarise(Total = sum(n)) %>%
  mutate(total_geral = n_decisoes_comorbidade,
         percentual = Total / total_geral,
         comorbidade = ifelse(comorbidade == "Diabete", "Diabetes", comorbidade),
         comorbidade = factor(comorbidade, 
                              levels = c("Cardiopatia", "COVID-19",
                                         "Diabetes", "Doença respiratória",
                                         "Doenças psiquiátricas", "HIV",
                                         "Insuficiência renal", "Tuberculose",
                                         "Outros", "Não informado"),
                              labels = c("Cardiopatia", "COVID-19",
                                         "Diabetes", "Doença respiratória",
                                          "Doenças psiquiátricas", "HIV",
                                         "Insuficiência renal", "Tuberculose",
                                         "Outros", "Não informado")
                              ),
         comorbidade = fct_reorder(comorbidade, percentual)
         )

g_comorbidades <- comorbidades %>%
  ggplot(aes(x = comorbidade, y = percentual, fill = comorbidade)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,0.6)) +
  labs(x = "", y = "", title = "Comorbidades alegadas") +
  geom_text(aes(label = scales::percent(percentual, accuracy = 1L)), hjust = -0.3) +
  coord_flip() +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif"))

g_comorbidades

ggsave("produtos/comorbidades_alegadas.png", plot = g_comorbidades)

# Análise dos concedidos --------------------------------------------------
concedidos <- decisoes %>%
  filter(Resultado.do.habeas.corpus_2 == "Concedido / Parcialmente Concedido")

n_concedidos <- nrow(concedidos)

# * Crimes cometidos ------------------------------------------------------
concedidos_crimes_cometidos <- concedidos %>%
  select(Crime.cometido) %>%
  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
  pivot_longer(cols = starts_with("crime"), names_to = "ignora", values_to = "Crime") %>%
  count(Crime) %>%
  filter(!is.na(Crime))

# * Crimes cometidos por violencia ----------------------------------------
concedidos_crimes_cometidos_violencia <- concedidos %>%
  select(Crime.cometido) %>%
  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
  pivot_longer(cols = starts_with("crime"), names_to = "ignora", values_to = "Crime") %>%
  filter(!is.na(Crime)) %>%
  mutate(Crime_clean = case_when(Crime %in% c("Roubo", "Homicídio", "Extorsão", "Estupro", "Violência doméstica", "Lesão corporal") ~ "Com violência ou grave ameaça",
                                 Crime %in% c("Receptação", "Furto", "Estelionato", "Tráfico de drogas", "Posse de armas",
                                              "Porte/posse de armas", "Crimes de trânsito") ~ "Sem violência ou grave ameaça",
                                 TRUE ~ Crime),
         Crime_clean = factor(Crime_clean, levels = c("Com violência ou grave ameaça",
                                                      "Sem violência ou grave ameaça",
                                                      "Não informado"),
                              labels = c("Com violência ou grave ameaça",
                                         "Sem violência ou grave ameaça",
                                         "Não informado"))) %>%
  count(Crime_clean, name = "Total") %>%
  mutate(total_geral = sum(Total),
         Percentual = Total / total_geral)
  

g_concedidos_crimes_cometidos_violencia <- concedidos_crimes_cometidos_violencia %>%
  ggplot(aes(x = Crime_clean, y = Percentual, fill = Crime_clean)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "", y = "", title = expression(paste(italic("Habeas corpus "), "concedidos: natureza do crime"))) +
  geom_text(aes(label = scales::percent(Percentual, accuracy = 1L)), vjust = -1) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif"))

g_concedidos_crimes_cometidos_violencia

ggsave("produtos/concedidos_natureza_crime.png", plot = g_concedidos_crimes_cometidos_violencia, width = 5, height = 4)

# * Argumentos trazidos ---------------------------------------------------
concedidos_argumentos <- concedidos %>%
  separate(Argumentos.trazidos.para.concessão.da.ordem, sep = ";", into = c("argumento_1", "argumento_2"), fill = "right")

argumento_1 <- concedidos_argumentos$argumento_1
argumento_2 <- filter(concedidos_argumentos, !is.na(argumento_2))
argumento_2 <- argumento_2$argumento_2
concedidos_argumentos <- data.frame(argumentos = c(argumento_1, argumento_2)) %>%
  count(argumentos, name = "Total") %>%
  mutate(total_geral = n_concedidos,
         Percentual = Total / total_geral) %>%
  mutate(argumentos = ifelse(argumentos == "Paciente se enquadra nos requisitos", "Paciente se enquadra nos requisitos da recomendação do CNJ", argumentos),
         argumentos = factor(argumentos),
         argumentos = fct_reorder(argumentos, Percentual))


rm(argumento_1, argumento_2)

g_concedidos_argumentos <- concedidos_argumentos %>%
  ggplot(aes(x = argumentos, y = Percentual, fill = argumentos)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,0.6)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "", y = "", title = expression(paste(italic("Habeas corpus "), "concedidos: fundamentos"))) +
  geom_text(aes(label = scales::percent(Percentual, accuracy = 1L)), hjust = -0.3) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif")) +
  coord_flip()

g_concedidos_argumentos

ggsave("produtos/concedidos_fundamentos.png", plot = g_concedidos_argumentos, width = 7, height = 5.3)

# * Comorbidade -----------------------------------------------------------
concedidos_n_decisoes_comorbidade <- concedidos %>%
  filter(!is.na(`Caso.seja.por.comodidade,.qual?`)) %>%
  nrow()

concedidos_comorbidades <- concedidos %>%
  filter(!is.na(`Caso.seja.por.comodidade,.qual?`)) %>%
  separate(`Caso.seja.por.comodidade,.qual?`, sep = ";", into = c("comorbidade_1", 'comorbidade_2', "comorbidade_3", "comorbidade_4"), fill = "right") %>%
  select(starts_with("comorbidade")) %>%
  pivot_longer(starts_with("comorbidade"), names_to = "coluna", values_to = "comorbidade") %>%
  select(comorbidade) %>%
  count(comorbidade) %>%
  filter(!is.na(comorbidade)) %>%
  mutate(total = concedidos_n_decisoes_comorbidade,
         percentual = n / total * 100) %>%
  group_by(comorbidade) %>%
  summarise(Total = sum(n)) %>%
  mutate(total_geral = concedidos_n_decisoes_comorbidade,
         percentual = Total / total_geral,
         comorbidade = ifelse(comorbidade == "Diabete", "Diabetes", comorbidade),
         comorbidade = factor(comorbidade),
         comorbidade = fct_reorder(comorbidade, percentual))

g_concedidos_comorbidades <- concedidos_comorbidades %>%
  ggplot(aes(x = comorbidade, y = percentual, fill = comorbidade)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,0.8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  geom_text(aes(label = scales::percent(percentual, accuracy = 1L)), hjust = -0.3) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif"),
        plot.title = element_text(family = "serif")) +
  labs(x = "", y = "", title = expression(paste(italic("Habeas corpus "), "concedidos: comorbidades"))) +
  coord_flip()

g_concedidos_comorbidades

ggsave("produtos/concedidos_comorbidades.png", plot = g_concedidos_comorbidades, width = 5, height = 4)  

# * Juntou comprovação? ---------------------------------------------------
concedidos_comprovacao <- concedidos %>%
  count(`O.impetrante.juntou.comprovação.da.situação.do.paciente?`) %>%
  filter(!is.na(`O.impetrante.juntou.comprovação.da.situação.do.paciente?`))

concedidos_comorbidades_comprovacao <- concedidos %>%
  separate(Argumentos.relacionados.às.recomendações, sep = ";", into = c("argumento_1", 'argumento_2'), fill = "right") %>%
  select(starts_with("argumento_"), `O.impetrante.juntou.comprovação.da.situação.do.paciente?`) %>%
  pivot_longer(starts_with("argumento_"), names_to = "coluna", values_to = "argumentos") %>%
  filter(!is.na(argumentos)) %>%
  group_by(argumentos, `O.impetrante.juntou.comprovação.da.situação.do.paciente?`) %>%
  summarise(total = n()) %>%
  filter(!is.na(`O.impetrante.juntou.comprovação.da.situação.do.paciente?`) & argumentos %!in% c("Regime de cumprimento de pena",
                                                                                                "Situação do estabelecimento prisional"))


# * Tipo de prisao --------------------------------------------------------
concedidos_tipo_prisao <- concedidos %>%
  mutate(Natureza.da.prisão_2 = case_when(Natureza.da.prisão %in% c("Prisão temporária", 
                                                                    "Prisão preventiva decretada", 
                                                                    "Prisão em flagrante convertida em preventiva") ~ "Prisão provisória",
                                          TRUE ~ Natureza.da.prisão)) %>%
  count(Natureza.da.prisão_2) %>%
  mutate(total = sum(n),
         Percentual = n / total)

g_concedidos_tipo_prisao <- concedidos_tipo_prisao %>%
  ggplot(aes(x = Natureza.da.prisão_2, y = Percentual, fill = Natureza.da.prisão_2)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "", y = "", title = expression(paste(italic("Habeas corpus "), "concedidos: natureza da prisão"))) +
  geom_text(aes(label = scales::percent(Percentual, accuracy = 1L)), vjust = -1) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif"))

g_concedidos_tipo_prisao

ggsave("produtos/concedidos_tipo_prisao.png", plot = g_concedidos_tipo_prisao, width = 5, height = 4)  

# * Camara criminal -------------------------------------------------------
concedidos_camara_criminal <- concedidos %>%
  count(Câmara.Criminal.julgadora) %>%
  left_join(camara_criminal_denegados_ou_concedidos[,c(1:2)], by = "Câmara.Criminal.julgadora", suffix = c("_concedidos", "_total")) %>%
  mutate(prop = n_concedidos / n_total,
         total_geral = sum(n_concedidos),
         prop_concedidos = n_concedidos / total_geral* 100)

# Análise dos denegados ---------------------------------------------------
denegados <- decisoes %>%
  filter(Resultado.do.habeas.corpus_2 == "Denegado")


# * Crimes cometidos ------------------------------------------------------
denegados_crimes_cometidos <- denegados %>%
  select(Crime.cometido) %>%
  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
  pivot_longer(cols = starts_with("crime"), names_to = "ignora", values_to = "Crime") %>%
  count(Crime) %>%
  filter(!is.na(Crime)) %>%
  mutate(Crime = ifelse(n <= 5, "Outros", Crime)) %>%
  group_by(Crime) %>%
  summarise(Total = sum(n))

# * Crimes cometidos por violencia ----------------------------------------
denegados_crimes_cometidos_violencia <- denegados %>%
  select(Crime.cometido) %>%
  separate(Crime.cometido, sep = ";", into = c("crime_1", "crime_2", "crime_3"), fill = "right") %>%
  pivot_longer(cols = starts_with("crime"), names_to = "ignora", values_to = "Crime") %>%
  filter(!is.na(Crime)) %>%
  mutate(Crime = str_trim(Crime),
         Crime_clean = case_when(Crime %in% c("Roubo", "Homicídio", "Extorsão", "Estupro", "Violência doméstica", "Lesão corporal") ~ "Crimes com violência ou grave ameaça",
                                 Crime %in% c("Receptação", "Furto", "Estelionato", "Tráfico de drogas", "Posse de armas",
                                              "Porte/posse de armas", "Crimes de trânsito",
                                              "Violação de direito autoral",
                                              "Tráfico de influência e exploração de prestígio",
                                              "Falsificar, corromper, adulterar ou alterar produto destinado a fins terapêuticos ou medicinais",
                                              "Corrupção ativa") ~ "Crimes sem violência ou grave ameaça",
                                 TRUE ~ Crime)) %>%
  count(Crime_clean, name = "Total") %>%
  mutate(total_geral = sum(Total),
         Percentual = Total / total_geral)

g_denegados_crimes_cometidos_violencia <- denegados_crimes_cometidos_violencia %>%
  ggplot(aes(x = Crime_clean, y = Percentual, fill = Crime_clean)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "", y = "", title = expression(paste(italic("Habeas corpus "), "denegados: natureza do crime"))) +
  geom_text(aes(label = scales::percent(Percentual, accuracy = 1L)), vjust = -1) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif"))

g_denegados_crimes_cometidos_violencia

ggsave("produtos/denegados_natureza_crime.png", plot = g_denegados_crimes_cometidos_violencia, width = 5, height = 4)


# * Argumentos trazidos ---------------------------------------------------
denegados_n_decisoes <- denegados %>%
  nrow()

denegados_argumentos <- denegados %>%
  separate(Argumentos.trazidos.para.denegação.da.ordem, sep = ";", into = c("argumento_1", "argumento_2", "argumento_3", "argumento_4", "argumento_5", "argumento_6"), fill = "right") %>%
  select(starts_with("argumento_")) %>%
  pivot_longer(cols = starts_with("argumento_"), names_to = "ignora", values_to = "Argumento") %>%
  filter(!is.na(Argumento)) %>%
  count(Argumento) %>%
  mutate(Argumento = ifelse(n < 9, "Outros", Argumento)) %>%
  group_by(Argumento) %>%
  summarise(Total = sum(n),
            Percentual = Total / denegados_n_decisoes) %>%
  mutate(Argumento = factor(Argumento),
         Argumento = fct_reorder(Argumento, Percentual))

g_denegados_argumentos <- denegados_argumentos %>%
  ggplot(aes(x = Argumento, y = Percentual, fill = Argumento)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "", y = "", title = expression(paste(italic("Habeas corpus "), "denegados: fundamentos"))) +
  geom_text(aes(label = scales::percent(Percentual, accuracy = 1L)), hjust = -0.3) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif")) +
  coord_flip()

g_denegados_argumentos

ggsave("produtos/denegados_fundamentos.png", plot = g_denegados_argumentos, width = 7, height = 6)

# * Comorbidade -----------------------------------------------------------
denegados_n_decisoes_comorbidade <- denegados %>%
  filter(!is.na(`Caso.seja.por.comodidade,.qual?`)) %>%
  nrow()

denegados_comorbidades <- denegados %>%
  filter(!is.na(`Caso.seja.por.comodidade,.qual?`)) %>%
  separate(`Caso.seja.por.comodidade,.qual?`, sep = ";", into = c("comorbidade_1", 'comorbidade_2', "comorbidade_3", "comorbidade_4"), fill = "right") %>%
  select(starts_with("comorbidade")) %>%
  pivot_longer(starts_with("comorbidade"), names_to = "coluna", values_to = "comorbidade") %>%
  select(comorbidade) %>%
  count(comorbidade) %>%
  filter(!is.na(comorbidade)) %>%
  mutate(comorbidade = ifelse(n < 4, "Outros", comorbidade)) %>%
  group_by(comorbidade) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(total = denegados_n_decisoes_comorbidade,
         percentual = n / total * 100) %>%
  group_by(comorbidade) %>%
  summarise(Total = sum(n)) %>%
  mutate(total_geral = denegados_n_decisoes_comorbidade,
         percentual = Total / total_geral,
         comorbidade = ifelse(comorbidade == "Diabete", "Diabetes", comorbidade),
         comorbidade = factor(comorbidade),
         comorbidade = fct_reorder(comorbidade, percentual))

g_denegados_comorbidades <- denegados_comorbidades %>%
  ggplot(aes(x = comorbidade, y = percentual, fill = comorbidade)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "", y = "", title = expression(paste(italic("Habeas corpus "), "denegados: comorbidades"))) +
  geom_text(aes(label = scales::percent(percentual, accuracy = 1L)), hjust = -0.3) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif")) +
  coord_flip()

g_denegados_comorbidades

ggsave("produtos/denegados_comorbidades.png", plot = g_denegados_comorbidades, width = 5, height = 4)  


# * Juntou comprovação? ---------------------------------------------------
denegados_comprovacao <- denegados %>%
  count(`O.impetrante.juntou.comprovação.da.situação.do.paciente?`) %>%
  filter(!is.na(`O.impetrante.juntou.comprovação.da.situação.do.paciente?`))

denegados_comorbidades_comprovacao <- denegados %>%
  separate(Argumentos.relacionados.às.recomendações, sep = ";", into = c("argumento_1", 'argumento_2', "argumento_3", "argumento_4"), fill = "right") %>%
  select(starts_with("argumento_"), `O.impetrante.juntou.comprovação.da.situação.do.paciente?`) %>%
  pivot_longer(starts_with("argumento_"), names_to = "coluna", values_to = "argumentos") %>%
  filter(!is.na(argumentos) & argumentos != "Necessidade de reavaliação das prisões") %>%
  group_by(argumentos, `O.impetrante.juntou.comprovação.da.situação.do.paciente?`) %>%
  summarise(total = n()) %>%
  filter(!is.na(`O.impetrante.juntou.comprovação.da.situação.do.paciente?`) & argumentos %!in% c("Regime de cumprimento de pena",
                                                                                                 "Situação do estabelecimento prisional")) %>%
  group_by(argumentos) %>%
  mutate(total_grupo = sum(total),
         percentual = total / total_grupo * 100)


# * Tipo de prisao --------------------------------------------------------
denegados_tipo_prisao <- denegados %>%
  mutate(Natureza.da.prisão_2 = case_when(Natureza.da.prisão %in% c("Prisão temporária", 
                                                                    "Prisão preventiva decretada", 
                                                                    "Prisão em flagrante convertida em preventiva") ~ "Prisão provisória",
                                          TRUE ~ Natureza.da.prisão)) %>%
  count(Natureza.da.prisão_2) %>%
  mutate(total = sum(n),
         Percentual = n / total)

g_denegados_tipo_prisao <- denegados_tipo_prisao %>%
  ggplot(aes(x = Natureza.da.prisão_2, y = Percentual, fill = Natureza.da.prisão_2)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(x = "", y = "", title = expression(paste(italic("Habeas corpus "), "denegados: natureza da prisão"))) +
  geom_text(aes(label = scales::percent(Percentual, accuracy = 1L)), vjust = -1) +
  theme_classic() +
  theme(legend.position="none",
        text = element_text(family = "serif"))

g_denegados_tipo_prisao

ggsave("produtos/denegados_tipo_prisao.png", plot = g_denegados_tipo_prisao, width = 5, height = 4)  

# * Camara criminal -------------------------------------------------------
denegados_camara_criminal <- denegados %>%
  count(Câmara.Criminal.julgadora) %>%
  left_join(camara_criminal_denegados_ou_concedidos[,c(1:2)], by = "Câmara.Criminal.julgadora", suffix = c("_denegados", "_total")) %>%
  mutate(prop = n_denegados / n_total)

#> Gráfico concedidos e denegados câmara criminal
camara_criminal_geral <- denegados_camara_criminal[,c(1,4)] %>%
  left_join(concedidos_camara_criminal[, c(1,4)], by = "Câmara.Criminal.julgadora",
            suffix = c("_denegados", "_concedidos")) %>%
  pivot_longer(!`Câmara.Criminal.julgadora`, names_to = "analise", values_to = "percentual") %>%
  mutate(analise = ifelse(analise == "prop_denegados", "Denegados", "Concedidos"),
         analise = factor(analise, levels = c("Concedidos", "Denegados"),
                          labels = c("Concedidos", "Denegados")),
         `Câmara.Criminal.julgadora` = str_sub(`Câmara.Criminal.julgadora`, 1, 2)) %>%
  rename(`Resultado` = analise)

g_camara_criminal_geral <- camara_criminal_geral %>%
  ggplot(aes(fill=Resultado, y=percentual, x=`Câmara.Criminal.julgadora`)) + 
  geom_bar(position="stack", stat="identity", width = 0.75) +
  scale_fill_manual(values = cores[c(7,1)]) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  labs(x = "", y = "", title = "Decisões por Câmara Criminal") +
  theme_classic() +
  geom_text(aes(label = scales::percent(percentual, accuracy = 1L)), position = position_stack(vjust = 0.5), size = 2.7) +
  theme(text = element_text(family = "serif"))

g_camara_criminal_geral

ggsave("produtos/decisoes_por_camara_criminal.png", plot = g_camara_criminal_geral)
