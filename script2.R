library(dplyr)

options(max.print = 10000)

filenames <- c("https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_01_circulo_eleitoral_aveiro.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_02_circulo_eleitoral_beja.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_03_circulo_eleitoral_braga.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_04_circulo_eleitoral_braganca.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_05_circulo_eleitoral_castelo_branco.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_06_circulo_eleitoral_coimbra.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_07_circulo_eleitoral_evora.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_08_circulo_eleitoral_faro.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_09_circulo_eleitoral_guarda.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_10_circulo_eleitoral_leiria.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_12_circulo_eleitoral_portalegre.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_13_circulo_eleitoral_porto.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_14_circulo_eleitoral_santarem.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_15_circulo_eleitoral_setubal.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_16_circulo_eleitoral_viana_castelo.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_17_circulo_eleitoral_vila_real.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_18_circulo_eleitoral_viseu.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_30_circulo_eleitoral_ra_madeira.pdf",
"https://www.cne.pt/sites/default/files/dl/eleicoes/2022_ar/listas_candidatos/2022ar_listas_candidatos_40_circulo_eleitoral_ra_acores.pdf")

extract_data <- function(x){
  temp <- pdftools::pdf_text(x)
  temp_circ <- sub(".*circulo_eleitoral_", "", x)
  temp <- stringr::str_split(temp, "\n")
  temp <- data.frame(mine = unlist(temp))
  temp$partido <- grepl("^[A-Z]+", temp$mine) & (grepl("–", temp$mine) | grepl("-", temp$mine)) &
    !grepl("Miguel Almeida Corte-Real Gomes", temp$mine) &
    !grepl("Abel Filipe Marques Pereira – Independente", temp$mine) &
    !grepl("Cristina Paula Silva Oliveira – Independente", temp$mine) &
    !grepl("Marta Raquel dos Santos Macedo – Independente", temp$mine)
    
  temp <- temp[temp$mine != "", ]
  temp <- temp[-(1:3), ]
  temp$candidatos_e <- grepl("Efetivos", temp$mine)
  temp$candidatos_s <- grepl("Suplentes", temp$mine)
  
  for(i in 1:nrow(temp)){
    temp$partido2[i] <- ifelse(temp$partido[i] == TRUE,
                                  temp$mine[i],
                                  temp$partido2[i-1])
  }
  temp <- temp[temp$partido != TRUE, ]
  
  for(i in 1:nrow(temp)){
    temp$candidatos[i] <- ifelse(temp$candidatos_e[i] == TRUE,
                                    "Efectivo",
                                    ifelse(temp$candidatos_s[i-1] == TRUE,
                                           "Suplente", NA))
    temp$candidatos[i] <- ifelse(!is.na(temp$candidatos[i]),
                                    temp$candidatos[i],
                                    temp$candidatos[i-1])
  }
  
  temp <- select(temp, -c(candidatos_e, candidatos_s, partido))
  
  temp <- temp[!grepl("Candidatos Efetivos", temp$mine) &
                       !grepl("Candidatos Suplentes", temp$mine) & !grepl("Página", temp$mine), ]
  
  temp <- temp %>%
    group_by(partido2) %>%
    mutate(ordem = 1:n()) %>%
    ungroup
  
  temp$circ <- temp_circ
  
  names(temp) <- c("Candidato", "Partido", "Efectivo/Suplente","Ordem",  "Círculo")
  temp <- temp[, c(1, 2, 4, 5, 3)]
  temp$Candidato <- stringr::str_squish(temp$Candidato)
  temp
}


data1 <- lapply(filenames, extract_data)
data1 <- do.call(rbind.data.frame, data1)

#write.csv(unique(data1$Partido), "check_partidos.csv")
partidos <- read.csv("partidos.csv", header = T, sep = ";")

data1 <- data1 %>%
  mutate(Círculo = stringi::stri_replace_all_fixed(Círculo,
                                                   unique(data1$Círculo),
                                                   c("Aveiro", "Beja", "Braga", "Bragança", "Castelo Branco", "Coimbra",
                                                     "Évora", "Faro", "Guarda", "Leiria", "Portalegre",
                                                     "Porto", "Santarém", "Setúbal", "Viana do Castelo", "Vila Real", "Viseu",
                                                     "Madeira", "Açores"), vectorize_all = FALSE),
         Partido = stringi::stri_replace_all_fixed(Partido,
                                                   partidos$original,
                                                   partidos$corrigido,
                                                   vectorize_all = FALSE),
         Ordem = sprintf("%02d", Ordem))



#saveRDS(data1, "listas.rds")
#write.csv(data1, "listas.csv", row.names = F)      

#DT::datatable(data1, rownames = F)
