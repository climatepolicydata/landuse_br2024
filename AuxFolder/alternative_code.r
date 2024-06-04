library(dplyr)
library(stringr)

crop_search_pattern_BNDES <- function(data_frame_BNDES, Coluna_search) {
  # Combine all patterns into a single regular expression
  pattern <- paste0("\\b(atividades.*apoio.*agricultura|atividades.*pos.*colheita|",
                    "beneficiamento.*arroz|beneficiamento.*cafe|com.*atac.*cereal|",
                    "comercio.*atacadista|cult.*outras.*fibras|cult.*outras.*oleaginosas|",
                    "cult.*plantas|cultivo.*abacaxi|cultivo.*acai|cultivo.*algodao|",
                    "cultivo.*alho|cultivo.*amendoim|cultivo.*arroz|cultivo.*banana|",
                    "cultivo.*batata|cultivo.*cacau|cultivo.*cafe|cultivo.*caju|",
                    "cultivo.*cana|cultivo.*cebola|cultivo.*cereais|cultivo.*cha|",
                    "cultivo.*citricos|cultivo.*coco|cultivo.*dende|cultivo.*erva|",
                    "cultivo.*feijao|cultivo.*flores|cultivo.*fumo|cultivo.*girassol|",
                    "cultivo.*laranja|cultivo.*maca|cultivo.*mamao|cultivo.*mandioca|",
                    "cultivo.*manga|cultivo.*maracuja|cultivo.*melancia|cultivo.*melao|",
                    "cultivo.*milho|cultivo.*morango|cultivo.*oleaginosas|",
                    "cultivo.*outros.*cereais|cultivo.*pessego|cultivo.*pimenta|",
                    "cultivo.*plantas|cultivo.*soja|cultivo.*tomate|cultivo.*trigo|",
                    "cultivo.*uva|cultivo.*frutas.*lavoura|cultivo.*outras.*plantas|",
                    "cultivo.*plantas.*lavoura|fabri.*refresco|fabricacao.*conservas|",
                    "fabricacao.*acucar|fabricacao.*adocantes|fabricacao.*adubos|",
                    "fabricacao.*aguardente|fabricacao.*aguardentes|fabricacao.*aguas|",
                    "fabricacao.*alimentos|fabricacao.*amidos|fabricacao.*bebidas|",
                    "fabricacao.*biscoitos|fabricacao.*cervejas|fabricacao.*cha|",
                    "fabricacao.*cigarrilhas|fabricacao.*cigarros|fabricacao.*defensivos|",
                    "fabricacao.*especiarias|fabricacao.*farinha|fabricacao.*fermentos|",
                    "fabricacao.*frutas|fabricacao.*malte|fabricacao.*massa|",
                    "fabricacao.*oleo|fabricacao.*oleos|fabricacao.*outras.*aguardentes|",
                    "fabricacao.*outros.*produtos|fabricacao.*produtos.*cafe|",
                    "fabricacao.*produtos.*alimenticios|fabricacao.*produtos.*panificacao|",
                    "fabricacao.*produtos.*arroz|fabricacao.*produtos.*fumo|",
                    "fabricacao.*produtos.*infusao|fabricacao.*refrigerantes|",
                    "fabricacao.*sorvetes|fabricacao.*sucos|fabricacao.*vinho|",
                    "fabricacao.*vinagres|fabricacao.*refino.*acucar|",
                    "fabricacao.*farinha.*milho|fabricacao.*outras.*beb|",
                    "fabricacao.*outros.*produtos.*fumo|horticultura|",
                    "horticultura.*floricultura|moagem.*trigo|moagem.*fabricacao|",
                    "processamento.*industrial.*fumo|producao.*lavouras|",
                    "producao.*sementes|producao.*mudas|servico.*poda|",
                    "servico.*preparacao.*terreno|servico.*pulverizacao|",
                    "torrefacao.*moagem.*cafe|capacidade.*moagem|biofertilizantes)\\b")
  
  # Use str_detect to filter the dataframe
  data_frame_crop <- data_frame_BNDES %>%
    filter(str_detect(Coluna_search, pattern))
  
  return(data_frame_crop)
}

crop_siop <- crop_search_pattern_SIOP(data_frame_SIOP = df_remainder_SIOP1,Coluna_search = Coluna_search)