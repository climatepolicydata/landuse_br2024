library(tidyverse)
##################################################################
# Script containing all words combination to classify the sectors#
##################################################################

######################################################################################################################################################################################################

# BNDES Sectors
# Bioenergy

bioenergy_search_pattern_BNDES <- function(data_frame_BNDES,Coluna_search){
    data_frame_bioenergy <- data_frame_BNDES %>% filter((grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\balcool\\b",x = Coluna_search,ignore.case = TRUE)) |
    (grepl(pattern = "\\bfabricacao\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern =  "\\bbiocombustiveis\\b",x = Coluna_search, ignore.case = TRUE)) |
    (grepl(pattern = "\\bco-geracao\\b",x = Coluna_search, ignore.case = TRUE))|
    (grepl(pattern = "\\bgeracao\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\benergia\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\beletrica\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\bfontes\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\balternat\\b",x = Coluna_search, ignore.case = TRUE)) |
    (grepl(pattern = "\\bgeracao\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\benergia\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\btermica\\b",x = Coluna_search, ignore.case = TRUE)) |
    (grepl(pattern = "\\bproducao\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\bdistribuicao\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\bvapor\\b",x = Coluna_search, ignore.case = TRUE)) |
    (grepl(pattern = "\\bbiomassa\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\bcana\\b",x = Coluna_search, ignore.case = TRUE)) |
    (grepl(pattern = "\\bgeracao\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\bvapor",x = Coluna_search, ignore.case = TRUE)) |
    (grepl(pattern = "\\benergia\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\bbagaco\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\bcana\\b",x = Coluna_search, ignore.case = TRUE)) |
    (grepl(pattern = "\\bmilho\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\betanol\\b",x = Coluna_search, ignore.case = TRUE)) |
    (grepl(pattern = "\\btermoeletrica\\b",x = Coluna_search, ignore.case = TRUE) & grepl(pattern = "\\bbiogas\\b",x = Coluna_search, ignore.case = TRUE))|
    (grepl("\\bcaptacao\\b",x = Coluna_search, ignore.case = TRUE) & grepl("\\bbiometano\\b",x = Coluna_search, ignore.case = TRUE)))
    return(data_frame_bioenergy)

}

# Cattle
cattle_search_pattern_BNDES <- function(data_frame_BNDES,Coluna_search){
    data_frame_cattle <- data_frame_BNDES %>% filter((grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baves\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\banimais\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsuinos\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbovinos\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbufalinos\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bequinos\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsubprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsubproduto\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babelha\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babelhas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bapicultura\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\baquicultura\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\baquicultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdoce\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\baquicultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsalgada\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baquicultura\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpecuaria\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesca\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\banimais vivos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bleite\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\blaticinios\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpescados\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfrutos do mar\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\banimal\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\banimais\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\basininos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baves\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbicho\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbovinos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbufalinos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcamaroes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcaprinos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl('\\bequinos\\b', x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfrangos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bostras\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgalinaceos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bovinos\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpeixes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsuinos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcult\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bsemicult\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balimentos\\b", x = Coluna_search , ignore.case = TRUE)&grepl("\\banimais\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE)& grepl("\\balimentos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconservas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blaticinios\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmedicamentos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bveterinario\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balimenticios\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcarne\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgordura\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bfrigorifico\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bmatadouro\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bmoagem\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bamilaceos", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpecuaria\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpesca\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcrustaceos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpesca\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpeixes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpesca\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baquicultura\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpesca\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdoce\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpesca\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsalgada\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpreparacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpreparacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bleite\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpreservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpeixes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bpreservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpescado\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bovos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpintos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsementes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bservico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binseminacao\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bservico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmanejo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\banimais\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcompostagem\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\borganica\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bovos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnatura\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\babate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baves\\b",x = Coluna_search , ignore.case = TRUE)))
    return(data_frame_cattle)
}

# Forest
forest_search_pattern_BNDES <- function(data_frame_BNDES,Coluna_search){
    data_frame_forest <- data_frame_BNDES %>% filter((grepl(pattern = "\\bjardim botanico\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bzoo\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bpq nac\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\breserva eco\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bprot ambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grep(pattern = "\\bproducao florestal\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bpatrimonio\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bcoleta\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\castanha\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bcoleta\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\blatex\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bcoleta\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bfloresta nativa\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bcomercio\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bsementes\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bcomercio\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern="\\bflores\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern="\\bcomercio\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bplantas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bcomercio\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bgramas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bconservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bflorestas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bcult\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bespecies\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern="\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bacacia\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\beucalipto\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl(pattern = "\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bmudas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bpinus\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bteca\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bextracao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bmadeira\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bnativas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bextracao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bmadeira\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bplantadas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfab\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bpapel\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfab\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfab\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bpstas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\babsorventes\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bcartolina\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bcelulose\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bchapas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bembalagens\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE ) & grepl(pattern = "\\bformularios\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bfraldas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bpapel\\b", x = Coluna_search , ignore.case = TRUE) )|
    (grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bembalagem\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bpro\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bnao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bmadeira\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bcarvao\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bflorestal\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bhectares\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bfloresta\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bhectares", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bflorestas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\brestauracao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bflorestal\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bplantio\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bfloresta\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl(pattern = "\\bprograma\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bflorestal\\b", x = Coluna_search , ignore.case = TRUE)))
    return(data_frame_forest)

}

#Multi Sector
multiSector_search_pattern_BNDES <- function(data_frame_BNDES,Coluna_search){
    data_frame_multisector <- data_frame_BNDES %>% filter((grepl(pattern = "\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bpecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bservicos\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\batv\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bpecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\boleos\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bgorduras\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bvegetais\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\banimais\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bexperimental\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\bserv\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bagronomia\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bconsultoria\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl(pattern = "\\businas\\b", x = Coluna_search , ignore.case = TRUE) & grepl(pattern = "\\bcompostagem\\b", x = Coluna_search , ignore.case = TRUE)))
    return(data_frame_multisector)
}

# Crop
crop_search_pattern_BNDES <- function(data_frame_BNDES,Coluna_search){
    data_frame_crop <- data_frame_BNDES %>% filter((grepl("\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcolheita\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bbeneficiamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\barroz\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bbeneficiamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafe\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcom\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\batac\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcereal\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcomercio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\batacadista\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcult\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boutras\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfibras\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcult\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boutras\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boleaginosas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcult\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bplantas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babacaxi\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bacai\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balgodao\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balho\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bamendoim\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\barroz\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbanana\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbatata\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcacau\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafe\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcaju\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcana\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcebola\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcereais\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcha\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcitricos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcoco\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdende\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\berva\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfeijao\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflores\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfumo\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgirassol\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blaranja\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmaca\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmamao\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmandioca\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmanga\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmaracuja\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmelancia\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmelao\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmilho\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmorango\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boleaginosas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boutros\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcereais\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpessego\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpimenta\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bplantas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsoja\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btomate\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btrigo\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\buva\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfrutas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blavoura\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boutras\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bplantas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcultivo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bplantas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blavoura\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabri\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brefresco\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconservas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bacucar\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\badocantes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\badubos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baguardente\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baguardentes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baguas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balimentos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bamidos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbebidas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiscoitos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcervejas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcha\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcigarrilhas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcigarros\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdefensivos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bespeciarias\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfarinha\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfermentos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfrutas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmalte\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmassa\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boleo\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boleos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boutras\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baguardentes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boutros\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafe\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balimenticios\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpanificacao\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\barroz\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfumo\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binfusao\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brefrigerantes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsorvetes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsucos\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bvinho\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bvinagres\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brefino\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bacucar\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfarinha\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmilho\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boutras\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbeb\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boutros\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfumo\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bhorticultura\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bhorticultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbfloricultura\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bmoagem\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btrigo\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bmoagem\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfabricacao\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bprocessamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bindustrial\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfumo\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blavouras\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsementes\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmudas\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bservico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpoda\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bservico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpreparacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bterreno\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bservico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpulverizacao\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\btorrefacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmoagem\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafe\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\capacidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmoagem\\b", x = Coluna_search , ignore.case = TRUE)))
    return(data_frame_crop)
}
##############################################################################################################################################################################################################################################################

# Para SIOP:
