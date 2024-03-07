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
#bioenergy
bioenergy_search_pattern_SIOP <- function(data_frame_SIOP,Coluna_search){
    data_frame_bioenergy <- data_frame_SIOP %>% filter(
        (grepl("\\bpolitica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiocombustiveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpolitica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiocombustivel\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiocombustiveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiocombustivel\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpetroleo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bderivados\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiocombustiveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbiocombustiveis\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bministerio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bminas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbiocombustivel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bministerio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bminas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroenergia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroenergetica\\b", x = Coluna_search , ignore.case = TRUE))|
        (grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiocombustiveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bindustria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiocombustiveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbiocombustivel\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbiocombustiveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagroenergia\\b", x = Coluna_search , ignore.case = TRUE)) 
        
    )
    return(data_frame_bioenergy)
}
# Crop
crop_search_pattern_SIOP <- function(data_frame_SIOP,Coluna_search){
    data_frame_crop <- data_frame_SIOP %>% filter(
        (grepl("\\breducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\briscos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bestudos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bimplementacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmanutencao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bzoneamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricola\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brisco\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclimatico\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\abastecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomercializacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bassistencia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutor\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("agricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\breforma\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bampliacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blaboratorios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarios\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdefesa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdivulgacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binformacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologicas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclimatologicas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologia\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        
        (grepl("\\bpromocao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brisco\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\borganizacao agraria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfortalecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdinamizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento agrario\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdigitalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bacervo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhistorico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologicos\\b", x = Coluna_search , ignore.case = TRUE)) |
       
        (grepl("\\bprotecao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcultivares\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\buso sustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos geneticos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bterritorios rurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) &grepl("\\bterritorial\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b" , x = Coluna_search , ignore.case = TRUE) & grepl("\\bcombate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpobreza\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagrario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmulheres\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroalimentar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpos colheita\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bavaliacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsafras\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcompanhia nacional de abastecimento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bembrapa\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btecnologico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bengenharia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binovacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\btransferencia\\b" , x = Coluna_search , ignore.case = TRUE) & grepl("\\btecnologias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\btecnologias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bunidades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\breferencia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btecnologica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdifusao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconhecimento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcensos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdemografico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcombate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpobreza\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsemiarido\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
       

        (grepl("\\bfiscalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarias\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfiscalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfertilizantes\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("defesa agropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |        
        (grepl("\\bagroecologia\\b", x = Coluna_search , ignore.case = TRUE)) |
     
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\borganica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcontrole\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\borganica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpro-organico\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsetor\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarios\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricolas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroambientais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bzoneamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivo\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroecossistemas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsistemas produtivos rurais sustentaveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\bp&d\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcompetitividade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentabilidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpd\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcompetitividade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentabilidade\\b" , x = Coluna_search , ignore.case = TRUE) & grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesqueira\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baquicola\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\barranjos produtivos locais\\b", x = Coluna_search , ignore.case = TRUE)) |
      
       
        
        (grepl("\\bcacaueiras\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbrasil\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmiseria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafeicultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprodutores\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagronegocio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafe\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfundo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdefesa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafeeira\\b", x = Coluna_search , ignore.case = TRUE)) |
       
       
      
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeocientifica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarias\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarias\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcenso\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcidadania\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmulheres\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\btrabalhadora\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagrario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bestruturacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutiva\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpequenos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutores rurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmedios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutores rurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsemiarido\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfundo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgarantiasafra\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfundo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsafra\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfundo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgarantia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsafra\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgarantia-safra\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE)  & grepl("\\bmeteorologico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bquarentena vegetal\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bquilombolas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bindigenas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btradicionais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomunidades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bindigenas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btecnologia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binterpretacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binformacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsolos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeneticos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsetor\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbioinsumos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeneticos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balimentacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\bproagro\\b", x = Coluna_search , ignore.case = TRUE)) |
         (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binovacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprograma\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgarantia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\batividade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bPROAGRO\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcadeia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutiva\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesqueira\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bestrategica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprojetos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcorporativos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bministerio da agricultura, pecuaria e abastecimento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpromocao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagronegocio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbrasileiro\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmercado internacional\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcadeia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutiva\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagropecuaria conservacionista\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprograma\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcredito fundiario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsubvencao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpremio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bseguro rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpolos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        
        (grepl("\\bcentros\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baquicultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpromocao da educacao do campo\\b", x = Coluna_search , ignore.case = TRUE))
        
        
    )
    return(data_frame_crop)
}

# Para Multi-Sector
multisector_search_pattern_SIOP <- function(data_frame_SIOP,Coluna_search){
    data_frame_multisector <- data_frame_SIOP %>% filter(
        (grepl("\\binfraestruturas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bseguranca\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhidrica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\boperacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brede\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhidrometeorologica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\balertas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcheias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binundacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\briscos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesastres\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\balerta\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\beventos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhidrologicos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcriticos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmapeamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeologicogeotecnico\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bconstrucao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcanal\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\badutor\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\benfrentamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprocessos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesertificacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\badaptacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\befeitos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bseca\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\breducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bvulnerabilidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesertificacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmudancas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclimaticas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bciencia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btecnologia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmar\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boceanos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclima\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binformacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balerta\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcheias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binundacoes\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprojetos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmudancas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclimaticas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento cientifico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdifusao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpopularizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsemiarido\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprojetos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmudancas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclimaticas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsatelite\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeoestacionario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balerta\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesastres naturais\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcemaden\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcemaden\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binteroperabilidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomando e controle\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcentro de estudo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmonitoramento brasileiro do clima espacial\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bembrace\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\b(embrace)\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baplicacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bobservacao da terra\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\barticulacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bimplementacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpoliticas publicas ambientais\\b", x = Coluna_search , ignore.case = TRUE))|
        (grepl("\\bprojeto\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\birrigacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        
        
        (grepl("\\blancamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsatelites\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\bsatelites\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcientificos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomposicao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\becossistemas brasileiros\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bantartida\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmar\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\boceanos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclima\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brastreio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcontrole\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsatelites\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprogramas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bestrategicos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiodiversidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeio ambiente\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcentro\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbrasileiro\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprevisao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclima espacial\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bobservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bterra\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bct-amazonia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baeroespacial\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blogistica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bantartica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binformacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeologica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bplano\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentaveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\b(ppcs)\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bppcs\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcontratacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpublicas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentaveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdados de satelites\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\blevantamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhidrogeologicos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\besgotamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsanitario\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomunidades ribeirinhas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmelhoria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bqualidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpraticas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambientalmente\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentaveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpoliticas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpublicas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambientais\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacionais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bzona\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcosteira\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\blevantamentos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeocientificos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bareas urbanas\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\bpolitica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bresiduos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsolidos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binovacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bamazonia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bvaloracao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentabilidade\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brecursos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhumanos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binpe\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brecursos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhumanos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brecursos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhumanos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bchico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmendes\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binstituto\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bamazonia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsistema\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeodesico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbrasileiro\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmar\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bilhas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bareas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bestrategicas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiotecnologia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bareas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bestrategicas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiodiversidade\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bamazonia azul\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binovacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbioeconomia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bestacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomandante ferraz\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\becossistemas amazonicos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmudancas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binovacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binpa\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bestudos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprojetos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bobras\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bamortecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcheias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binundacoes\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\befeitos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesertificacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\babastecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpublico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagua\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bzona\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcosteira\\b", x = Coluna_search , ignore.case = TRUE)) |  
        
        (grepl("\\bprevencao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpreparo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bresposta\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bresposta\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdanos ambientais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bplataforma\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconhecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\badaptacao a mudanca do clima\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\barranjos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbioeconomia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bregulacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfiscalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos hidricos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\birrigacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagua bruta\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bseguranca\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbarragens\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bestudos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprojetos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmudanca do clima\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\biniciativas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bimplementacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpolitica nacional\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmudanca do clima\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\belaboracao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bplanos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprojetos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsaneamento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmelhoria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bqualidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bregulatoria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsetor de saneamento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blogistico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcientifica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bantartica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bconstrucao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binfraestrutura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binstituto nacional do semi-arido\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa, desenvolvimento e supercomputacao para previsao de tempo e clima\\b", x = Coluna_search , ignore.case = TRUE)) | 
        (grepl("\\bpesquisa e desenvolvimento para estudos de tempo, clima, observacao e modelagem do sistema terrestre\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio a sistemas de drenagem urbana sustentavel e de manejo de aguas pluviais em municipios criticos sujeitos a eventos recorrentes de inundacoes, enxurradas e alagamentos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio a implantacao, ampliacao ou melhorias de sistemas de esgotamento sanitario em municipios com populacao superior a 50 mil habitantes ou municipios integrantes de regioes metropolitanas ou de regioes integradas de desenvolvimento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio a empreendimentos de saneamento integrado em municipios com populacao superior a 50 mil habitantes ou municipios integrantes de regioes metropolitanas ou de regioes integradas de desenvolvimento\\b", x = Coluna_search , ignore.case = TRUE))|
        (grepl("\\bapoio a implementacao de acoes de desenvolvimento do setor aguas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bimplantacao, ampliacao, melhoria ou adequacao de sistemas de esgotamento sanitario na area de atuacao da codevasf\\b", x = Coluna_search , ignore.case = TRUE))
        
        
    )
    return(data_frame_multisector)
}

forest_search_pattern_SIOP <- function(data_frame_SIOP,Coluna_search){
    data_frame_forest <- data_frame_SIOP %>% filter(
        (grepl("\\badministracao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bunidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgestao ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprograma\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmanutencao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bministerio do meio ambiente\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcobertura da terra\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bqueimadas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binpe\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmudanca\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclima\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bqueimadas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bincendios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcolecoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bvivas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bjardins\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbotanicos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bjardim\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbotanico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\buso\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiodiversidade\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsistema\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bunidades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnatureza\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfortalecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsnuc\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsnuc\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bSNUC\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpolitica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhidricos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bconservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baquiferos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhidricos\\b", x = Coluna_search , ignore.case = TRUE)) |        
        (grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\buso\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiodiversidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecuperacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestal ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcontrole ambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bciencia e tecnologia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcontrole ambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\badministracao geral\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\blicenciamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpagamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfundacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bindio\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\beducacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsatelites\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bamazonia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcolecoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bvivas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcontrole\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btriagem\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\banimais\\b", x = Coluna_search , ignore.case = TRUE) &grepl("\\bsilvestres\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfaunisticos\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\bconservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos hidricos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprevencao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcombate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bincendios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcontrole\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bqueimadas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bflorestas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprevencao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesmatamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bincendios\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsistema\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiente\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmodelos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brurais\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentaveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdireitos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpovos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bindigenas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bindenizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bquilombolas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprotecao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpovos\\b" , x = Coluna_search , ignore.case = TRUE) & grepl("\\bisolados\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bampliacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconsolidacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsistema nacional de unidades de conservacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfiscalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesmatamento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnormatizacao e fiscalizacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bunidades de conservacao federais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bestrutura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfundiaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\breforma agraria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\borganizacao agraria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bordenamento territorial\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\borganizacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestal\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestal ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdifusao do conhecimento cientifico e tecnologico\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bservico florestal brasileiro\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bSFB\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsfb\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsatelites\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bserie\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bamazonia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brecuperacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecuperacao de areas degradadas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdescomissionamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmineroindustriais\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bradioativo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bareas degradadas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brecuperacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bareas degradadas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bciencia e tecnologia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecuperacao de areas degradadas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecuperacao de areas degradadas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binventario\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestal\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bflora\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbrasileira\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento cientifico\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bjardim botanico do rio de janeiro\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bjbrj\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bjardim botanico\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpreservacao e conservacao ambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binstituto chico mendes de conservacao da biodiversidade\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bespecies\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bregularizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bimoveis\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bunidades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfederacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\badministracao geral\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bregularizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprojetos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\birrigacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\birrigacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpreservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcultural\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpovos indigenas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmanejo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestal\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomunitario\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bconservacao e uso sustentavel da biodiversidade e dos recursos naturais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcadastro\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestal\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgerenciamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfiscalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcadastro rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brecuperacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpreservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbacias hidrograficas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bflorestas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bplantadas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bheveicultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bflorestas plantadas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bheveicultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binclusao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsocioprodutiva\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsocioambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bterritorios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultores\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binformacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bconcessoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpnma\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\buso\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiodiversidade\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpericulosidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcontrole\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bassentamentos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestas alagadas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpolitica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiodiversidade\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bavaliacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\becotoxicologia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bqualidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsipam\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsistema\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprotecao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bamazonia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bregularizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bestrutura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfundiaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgeorreferenciamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdigitalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfundiaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao ambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\betnodesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmanejo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestal\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprevencao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcontrole\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesmatamento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bconcessao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bautorizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blicenciamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bzonas de amortecimento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bavaliacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpericulosidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcontrole\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brecuperacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcobertura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bvegetal\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnativa\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bzoneamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\becologicoeconomico\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\badministracao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bunidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\badministracao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bunidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprevfogo\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bincendios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcombate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bincendios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |

        (grepl("\\bcadastro\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bflorestal\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bexpansao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsilvicultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bnativas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagroflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsociodiversidade\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconflitos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagrarios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpacificacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcampo", x = Coluna_search , ignore.case = TRUE))|
        (grepl("\\bconsolidacao de assentamentos rurais\\b", x = Coluna_search , ignore.case = TRUE))|
        (grepl("\\baquisicao de terras\\b", x = Coluna_search , ignore.case = TRUE))|
        (grepl("\\breforma agraria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bregularizacao fundiaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\baposentadorias e pensoes civis da uniao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcontribuicao da uniao, de suas autarquias e fundacoes para o custeio do regime de previdencia dos servidores publicos federais\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbeneficios obrigatorios aos servidores civis, empregados, militares e seus dependentes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bajuda de custo para moradia ou auxilio-moradia a agentes publicos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bassistencia medica e odontologica aos servidores civis, empregados, militares e seus dependentes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbeneficios e pensoes indenizatorias decorrentes de legislacao especial e/ou decisoes judiciais\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bimplementacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bunidades de conservacao federais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\baposentadorias e pensoes civis da uniao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbeneficios obrigatorios aos servidores civis, empregados, militares e seus dependentes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bajuda de custo para moradia ou auxilio-moradia a agentes publicos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbeneficios e pensoes indenizatorias decorrentes de legislacao especial e/ou decisoes judiciais\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcriacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bimplementacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bunidades de conservacao federais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\badministracao da unidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bministerio do meio ambiente e mudanca do clima\\b", x = Coluna_search , ignore.case = TRUE))
        
        
    )
    return(data_frame_forest)
}
bioenergy_out_SIOP <- function(data_frame_SIOP_bioenergia,Coluna_search){
    data_frame_bioenergy_filter <- data_frame_SIOP_bioenergia %>% filter(
        (!grepl("\\bestudos da industria de petroleo e gas natural\\b", x = Coluna_search , ignore.case = TRUE)))
    return(data_frame_bioenergy_filter)
}

crop_out_SIOP <- function(data_frame_SIOP_crop,Coluna_search){
    data_frame_crop_filter <- data_frame_SIOP_crop %>% filter(
        (!grepl("\\boperacao dos servicos administrativos\\b", x = Coluna_search , ignore.case = TRUE)) &
        (!grepl("\\boperacao dos servicos\\b", x = Coluna_search , ignore.case = TRUE)) &
        (!grepl("\\badministracao da unidade - despesas diversas\\b", x = Coluna_search , ignore.case = TRUE)) &
        (!grepl("\\bativos civis da uniao\\b", x = Coluna_search , ignore.case = TRUE)) &
        (!grepl("\\bassistencia medica e odontologica aos servidores civis, empregados, militares e seus dependentes\\b", x = Coluna_search , ignore.case = TRUE)) &
        (!grepl("\\bauxilio-familiar no exterior\\b", x = Coluna_search , ignore.case = TRUE)) &
        (!grepl("\\bbeneficios obrigatorios aos servidores civis, empregados, militares e seus dependentes\\b", x = Coluna_search , ignore.case = TRUE)) &
        (!grepl("\\bbeneficios e pensoes indenizatorias decorrentes de legislacao especial e/ou decisoes judiciais\\b", x = Coluna_search , ignore.case = TRUE)) &
        (!grepl("\\bsentencas judiciais devidas por empresas estatais\\b", x = Coluna_search , ignore.case = TRUE)) &
        (!grepl("\\bsentencas judiciais transitadas em julgado de pequeno valor\\b", x = Coluna_search , ignore.case = TRUE))
    )
    return(data_frame_crop_filter)
}

multisector_out_SIOP <- function(data_frame_SIOP_multisector,Coluna_search){
    data_frame_multisector_filter <- data_frame_SIOP_multisector %>% filter(

    (!grepl("\\badministracao da unidade\\b", x = Coluna_search , ignore.case = TRUE)) 

    )    
    return(data_frame_multisector_filter)
}
forest_out_SIOP <- function(data_frame_SIOP_forest,Coluna_search){
    data_frame_forest_filter <- data_frame_SIOP_forest %>% filter(

    (!grepl("\\bativos civis da uniao\\b", x = Coluna_search , ignore.case = TRUE)) &
    (!grepl("\\bprecatorios\\b", x = Coluna_search , ignore.case = TRUE))

    )    
    return(data_frame_forest_filter)
}
##############################################################################################################################################################################################################################################################
# Dicionário NINT

# Para bioenergia
bioenergia_search_pattern_NINT <- function(data_frame_NINT,Coluna_search){
    data_frame_bioenergia <- data_frame_NINT %>% filter(
        (grepl("\\bcorn\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bethanol\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bethanol\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbioenergy\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brenovabio\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bindustrialização\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcana-de-açucar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\baumento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\beficiência\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodução\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\betanol\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\busina\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcogeração\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbagaço\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcana-de-açucar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbiocombustíveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprodução\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiocombustível\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\betanol\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcogeração\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\baquisição\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binsumos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodução\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\betanol\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcogeração\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brenovável\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprodução\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiocombustível\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\betanol\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcogeração\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiomassa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcana de açúcar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsolar\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcredit\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsolar\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\benergy\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprojects\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bethanol\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcorn\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsolar\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\beólica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpequenas hidrelétricas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbiogás\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgeração\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bvapor\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcogeração\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\belétrica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbiomassa\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfinanciar\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodução\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\betanol\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprodução\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\betanol\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\braízen\\b", x = Coluna_search , ignore.case = TRUE))|
        (verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211025+Usina+Sonora.pdf")
    )
    return(data_frame_bioenergia)
}
bioenergia_NINT_out <- function(data_frame_NINT){
    data_frame_bioenergia_filter <- data_frame_NINT %>% filter(
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210921+GLP+Brasil.pdf") &
        (!verificador_externo2 %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210921+GLP+Brasil.pdf") &
        (!cbi %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210921+GLP+Brasil.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211020+JF+Citrus.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20231129+Capal.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220114+Sanepar.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220420+BRK.pdf") &
        (!verificador_externo %in% "https://api.mziq.com/mzfilemanager/v2/d/9ffe3afc-e8e3-4e62-9f49-04166095f065/0f113c82-79af-6add-1d24-31ef43427f09?origin=1") &
        (!verificador_externo2 %in% "https://api.mziq.com/mzfilemanager/v2/d/9aa4d8c5-604a-4097-acc9-2d8be8f71593/613063a6-dd33-8d8d-d715-55284ed6297b?origin=1") &
        (!verificador_externo2 %in% "https://api.mziq.com/mzfilemanager/v2/d/9aa4d8c5-604a-4097-acc9-2d8be8f71593/7d2a2b2a-5c4b-dc7e-ec20-984c550c5259?origin=1") &
        (!verificador_externo %in% "https://www.spglobal.com/_assets/documents/ratings/pt/pdf/2023/2023-10-27-debentures-verdes-de-955-milhoes-da-concessionaria-de-saneamento-do-amapa.pdf") &
        (!verificador_externo %in% "https://www.iss-corporate.com/file/documents/spo/spo-20210108-simpar.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210614+S%C3%A3o+Martinho.pdf") &
        (!grepl("\\boperações gerais da empresa\\b", x = project_description,ignore.case = TRUE)) &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210520+Diana+Bioenergia.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210623+S%C3%A3o+Manoel.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210701+Colombo+Agroindustria.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211020+JF+Citrus.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211124++Jalles+Machado.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211025+Usina+Sonora.pdf") &
        (!verificador_externo %in% "https://api.mziq.com/mzfilemanager/v2/d/5b5bf7fa-0e5c-4b44-91cb-707e34a1356a/807f065d-e12a-d9b0-2c45-54c547094632?origin=2")&
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220721+Tobasa.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20221212+Agrogalaxy.pdf") &
        (!verificador_externo2 %in% "https://www.santander.com/content/dam/santander-com/es/documentos/presentaciones-de-renta-fija/2023/06/prf-santander-group-green-social-and-sustainability-funding-global-framework-second-party-opinion-2023.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/opea_Pre%20issuance%20assurance%20statement.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20231129+Capal.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210330+Allonda.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210528+Tereos.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220127+State+Grid.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220406+Allonda.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220713+Usina+S%C3%A3o+Jos%C3%A9.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220726+Irani.pdf")
    )

    return(data_frame_bioenergia_filter)
    
}
# Para Crop NINT
crop_search_pattern_NINT <- function(data_frame_NINT,Coluna_search){
    data_frame_crop <- data_frame_NINT %>% filter(
        (grepl("\\bagricultura\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bprecisão\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bdigital\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bdigital\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bagriculture\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bplantio\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcana-de-açúcar\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\brenovação\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcanavial\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcana de açúcar\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bplantio\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\btratos\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcana de açúcar\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\brenovação\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcanavial\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcana de açúcar\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bcultivo\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcana-de-açúcar\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bbaixo\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcarbono\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bmanutenção\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\brenovação\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bmelhoria\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcanavial\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\brenovação\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcanavial\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bbioinsumos\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bpurchase\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bsoy\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\blow-carbon\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\btechniques\\b",x = Coluna_search,ignore.case = TRUE))
    )
    return(data_frame_crop)

}
crop_NINT_out <- function(data_frame_NINT){
    data_frame_crop_filter <- data_frame_NINT %>% filter(
        (!grepl("\\boperações gerais da empresa\\b", x = project_description,ignore.case = TRUE)) &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20201125+Rio+Amambai.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210128+Colombo+Agroindustria.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210201+Tereos.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210324+Ferrari.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!verificador_externo2 %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210730+Grupo+Balbo.pdf") &
        (!verificador_externo %in% "https://www.climatebonds.net/files/files/FS-Agrisolutions-Industria-de-Biocombustiveis-Ltda_Pre%20issuance%20assurance%20statement.pdf") &
        (!verificador_externo %in% "https://api.mziq.com/mzfilemanager/v2/d/5b5bf7fa-0e5c-4b44-91cb-707e34a1356a/807f065d-e12a-d9b0-2c45-54c547094632?origin=2") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220713+Usina+S%C3%A3o+Jos%C3%A9.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220721+Tobasa.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20230526+Usina+Itamarati.pdf") &
        (!verificador_externo2 %in% "https://www.santander.com/content/dam/santander-com/es/documentos/presentaciones-de-renta-fija/2023/06/prf-santander-group-green-social-and-sustainability-funding-global-framework-second-party-opinion-2023.pdf") &
        (!cbi %in% "https://www.climatebonds.net/files/files/opea_Pre%20issuance%20assurance%20statement.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210305+Fazenda+da+Toca.pdf")&
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211025+Usina+Sonora.pdf")

    )
    return(data_frame_crop_filter)
}
# Para Forest NINT
forest_search_pattern_NINT <- function(data_frame_NINT,Coluna_search){
    data_frame_forest <- data_frame_NINT %>% filter(
        (grepl("\\bmanejo\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bsustentável\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bflorestas certificadas\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bfomento\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bflorestal\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\baquisição\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bflorestas\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bplantadas\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\bsilvicutura\\b",x = Coluna_search,ignore.case = TRUE)) |
        (grepl("\\breflorestamento\\b",x = Coluna_search,ignore.case = TRUE))        
    )
    return(data_frame_forest)
}  
forest_NINT_out <- function(data_frame_NINT){
    data_frame_forest_filter <- data_frame_NINT %>% filter(
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210305+Fazenda+da+Toca.pdf")&
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210614+S%C3%A3o+Martinho.pdf")&
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211020+JF+Citrus.pdf")&
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211124++Jalles+Machado.pdf")&
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220317+Citrosuco.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20221212+Agrogalaxy.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20201125+Rio+Amambai.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210201+Tereos.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210330+Allonda.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210528+Tereos.pdf") &
        (!verificador_externo %in% "https://api.mziq.com/mzfilemanager/v2/d/3c0b3516-7dff-44a5-946f-20e7ec87dfa0/93abd1c0-ab0a-b2c6-f17c-03659badeec1?origin=1") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211001+ComBio.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220127+State+Grid.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220406+Allonda.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220608+ComBio.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220713+Usina+S%C3%A3o+Jos%C3%A9.pdf") &
        (!verificador_externo %in% "https://api.mziq.com/mzfilemanager/v2/d/5b5bf7fa-0e5c-4b44-91cb-707e34a1356a/807f065d-e12a-d9b0-2c45-54c547094632?origin=2")&
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220721+Tobasa.pdf")
    )
    return(data_frame_forest_filter)
}
# Para Cattle NINT
cattle_search_pattern_NINT <- function(data_frame_NINT,Coluna_search){
    data_frame_cattle <- data_frame_NINT %>% filter(
        (grepl("\\baumento\\b",x = Coluna_search, ignore.case = TRUE) & grepl("\\bprodução\\b",x = Coluna_search, ignore.case = TRUE) & grepl("\\bcomercialização\\b",x = Coluna_search, ignore.case = TRUE) & grepl("\\bovos orgânicos\\b",x = Coluna_search, ignore.case = TRUE)) |
        (grepl("\\bbem-estar\\b",x = Coluna_search, ignore.case = TRUE) & grepl("\\banimal\\b",x = Coluna_search, ignore.case = TRUE)) |
        (grepl("\\bprodução\\b",x = Coluna_search, ignore.case = TRUE) & grepl("\\borgânica\\b",x = Coluna_search, ignore.case = TRUE))

    )
    return(data_frame_cattle)
}
cattle_NINT_out <- function(data_frame_NINT){
    data_frame_cattle_filter <- data_frame_NINT %>% filter(
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210520+Diana+Bioenergia.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210614+S%C3%A3o+Martinho.pdf")&
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210528+Tereos.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210623+S%C3%A3o+Manoel.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210701+Colombo+Agroindustria.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211020+JF+Citrus.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211124++Jalles+Machado.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211025+Usina+Sonora.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20221212+Agrogalaxy.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20201125+Rio+Amambai.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210128+Colombo+Agroindustria.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210201+Tereos.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210324+Ferrari.pdf") &
        (!verificador_externo2 %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210730+Grupo+Balbo.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211001+ComBio.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20230526+Usina+Itamarati.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220608+ComBio.pdf")
    )
    return(data_frame_cattle_filter)
}   

# Para MultiSector
multisector_search_pattern_NINT <- function(data_frame_NINT,Coluna_search){
    data_frame_multisector <- data_frame_NINT %>% filter(
        (grepl("\\bsaneamento\\b",x =Coluna_search,ignore.case = TRUE )) |
        (grepl("\\bampliação\\b",x =Coluna_search,ignore.case = TRUE) & grepl("\\bsistemas\\b",x =Coluna_search,ignore.case = TRUE) & grepl("\\babastecimento\\b",x =Coluna_search,ignore.case = TRUE) & grepl("\\bágua\\b",x =Coluna_search,ignore.case = TRUE))|
        (grepl("\\bsanitation\\b",x =Coluna_search,ignore.case = TRUE) & grepl("\\bbrazil\\b",x =Coluna_search,ignore.case = TRUE)) |
        (grepl("\\btratamento\\b",x =Coluna_search,ignore.case = TRUE) & grepl("\\besgoto\\b",x =Coluna_search,ignore.case = TRUE) & grepl("\\bbiodiversidade\\b",x =Coluna_search,ignore.case = TRUE))

    )
    return(data_frame_multisector)
}
multisector_NINT_out <- function(data_frame_NINT){
    data_frame_multisector_filter <- data_frame_NINT %>% filter(
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210305+Fazenda+da+Toca.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210723+Adami.pdf") &
        (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220721+Tobasa.pdf")
    )
}
##############################################################################################################################################################################################################################################################
# Dicionario OCDE
# Para bioenergia
bioenergy_ocde <- function(data_frame_ocde,coluna_search){
    data_frame_bioenergy_ocde <-data_frame_ocde %>% filter(
        (grepl("\\bpolitica\\b", x = coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = coluna_search , ignore.case = TRUE) & grepl("\\bbiocombustiveis\\b", x = coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpolitica\\b", x = coluna_search , ignore.case = TRUE) & grepl("\\bnacional\\b", x = coluna_search , ignore.case = TRUE))

    )
    return(data_frame_bioenergy_ocde)
}