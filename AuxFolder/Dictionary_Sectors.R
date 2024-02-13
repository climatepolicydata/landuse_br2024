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
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpromocao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\abastecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomercializacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bassistencia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutor\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("agricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\breforma\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bampliacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blaboratorios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarios\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdefesa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdivulgacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binformacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologicas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclimatologicas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babastecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomercializacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bministerio da agricultura e pecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpromocao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brisco\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\borganizacao agraria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfortalecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdinamizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento agrario\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdigitalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bacervo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhistorico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologicos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btecnologia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprotecao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcultivares\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\buso sustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos geneticos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binovacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
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
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcarbono\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpreservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfiscalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarias\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfiscalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfertilizantes\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("defesa agropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |        
        (grepl("\\bagroecologia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsociobiodiversidade\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\borganica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcontrole\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\borganica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpro-organico\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsetor\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarios\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpromocao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricolas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroambientais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bzoneamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivo\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroecossistemas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsistemas produtivos rurais sustentaveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bp&d\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcompetitividade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentabilidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpd\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcompetitividade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentabilidade\\b" , x = Coluna_search , ignore.case = TRUE) & grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesqueira\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baquicola\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\barranjos produtivos locais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcacau\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprodutoras de cacau\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsistemas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagroflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcacaueiras\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbrasil\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmiseria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafeicultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprodutores\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagronegocio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafe\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfundo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdefesa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafeeira\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprodutoras\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcacau\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
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
        (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconservacionista\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binterpretacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binformacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsolos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeneticos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsetor\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbioinsumos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeneticos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balimentacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbioeconomia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproagro\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprograma\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgarantia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\batividade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bPROAGRO\\b", x = Coluna_search , ignore.case = TRUE))
    )
    return(data_frame_crop)
}
