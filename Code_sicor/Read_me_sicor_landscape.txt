


O SICOR é um registro de todas as operações de crédito rural e Proagro (Programa de Garantia de Atividade Agropecuária)
no Brasil. As bases principais são chamadas “SICOR_OPERACAO_BASICA_ESTADO_XXXX”, onde XXXX é o ano. Essas bases
contém o indexador da operação (ref_bacen e nu_ordem) e algumas informações básicas, incluindo o valor da operação.
Para encontrar informações adicionais como nome do Programa, Cultura, Atividade, etc, é preciso conectar a base
principal com as tabelas de domínio auxiliares, disponibilizadas na subpasta rawData/auxiliary, pois se trata de uma
base de dados relacional. É possível obter informações adicionais sobre os contratos de Proagro (uma espécie de
seguro para a operação de crédito), incluindo as indenizações e os eventos que geraram sinistro. Para mais
informações, consulte o manualDadosSicor_V2 na subpasta _documentation.


04_sicor_op_basica_transform_landscape.R 

1 - 
	Começamos com a importação das tabelas relacionais criadas e com a tabela de descrição de cada código de variável do datase original.
	Relacionamos as descrições com seus respectivos códigos e a aplicação das classificações das tabelas relacionais com o dataset origina.

2 - 
	Em seguida criamos as duas variáveis descritivas do formato landscape "project_name e project_description"
	Convertemos observações "NA" em valor 0 para evitar erro de contabilização na agregação dos valores.
	Criação do id único para cada observação a fim de utiliza-lo para a agregação de descrições iguais posteriormente.

3-	
	Para inclusão do nome da instituição financeira (NOME_IF) observamos a participação das 10 maiores instituições no total dos dados e destacamos os nomes dessas 10.
	O restante das instituições foram classificadas como "outros".

4-
	Após a classificação de uso climático agregamos as observações que tem descrições iguais a fim de diminuir a quantidade de observações no dataset, mas manter o valor total analisado
	Alteramos os nmes de acordo com a tabela de padronização do landscape.
	Aplicamos a combinação de palavras chaves para classificação dos setores landscape em 3 variáveis (sector_original, project_name, project_description)
	Por fim realocamos as colunas e aplicamos ajustes finais para fonte de financiamento a fim de atualizar algumas classificações para "LCA e FUNCAFE"


Obs:
Validação dados selecionados (perda ou não de observações ou valores)
	A validação dos dados, para valores e perda de observações está no final do código, porém está comentada para não execução quando executamos o código no masterfile de cada base.

	