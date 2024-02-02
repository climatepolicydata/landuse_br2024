03_sicor_op_basica_filter_dummies

Para seleção das operações de crédito rural alinhadas a objetivos climáticos foram utilizados como critérios (1) + (2): 

Critérios de sustentabilidade aplicáveis na concessão de crédito rural indicados no anexo (tabela na página 6 a 9) da Consulta pública nº 82 de 2021 do Banco Central do Brasil (BCB). 

Tabela com as categorias e códigos dos campos do Sicor selecionados disponíveis no arquivo de Excel:(códigos_consulta_pública_82_bcb.xls). 

Todas as linhas do Programa para Redução da Emissão de Gases de Efeito Estufa na Agricultura (Programa ABC+), 
Programa Nacional de Fortalecimento da Agricultura Familiar no âmbito do ABC (Pronaf ABC+) e Fundo Constitucional de Financiamento do Norte no âmbito do ABC (FNO-ABC), ainda que não especificadas na Consulta Pública nº82. 

-----------------------------------------------------------------------------------------------------------------------------------

Utilizamos a criação de dummies para a identificação e melhor manipulação das condições para a filtragem desejada no código.

Foram criadas portanto 10 dummies para alinhamento com a tabela relacional criada e as informações da consulta pública 82:

	A filtragem posterior é seguida pela criação de uma variável "sum_dummy" onde somamos os valores de todas as dummies anteriores (exclusivamente a dummy de programa).
	Caso a nova variável possua pelo menos uma dummy entre todas igual a 1, a observação entrará no filtro do novo data frame.
	Nessa análise também não incluímos as observações com finalidade = "comercialização".


Obs:
Validação dados selecionados (perda ou não de observações ou valores)
	A validação dos dados, para valores e perda de observações está no final do código, porém está comentada para não execução quando executamos o código no masterfile de cada base.

	
	