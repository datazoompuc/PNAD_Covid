******************************************************
*             datazoom_pnad_covid.ado              *
******************************************************
* version 1.0

program define datazoom_pnad_covid

syntax, months(numlist) original(str) saving(str) 

/* Pastas para guardar arquivos da sessão */
cd `"`saving'"'

/* Criação de pastas para salvar os arquivos */
capture mkdir pnad_covid
if _rc == 693 {
   tempname numpasta
   local numpasta = 0
   while _rc == 693 {
      capture mkdir "pnad_covid_`++numpasta'"
   }
   cd "pnad_covid_`numpasta'"
}
else {
   cd "pnad_covid"
}
loc caminhoprin = c(pwd)

/* Dicionário */
findfile pnad_covid.dct
loc dic = r(fn)

/* Extração dos arquivos */

* separar o ado em duas partes
foreach month in `months'{
	di as input "Extraindo arquivo PNAD_COVID_0`month'2020  ..."
	*cap infile using "`dic'", using("`original'/PNAD_COVID_`month'.csv") clear	
	import delimited using "`original'/PNAD_COVID_0`month'2020.csv", clear	
	if _rc == 0 {
	// normal labels
	cap label var Ano			"Ano de referência	"
	cap label var UF			"Unidade da Federação	"
	cap label var CAPITAL		"Capital	"
	cap label var RM_RIDE		"Região Metropolitana e Região Administrativa Integrada de Desenvolvimento"
	cap label var V1008		"Número de seleção do domicílio	"
	cap label var V1012		"Semana no mês	"
	cap label var V1013		"Mês da pesquisa	"
	cap label var V1016		"Número da entrevista no domicílio	"
	cap label var Estrato		"Estrato	"
	cap label var UPA			"UPA	"
	cap label var V1022		"Situação do domicílio	"
	cap label var V1023		"Tipo de área	"
	cap label var V1030		"Projeção da população	"
	cap label var V1031			"Peso do domicílio e das pessoas	"
	cap label var V1032			"Peso do domicílio e das pessoas	"
	cap label var posest		"Domínios de projeção	"
	cap label var A001		"Número de ordem	"
	cap label var A001A		"Condição no domicílio	"
	cap label var A001B1		"Dia de nascimento	"
	cap label var A001B2		"Mês de nascimento	"
	cap label var A001B3		"Ano de nascimento	"
	cap label var A002		"Idade do morador 	"
	cap label var A003		"Sexo	"
	cap label var A004		"Cor ou raça	"
	cap label var A005		"Escolaridade	"
	cap label var B0011		"Nos últimos sete dias teve febre?	"
	cap label var B0012		"Nos últimos sete dias teve tosse?	"
	cap label var B0013		"Nos últimos sete dias teve dor de garganta?	"
	cap label var B0014		"Nos últimos sete dias teve dificuldade para respirar?	"
	cap label var B0015		"Nos últimos sete dias teve dor de cabeça?	"
	cap label var B0016		"Nos últimos sete dias teve dor no peito?	"
	cap label var B0017		"Nos últimos sete dias teve nausea?	"
	cap label var B0018		"Nos últimos sete dias teve nariz entupido ou escorrendo?	"
	cap label var B0019		"Nos últimos sete dias teve fadiga?	"
	cap label var B00110		"Nos últimos sete dias teve dor nos olhos?	"
	cap label var B00111		"Nos últimos sete dias teve perda de cheiro ou sabor?	"
	cap label var B00112		"Nos últimos sete dias teve dor muscular?	"
	cap label var B002		"Por causa disso, foi a algum estabelecimento de saude?	"
	cap label var B0031		"Providência tomada para recuperar dos sintomas foi ficar em casa	"
	cap label var B0032		"Providência tomada para recuperar dos sintomas foi ligar para algum profissional de saúde	"
	cap label var B0033		"Providência tomada  para recuperar dos sintomas foi comprar e/ou tomar  remédio por conta própria	"
	cap label var B0034		"Providência tomada para recuperar dos sintomas foi comprar e/ou tomar remédio por orientação médica	"
	cap label var B0035		"Providência tomada para recuperar dos sintomas foi receber visita de algum profissional de saúde do SUS (equipe de saúde da família, agente comunitário, etc.)	"
	cap label var B0036		"Providência tomada para recuperar dos sintomas foi receber visita de profissional de saúde particular 	"
	cap label var B0037		"Providência tomada para recuperar dos sintomas foi outra	"
	cap label var B0041		"Local que buscou atendimento foi posto de saúde/Unidade básica de saúde /Equipe de Saúde da Família (médico, enfermeiro, técnico de enfermagem ou agente comunitário de saúde)	"
	cap label var B0042		"Local que buscou atendimento foi pronto socorro do SUS/UPA	"
	cap label var B0043		"Local que buscou atendimento foi hospital do SUS	"
	cap label var B0044		"Local que buscou atendimento foi ambulatório ou consultório privado ou ligado às forças armadas	"
	cap label var B0045		"Local que buscou atendimento foi pronto socorro privado ou ligado às forças armadas	"
	cap label var B0046		"Local que buscou atendimento foi hospital privado ou ligado às forças armadas	"
	cap label var B005		"Ao procurar o hospital, teve que ficar internado por um dia ou mais	"
	cap label var B006		"Durante a internação, foisedado, entubado e colocado em respiração artificial com ventilador	"
	cap label var B007		"Tem algum plano de saúde médico, seja particular, de empresa ou de órgão público	"
	cap label var C001		"Na semana passada, por pelo menos uma hora, trabalhou ou fez algum bico?	"
	cap label var C002		"Na semana passada, estava temporariamente afastado de algum trabalho?	"
	cap label var C003		"Qual o principal motivo deste afastamento temporário?	"
	cap label var C004		"Continuou a ser remunerado (mesmo que parcialmente) por esse trabalho	"
	cap label var C005		"Há quanto tempo está afastado desse trabalho?	"
	cap label var C0051		" tempo que estava afastado (De 1 mês a menos de 1 ano) 	"
	cap label var C0052		"tempo que estava afastado (De 1 ano a menos de 2 anos) 	"
	cap label var C0053		"tempo que estava afastado (de 02 anos a 98 anos) 	"
	cap label var C006		"Tem mais de um trabalho	"
	cap label var C007		"No trabalho (único ou principal) que tinha nessa semana, era:	"
	cap label var C007A		"Esse trabalho era na área:	"
	cap label var C007B		"Tem carteria de trabalho assinada ou é funcionário público estatutário?	"
	cap label var C007C		"Que tipo de trabalho, cargo ou função você realiza no seu trabalho (único ou principal)? 	"
	cap label var C007D		"Qual é a principal atividade do local ou empresa em que você trabalha? 	"
	cap label var C007E		"Na semana passada, quantos empregados trabalhavam nesse negócio/empresa que ... tinha ?	"
	cap label var C007E1		"1 a 5 empregados	"
	cap label var C007E2		"6 a 10 empregados	"
	cap label var C008		"Quantas horas, por semana, normalmente trabalhava?	"
	cap label var C009		"Quantas horas, na semana passada, de fato trabalhou?	"
	cap label var C010		"Quanto recebia (ou retirava) normalmente em todos os seus trabalhos	"
	cap label var C0101		"Recebia/retirava normalmente em dinheiro	"
	cap label var C01011		"Número da faixa do rendimento/retirada em dinheiro	"
	cap label var C01012		"Valor em dinheiro	"
	cap label var C0102		"Recebia normalmente em produtos e mercadorias	"
	cap label var C01021		"Número da faixa do rendimento/retirada em produtos e mercadorias	"
	cap label var C01022		"Valor em produtos e mercadorias	"
	cap label var C0103		"Recebia normalmente somente em benefícios	"
	cap label var C0104		"Era não remunerado	"
	cap label var C011A		"Quanto recebia (ou retirava) normalmente em todos os seus trabalhos	"
	cap label var C011A1		"Recebia/retirava normalmente em dinheiro	"
	cap label var C011A11		"Número da faixa do rendimento/retirada em dinheiro	"
	cap label var C011A12		"Valor em dinheiro	"
	cap label var C011A2		"Recebia normalmente em produtos e mercadorias	"
	cap label var C011A21		"Número da faixa do rendimento/retirada em produtos e mercadorias	"
	cap label var C011A22		"Valor em produtos e mercadorias	"
	cap label var C012		"Na maior parte do tempo, na semana passada, esse trabalho (único ou principal) foi exercido no mesmo local em que costuma trabalhar? 	"
	cap label var C013		"Na semana passada, o(a) Sr(a) estava em trabalho remoto (home office ou teletrabalho)?	"
	cap label var C014		"O(A) Sr(a) contribui para o INSS?  	"
	cap label var C015		"Na semana passada ___ tomou alguma providência efetiva para conseguir trabalho?  	"
	cap label var C016		"Qual o principal motivo de não ter procurado trabalho na semana passada? 	"
	cap label var C017A		"Algum outro morador deste domicílio trabalhou de forma remunerada na semana passada?	"
	cap label var D0011		"Rendimento recebido de aposentadoria e pensão por todos os moradores	"
	cap label var D0013		"Somatório dos valores recebidos	"
	cap label var D0021		"Rendimento de pensão alimentícia, doação ou mesada em dinheiro de pessoa que não morava no domicílio	"
	cap label var D0023		"Somatório dos valores recebidos	"
	cap label var D0031		"Rendimentos de Programa Bolsa Família	"
	cap label var D0033		"Somatório dos valores recebidos	"
	cap label var D0041		" No mês de ... (mês de referência), ... recebeu rendimentos de Benefício Assistencial de Prestação Continuada – BPC-LOAS?	"
	cap label var D0043		"Somatório dos valores recebidos	"
	cap label var D0051		"Auxilios emergenciais relacionados ao coronavirus	"
	cap label var D0053		"Somatório dos valores recebidos	"
	cap label var D0061		"Seguro desemprego	"
	cap label var D0063		"Somatório dos valores recebidos	"
	cap label var D0071		"Outros rendimentos, como aluguel, arrendamento, pervidencia privada, bolsa de estudos, rendimentos de aplicação financeira etc.	"
	cap label var D0073		"Somatório dos valores recebidos	"
	cap label var F001		"Este domicílio é: 	"
	cap label var F0021		"Qual foi o valor mensal do aluguel pago, ou que deveria ter sido pago, no mês de referência?	"
	cap label var F0022		"Número da faixa do aluguel pago	"
	cap label var F0061		"Quem respondeu ao questionário?	"
	cap label var F006		"Número de ordem do morador que prestou as label var"
	//
		cap label var ano			"Ano de referência	"
	cap label var uf			"Unidade da Federação	"
	cap label var capital		"Capital	"
	cap label var rm_ride		"Região Metropolitana e Região administrativa Integrada de desenvolvimento"
	cap label var v1008		"Número de seleção do domicílio	"
	cap label var v1012		"Semana no mês	"
	cap label var v1013		"Mês da pesquisa	"
	cap label var v1016		"Número da entrevista no domicílio	"
	cap label var estrato		"Estrato	"
	cap label var upa			"UPA	"
	cap label var v1022		"Situação do domicílio	"
	cap label var v1023		"Tipo de área	"
	cap label var v1030		"Projeção da população	"
	cap label var v1031			"Peso do domicílio e das pessoas	"
	cap label var v1032			"Peso do domicílio e das pessoas	"
	cap label var posest		"Domínios de projeção	"
	cap label var a001		"Número de ordem	"
	cap label var a001a		"Condição no domicílio	"
	cap label var a001b1		"dia de nascimento	"
	cap label var a001b2		"Mês de nascimento	"
	cap label var a001b3		"Ano de nascimento	"
	cap label var a002		"Idade do morador 	"
	cap label var a003		"Sexo	"
	cap label var a004		"Cor ou raça	"
	cap label var a005		"Escolaridade	"
	cap label var b0011		"Nos últimos sete dias teve febre?	"
	cap label var b0012		"Nos últimos sete dias teve tosse?	"
	cap label var b0013		"Nos últimos sete dias teve dor de garganta?	"
	cap label var b0014		"Nos últimos sete dias teve dificuldade para respirar?	"
	cap label var b0015		"Nos últimos sete dias teve dor de cabeça?	"
	cap label var b0016		"Nos últimos sete dias teve dor no peito?	"
	cap label var b0017		"Nos últimos sete dias teve nausea?	"
	cap label var b0018		"Nos últimos sete dias teve nariz entupido ou escorrendo?	"
	cap label var b0019		"Nos últimos sete dias teve fadiga?	"
	cap label var b00110		"Nos últimos sete dias teve dor nos olhos?	"
	cap label var b00111		"Nos últimos sete dias teve perda de cheiro ou sabor?	"
	cap label var b00112		"Nos últimos sete dias teve dor muscular?	"
	cap label var b002		"Por causa disso, foi a algum estabelecimento de saude?	"
	cap label var b0031		"Providência tomada para recuperar dos sintomas foi ficar em casa	"
	cap label var b0032		"Providência tomada para recuperar dos sintomas foi ligar para algum profissional de saúde	"
	cap label var b0033		"Providência tomada  para recuperar dos sintomas foi comprar e/ou tomar  remédio por conta própria	"
	cap label var b0034		"Providência tomada para recuperar dos sintomas foi comprar e/ou tomar remédio por orientação médica	"
	cap label var b0035		"Providência tomada para recuperar dos sintomas foi receber visita de algum profissional de saúde do SUS (equipe de saúde da família, agente comunitário, etc.)	"
	cap label var b0036		"Providência tomada para recuperar dos sintomas foi receber visita de profissional de saúde particular 	"
	cap label var b0037		"Providência tomada para recuperar dos sintomas foi outra	"
	cap label var b0041		"Local que buscou atendimento foi posto de saúde/Unidade básica de saúde /equipe de Saúde da Família (médico, enfermeiro, técnico de enfermagem ou agente comunitário de saúde)	"
	cap label var b0042		"Local que buscou atendimento foi pronto socorro do SUS/UPa	"
	cap label var b0043		"Local que buscou atendimento foi hospital do SUS	"
	cap label var b0044		"Local que buscou atendimento foi ambulatório ou consultório privado ou ligado às forças armadas	"
	cap label var b0045		"Local que buscou atendimento foi pronto socorro privado ou ligado às forças armadas	"
	cap label var b0046		"Local que buscou atendimento foi hospital privado ou ligado às forças armadas	"
	cap label var b005		"Ao procurar o hospital, teve que ficar internado por um dia ou mais	"
	cap label var b006		"durante a internação, foisedado, entubado e colocado em respiração artificial com ventilador	"
	cap label var b007		"Tem algum plano de saúde médico, seja particular, de empresa ou de órgão público	"
	cap label var c001		"Na semana passada, por pelo menos uma hora, trabalhou ou fez algum bico?	"
	cap label var c002		"Na semana passada, estava temporariamente afastado de algum trabalho?	"
	cap label var c003		"Qual o principal motivo deste afastamento temporário?	"
	cap label var c004		"Continuou a ser remunerado (mesmo que parcialmente) por esse trabalho	"
	cap label var c005		"Há quanto tempo está afastado desse trabalho?	"
	cap label var c0051		" tempo que estava afastado (de 1 mês a menos de 1 ano) 	"
	cap label var c0052		"tempo que estava afastado (de 1 ano a menos de 2 anos) 	"
	cap label var c0053		"tempo que estava afastado (de 02 anos a 98 anos) 	"
	cap label var c006		"Tem mais de um trabalho	"
	cap label var c007		"No trabalho (único ou principal) que tinha nessa semana, era:	"
	cap label var c007a		"Esse trabalho era na área:	"
	cap label var c007b		"Tem carteria de trabalho assinada ou é funcionário público estatutário?	"
	cap label var c007c		"Que tipo de trabalho, cargo ou função você realiza no seu trabalho (único ou principal)? 	"
	cap label var c007d		"Qual é a principal atividade do local ou empresa em que você trabalha? 	"
	cap label var c007e		"Na semana passada, quantos empregados trabalhavam nesse negócio/empresa que ... tinha ?	"
	cap label var c007e1		"1 a 5 empregados	"
	cap label var c007e2		"6 a 10 empregados	"
	cap label var c008		"Quantas horas, por semana, normalmente trabalhava?	"
	cap label var c009		"Quantas horas, na semana passada, de fato trabalhou?	"
	cap label var c010		"Quanto recebia (ou retirava) normalmente em todos os seus trabalhos	"
	cap label var c0101		"Recebia/retirava normalmente em dinheiro	"
	cap label var c01011		"Número da faixa do rendimento/retirada em dinheiro	"
	cap label var c01012		"Valor em dinheiro	"
	cap label var c0102		"Recebia normalmente em produtos e mercadorias	"
	cap label var c01021		"Número da faixa do rendimento/retirada em produtos e mercadorias	"
	cap label var c01022		"Valor em produtos e mercadorias	"
	cap label var c0103		"Recebia normalmente somente em benefícios	"
	cap label var c0104		"Era não remunerado	"
	cap label var c011a		"Quanto recebia (ou retirava) normalmente em todos os seus trabalhos	"
	cap label var c011a1		"Recebia/retirava normalmente em dinheiro	"
	cap label var c011a11		"Número da faixa do rendimento/retirada em dinheiro	"
	cap label var c011a12		"Valor em dinheiro	"
	cap label var c011a2		"Recebia normalmente em produtos e mercadorias	"
	cap label var c011a21		"Número da faixa do rendimento/retirada em produtos e mercadorias	"
	cap label var c011a22		"Valor em produtos e mercadorias	"
	cap label var c012		"Na maior parte do tempo, na semana passada, esse trabalho (único ou principal) foi exercido no mesmo local em que costuma trabalhar? 	"
	cap label var c013		"Na semana passada, o(a) Sr(a) estava em trabalho remoto (home office ou teletrabalho)?	"
	cap label var c014		"O(a) Sr(a) contribui para o INSS?  	"
	cap label var c015		"Na semana passada ___ tomou alguma providência efetiva para conseguir trabalho?  	"
	cap label var c016		"Qual o principal motivo de não ter procurado trabalho na semana passada? 	"
	cap label var c017a		"Algum outro morador deste domicílio trabalhou de forma remunerada na semana passada?	"
	cap label var d0011		"Rendimento recebido de aposentadoria e pensão por todos os moradores	"
	cap label var d0013		"Somatório dos valores recebidos	"
	cap label var d0021		"Rendimento de pensão alimentícia, doação ou mesada em dinheiro de pessoa que não morava no domicílio	"
	cap label var d0023		"Somatório dos valores recebidos	"
	cap label var d0031		"Rendimentos de Programa bolsa Família	"
	cap label var d0033		"Somatório dos valores recebidos	"
	cap label var d0041		" No mês de ... (mês de referência), ... recebeu rendimentos de benefício assistencial de Prestação continuada – bPc-LOaS?	"
	cap label var d0043		"Somatório dos valores recebidos	"
	cap label var d0051		"Auxilios emergenciais relacionados ao coronavirus	"
	cap label var d0053		"Somatório dos valores recebidos	"
	cap label var d0061		"Seguro desemprego	"
	cap label var d0063		"Somatório dos valores recebidos	"
	cap label var d0071		"Outros rendimentos, como aluguel, arrendamento, pervidencia privada, bolsa de estudos, rendimentos de aplicação financeira etc.	"
	cap label var d0073		"Somatório dos valores recebidos	"
	cap label var f001		"Este domicílio é: 	"
	cap label var f0021		"Qual foi o valor mensal do aluguel pago, ou que deveria ter sido pago, no mês de referência?	"
	cap label var f0022		"Número da faixa do aluguel pago	"
	cap label var f0061		"Quem respondeu ao questionário?	"
	cap label var f006		"Número de ordem do morador que prestou as label var"
	save PNAD_COVID_0`month'2020, replace
	}
	else continue, break
}


di _newline "Esta versão do pacote datazoom_pnad_covid é compatível com a última versão dos microdados da PNAD Covid divulgados em 24/06/2020"
di _newline "As bases de dados foram salvas em `c(pwd)'"
end
		