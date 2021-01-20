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
*findfile pnad_covid.dct
*loc dic = r(fn)

/* Extração dos arquivos */

* separar o ado em duas partes
foreach month in `months'{

	if `month' < 10 {
	di as input "Extraindo arquivo PNAD_COVID_0`month'2020  ..."
	*cap infile using "`dic'", using("`original'/PNAD_COVID_`month'.csv") clear	
	import delimited using "`original'/PNAD_COVID_0`month'2020.csv", clear
	}
	
	if `month' >= 10 {
	di as input "Extraindo arquivo PNAD_COVID_`month'2020  ..."
	*cap infile using "`dic'", using("`original'/PNAD_COVID_`month'.csv") clear	
	import delimited using "`original'/PNAD_COVID_`month'2020.csv", clear
	}

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
	cap label var A006		"Frequenta escola	"
	cap label var A006A		"A escola / escola ou faculdade que frequenta é pública ou privada?	"
	cap label var A006B		"Você está tendo aulas presenciais?	"
	cap label var A007		"Na semana passada foram disponibilizadas atividades escolares para realizar em casa?	"
	cap label var A007A		"Por que não realizou as atividades disponibilizadas na semana passada?	"
	cap label var A008		"Na semana passada, em quantos dias dedicou-se às atividades escolares? 	"
	cap label var A009		"Na semana passada, quanto tempo por dia gastou fazendo as atividades escolares? 	"
	cap label var B0011		"Na semana passada teve febre?	"
	cap label var B0012		"Na semana passada teve tosse?	"
	cap label var B0013		"Na semana passada teve dor de garganta?	"
	cap label var B0014		"Na semana passada teve dificuldade para respirar?	"
	cap label var B0015		"Na semana passada teve dor de cabeça?	"
	cap label var B0016		"Na semana passada teve dor no peito?	"
	cap label var B0017		"Na semana passada teve nausea?	"
	cap label var B0018		"Na semana passada teve nariz entupido ou escorrendo?	"
	cap label var B0019		"Na semana passada teve fadiga?	"
	cap label var B00110		"Na semana passada teve dor nos olhos?	"
	cap label var B00111		"Na semana passada teve perda de cheiro ou sabor?	"
	cap label var B00112		"Na semana passada teve dor muscular?	"
	cap label var B00113		"Na semana passada teve diarreia?	"
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
	cap label var B008		"O(A) Sr(a) fez algum teste para saber se estava infectado(a) pelo coronavírus?	"
	cap label var B009A		"Fez o exame coletado com cotonete na boca e/ou nariz (SWAB)?	"
	cap label var B009B		"Qual o resultado?	"
	cap label var B009C		"Fez o exame de coleta de sangue através de furo no dedo? "
	cap label var B009D		"Qual o resultado?	"
	cap label var B009E		"Fez o exame de coleta de sangue através da veia da braço?	"
	cap label var B009F		"Qual o resultado? "
	cap label var B0101		"Algum médico já lhe deu o diagnóstico de diabetes?	"
	cap label var B0102		"Algum médico já lhe deu o diagnóstico de hipertensão?	"
	cap label var B0103		"Algum médico já lhe deu o diagnóstico de asma/bronquite/enfisema/doenças respiratória crônica ou doença de pulmão?	"
	cap label var B0104		"Algum médico já lhe deu o diagnóstico de doenças do coração (infarto, angina, insuficiência cardiáca, arritima)?	"
	cap label var B0105		"Algum médico já lhe deu o diagnóstico de depressão?	"
	cap label var B0106		"Algum médico já lhe deu o diagnóstico de câncer?	"
	cap label var B011		"Qual foi o resultado do teste?  Na semana passada, devido à pandemia do Coronavírus, em que medida o(a) Sr(a) restringiu o contato com as pessoas? 	"
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
	cap label var C007F		"No trabalho (único ou principal) que tinha nesa semana, o(a) Sr(a) estava com o contrato de trabalho suspenso?	"
	cap label var C008		"Quantas horas, por semana, normalmente trabalhava?	"
	cap label var C009		"Quantas horas, na semana passada, de fato trabalhou?	"
	cap label var C009A		"Na semana passada, o(a) Sr(a) gostaria de ter trabalhado mais horas do que as de fato trabalhadas?	"
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
	cap label var E001		"Durante o período da pandemia alguém deste domicílio solicitou algum empréstimo?	"
	cap label var E0021		"Este empréstimo foi adquirido com banco ou financeira	"
	cap label var E0022		"Este empréstimo foi adquirido com parente ou amigo	"
	cap label var E0023		"Este empréstimo foi adquirido com empregados ou patrão	"
	cap label var E0024		"Este empréstimo foi adquirido com outro local ou pessoa 	"
	cap label var F001		"Este domicílio é: 	"
	cap label var F0021		"Qual foi o valor mensal do aluguel pago, ou que deveria ter sido pago, no mês de referência?	"
	cap label var F0022		"Número da faixa do aluguel pago	"
	cap label var F002A1		"No seu domicílio há os seguintes itens básicos de limepeza e proteção: sabão ou detergente	"
	cap label var F002A2		"No seu domicílio há os seguintes itens básicos de limepeza e proteção: álcool 70% ou superior (gel ou líquido)	"
	cap label var F002A3		"No seu domicílio há os seguintes itens básicos de limepeza e proteção: máscaras	"
	cap label var F002A4		"No seu domicílio há os seguintes itens básicos de limepeza e proteção: luvas descartáveis	"
	cap label var F002A5		"No seu domicílio há os seguintes itens básicos de limepeza e proteção: água sanitária ou desinfetante	"
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
	cap label var a006		"Frequenta escola	"
	cap label var a006a		"A escola / escola ou faculdade que frequenta é pública ou privada?	"
	cap label var a006b		"Você está tendo aulas presenciais?	"
	cap label var a007		"Na semana passada foram disponibilizadas atividades escolares para realizar em casa?	"
	cap label var a007a		"Por que não realizou as atividades disponibilizadas na semana passada?	"
	cap label var a008		"Na semana passada, em quantos dias dedicou-se às atividades escolares? 	"
	cap label var a009		"Na semana passada, quanto tempo por dia gastou fazendo as atividades escolares? 	"
	cap label var b0011		"Na semana passada teve febre?	"
	cap label var b0012		"Na semana passada teve tosse?	"
	cap label var b0013		"Na semana passada teve dor de garganta?	"
	cap label var b0014		"Na semana passada teve dificuldade para respirar?	"
	cap label var b0015		"Na semana passada teve dor de cabeça?	"
	cap label var b0016		"Na semana passada teve dor no peito?	"
	cap label var b0017		"Na semana passada teve nausea?	"
	cap label var b0018		"Na semana passada teve nariz entupido ou escorrendo?	"
	cap label var b0019		"Na semana passada teve fadiga?	"
	cap label var b00110		"Na semana passada teve dor nos olhos?	"
	cap label var b00111		"Na semana passada teve perda de cheiro ou sabor?	"
	cap label var b00112		"Na semana passada teve dor muscular?	"
	cap label var b00113		"Na semana passada teve diarreia?	"
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
	cap label var b008		"O(A) Sr(a) fez algum teste para saber se estava infectado(a) pelo coronavírus?	"
	cap label var b009a		"Fez o exame coletado com cotonete na boca e/ou nariz (SWAB)?	"
	cap label var b009b		"Qual o resultado?	"
	cap label var b009c		"Fez o exame de coleta de sangue através de furo no dedo? "
	cap label var b009d		"Qual o resultado?	"
	cap label var b009e		"Fez o exame de coleta de sangue através da veia da braço?	"
	cap label var b009f		"Qual o resultado? "
	cap label var b0101		"Algum médico já lhe deu o diagnóstico de diabetes?	"
	cap label var b0102		"Algum médico já lhe deu o diagnóstico de hipertensão?	"
	cap label var b0103		"Algum médico já lhe deu o diagnóstico de asma/bronquite/enfisema/doenças respiratória crônica ou doença de pulmão?	"
	cap label var b0104		"Algum médico já lhe deu o diagnóstico de doenças do coração (infarto, angina, insuficiência cardiáca, arritima)?	"
	cap label var b0105		"Algum médico já lhe deu o diagnóstico de depressão?	"
	cap label var b0106		"Algum médico já lhe deu o diagnóstico de câncer?	"
	cap label var b011		"Qual foi o resultado do teste?  Na semana passada, devido à pandemia do Coronavírus, em que medida o(a) Sr(a) restringiu o contato com as pessoas? 	"
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
	cap label var c007f		"No trabalho (único ou principal) que tinha nesa semana, o(a) Sr(a) estava com o contrato de trabalho suspenso?	"
	cap label var c008		"Quantas horas, por semana, normalmente trabalhava?	"
	cap label var c009		"Quantas horas, na semana passada, de fato trabalhou?	"
	cap label var c009a		"Na semana passada, o(a) Sr(a) gostaria de ter trabalhado mais horas do que as de fato trabalhadas?	"
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
	cap label var e001		"Durante o período da pandemia alguém deste domicílio solicitou algum empréstimo?	"
	cap label var e0021		"Este empréstimo foi adquirido com banco ou financeira	"
	cap label var e0022		"Este empréstimo foi adquirido com parente ou amigo	"
	cap label var e0023		"Este empréstimo foi adquirido com empregados ou patrão	"
	cap label var e0024		"Este empréstimo foi adquirido com outro local ou pessoa 	"
	cap label var f001		"Este domicílio é: 	"
	cap label var f0021		"Qual foi o valor mensal do aluguel pago, ou que deveria ter sido pago, no mês de referência?	"
	cap label var f0022		"Número da faixa do aluguel pago	"
	cap label var f002a1		"No seu domicílio há os seguintes itens básicos de limepeza e proteção: sabão ou detergente	"
	cap label var f002a2		"No seu domicílio há os seguintes itens básicos de limepeza e proteção: álcool 70% ou superior (gel ou líquido)	"
	cap label var f002a3		"No seu domicílio há os seguintes itens básicos de limepeza e proteção: máscaras	"
	cap label var f002a4		"No seu domicílio há os seguintes itens básicos de limepeza e proteção: luvas descartáveis	"
	cap label var f002a5		"No seu domicílio há os seguintes itens básicos de limepeza e proteção: água sanitária ou desinfetante	"
	cap label var f0061		"Quem respondeu ao questionário?	"
	cap label var f006		"Número de ordem do morador que prestou as label var"

	if `month' < 10 {
	save PNAD_COVID_0`month'2020, replace
	}

	if `month' >= 10 {
	save PNAD_COVID_`month'2020, replace
	}

	}
	else continue, break
}


di _newline "Esta versão do pacote datazoom_pnad_covid é compatível com a última versão dos microdados da PNAD Covid divulgados em 23/12/2020"
di _newline "As bases de dados foram salvas em `c(pwd)'"
end
		
