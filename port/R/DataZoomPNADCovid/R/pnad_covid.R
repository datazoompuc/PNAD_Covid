#' @importFrom dplyr %>%
#' @importFrom labelled set_variable_labels
#' @importFrom purrr map map2
#' @importFrom readr cols read_csv col_double col_integer
#' @importFrom stringr str_pad

NULL







#' Carregando microdados da PNAD COVID19
#'
#' Extrai e constrói bases de dados da PNAD COVID19 em formato R (.RData) a partir
#' dos microdados originais, os quais  não são disponibilizados pelo Portal do IBGE
#' (para informa??es sobre como obter os arquivos originais de dados,
#' consulte o site do IBGE (www.ibge.gov.br).
#' Como a pesquisa ainda é publicada pelo IBGE, este programa está em constante atualização.
#'  A função gera uma base para cada mês. Se for o caso, utilize o
#'  comando  \code{\link{bind_rows}} para empilhar as bases.
#'
#' @param diretorio_dados Diretório onde os microdados originais em formato de texto estão armazenados
#'
#' @param ... vetores com datas das pesquisas de interesse no  formato \code{c('m?s', 'ano')}
#'
#' @return Lista de dataframes, sendo cada entrada um m?s/ano em \code{...}
#' @encoding UTF-8
#' @export
#'
#' @examples
#' datazoom_pnadc('./Desktop', c(5, 2020))
#'

pnad_covid_microdados <- function(diretorio_dados,
                       ...){

  datas <- list(...)

  if (any(map(datas, length) != 2)) {stop('Escolha o mesmo N?mero de anos e meses', call. = FALSE)}
  mes <- datas %>% map( ~ .x[[1]])
  ano <- datas %>% map( ~ .x[[2]])

  if (sum(mes %in% 5:12 == F) > 0) {stop('A PNAD-COVID come?a em maio de 2020', call. = FALSE)}
  if (sum(ano != 2020) > 0) {stop('A PNAD-COVID considera o ano de 2020', call. = FALSE)}

mes <- mes %>% map(~.x %>% str_pad(width = 2, pad = 0))
datas <- map2(mes, ano, paste0)

filepath_in <- datas %>% map(~ .x %>% paste0('PNAD_COVID_',., '.csv') %>%
                               file.path(diretorio_dados, ., sep = '')
                             )

dados <- filepath_in %>%
  #####
map( ~ .x %>%
       read_csv(
         .,
         col_types = cols(
           Ano = col_integer(),
           UF = col_integer(),
           CAPITAL = col_integer(),
           RM_RIDE = col_integer(),
           V1008 = col_integer(),
           V1012 = col_integer(),
           V1013 = col_integer(),
           V1016 = col_integer(),
           V1022 = col_integer(),
           V1023 = col_integer(),
           posest = col_integer(),
           A001 = col_integer(),
           A001A = col_integer(),
           A001B1 = col_integer(),
           A001B2 = col_integer(),
           A001B3 = col_integer(),
           A002 = col_integer(),
           A003 = col_integer(),
           A004 = col_integer(),
           A005 = col_integer(),
           B0011 = col_integer(),
           B0012 = col_integer(),
           B0013 = col_integer(),
           B0014 = col_integer(),
           B0015 = col_integer(),
           B0016 = col_integer(),
           B0017 = col_integer(),
           B0018 = col_integer(),
           B0019 = col_integer(),
           B00110 = col_integer(),
           B00111 = col_integer(),
           B00112 = col_integer(),
           B002 = col_integer(),
           B0031 = col_integer(),
           B0032 = col_integer(),
           B0033 = col_integer(),
           B0034 = col_integer(),
           B0035 = col_integer(),
           B0036 = col_integer(),
           B0037 = col_integer(),
           B0041 = col_integer(),
           B0042 = col_integer(),
           B0043 = col_integer(),
           B0044 = col_integer(),
           B0045 = col_integer(),
           B0046 = col_integer(),
           B005 = col_integer(),
           B006 = col_integer(),
           B007 = col_integer(),
           C001 = col_integer(),
           C002 = col_integer(),
           C003 = col_integer(),
           C004 = col_integer(),
           C005 = col_integer(),
           C0051 = col_integer(),
           C0052 = col_integer(),
           C0053 = col_integer(),
           C006 = col_integer(),
           C007 = col_integer(),
           C007A = col_integer(),
           C007B = col_integer(),
           C007C = col_integer(),
           C007D = col_integer(),
           C007E = col_integer(),
           C007E1 = col_integer(),
           C007E2 = col_integer(),
           C008 = col_integer(),
           C009 = col_integer(),
           C010 = col_integer(),
           C0101 = col_integer(),
           C01011 = col_integer(),
           C01012 = col_double(),
           C0102 = col_integer(),
           C01021 = col_integer(),
           C01022 = col_double(),
           C0103 = col_integer(),
           C0104 = col_integer(),
           C011A = col_double(),
           C011A1 = col_integer(),
           C011A11 = col_integer(),
           C011A12 = col_double(),
           C011A2 = col_integer(),
           C011A21 = col_integer(),
           C011A22 = col_double(),
           C012 = col_integer(),
           C013 = col_integer(),
           C014 = col_integer(),
           C015 = col_integer(),
           C016 = col_integer(),
           C017A = col_integer(),
           D0011 = col_integer(),
           D0013 = col_double(),
           D0021 = col_integer(),
           D0023 = col_double(),
           D0031 = col_integer(),
           D0033 = col_double(),
           D0041 = col_integer(),
           D0043 = col_double(),
           D0051 = col_integer(),
           D0053 = col_double(),
           D0061 = col_integer(),
           D0063 = col_double(),
           D0071 = col_integer(),
           D0073 = col_double(),
           F001 = col_integer(),
           F0021 = col_double(),
           F0022 = col_integer(),
           F0061 = col_integer(),
           F006 = col_integer()
         )
       ))

Descricoes <-
#####
data.frame(
  Codigo_da_variavel = c(
    "Ano",
    "UF",
    "CAPITAL",
    "RM_RIDE",
    "V1008",
    "V1012",
    "V1013",
    "V1016",
    "Estrato",
    "UPA",
    "V1022",
    "V1023",
    "V1030",
    "V1031",
    "V1032",
    "posest",
    "A001",
    "A001A",
    "A001B1",
    "A001B2",
    "A001B3",
    "A002",
    "A003",
    "A004",
    "A005",
    "B0011",
    "B0012",
    "B0013",
    "B0014",
    "B0015",
    "B0016",
    "B0017",
    "B0018",
    "B0019",
    "B00110",
    "B00111",
    "B00112",
    "B002",
    "B0031",
    "B0032",
    "B0033",
    "B0034",
    "B0035",
    "B0036",
    "B0037",
    "B0041",
    "B0042",
    "B0043",
    "B0044",
    "B0045",
    "B0046",
    "B005",
    "B006",
    "B007",
    "C001",
    "C002",
    "C003",
    "C004",
    "C005",
    "C0051",
    "C0052",
    "C0053",
    "C006",
    "C007",
    "C007A",
    "C007B",
    "C007C",
    "C007D",
    "C007E",
    "C007E1",
    "C007E2",
    "C008",
    "C009",
    "C010",
    "C0101",
    "C01011",
    "C01012",
    "C0102",
    "C01021",
    "C01022",
    "C0103",
    "C0104",
    "C011A",
    "C011A1",
    "C011A11",
    "C011A12",
    "C011A2",
    "C011A21",
    "C011A22",
    "C012",
    "C013",
    "C014",
    "C015",
    "C016",
    "C017A",
    "D0011",
    "D0013",
    "D0021",
    "D0023",
    "D0031",
    "D0033",
    "D0041",
    "D0043",
    "D0051",
    "D0053",
    "D0061",
    "D0063",
    "D0071",
    "D0073",
    "F001",
    "F0021",
    "F0022",
    "F0061",
    "F006"
  ),
  descricao = c(
    "Ano de refer?ncia",
    "Unidade da Federa??o",
    "Capital",
    "Regi?o Metropolitana e Regi?o Administrativa Integrada de Desenvolvimento",
    "N?mero de sele??o do domic?lio",
    "Semana no m?s",
    "M?s da pesquisa",
    "N?mero da entrevista no domic?lio",
    "Estrato",
    "UPA",
    "Situa??o do domic?lio",
    "Tipo de ?rea",
    "Proje??o da popula??o",
    "Peso do domic?lio e das pessoas - sem p?s estratifica??o",
    "Peso do domic?lio e das pessoas - com p?s estratifica??o",
    "Dom?nios de proje??o",
    "N?mero de ordem",
    "Condi??o no domic?lio",
    "Dia de nascimento",
    "M?s de nascimento",
    "Ano de nascimento",
    "Idade do morador",
    "Sexo",
    "Cor ou ra?a",
    "Escolaridade",
    "Nos ?ltimos sete dias teve febre?",
    "Nos ?ltimos sete dias teve tosse?",
    "Nos ?ltimos sete dias teve dor de garganta?",
    "Nos ?ltimos sete dias teve dificuldade para respirar?",
    "Nos ?ltimos sete dias teve dor de cabe?a?",
    "Nos ?ltimos sete dias teve dor no peito?",
    "Nos ?ltimos sete dias teve n?usea?",
    "Nos ?ltimos sete dias teve nariz entupido ou escorrendo?",
    "Nos ?ltimos sete dias teve fadiga?",
    "Nos ?ltimos sete dias teve dor nos olhos?",
    "Nos ?ltimos sete dias teve perda de cheiro ou sabor?",
    "Nos ?ltimos sete dias teve dor muscular?",
    "Por causa disso, foi a algum estabelecimento de sa?de?",
    "Provid?ncia tomada para recuperar dos sintomas foi ficar em casa",
    "Provid?ncia tomada para recuperar dos sintomas foi ligar para algum
    profissional de sa?de",
    "Provid?ncia tomada  para recuperar dos sintomas foi comprar e/ou tomar
    rem?dio por conta própria",
    "Provid?ncia tomada para recuperar dos sintomas foi comprar
                              e/ou tomar rem?dio por orienta??o m?dica",
    "Provid?ncia tomada para recuperar dos sintomas foi
                              receber visita de algum profissional de sa?de do
    SUS (equipe de sa?de da fam?lia, agente comunit?rio, etc.)",
    "Provid?ncia tomada para recuperar dos sintomas foi receber
                              visita de profissional de sa?de particular",
    "Provid?ncia tomada para recuperar dos sintomas foi outra",
    "Local que buscou atendimento foi posto de sa?de/Unidade b?sica de
    sa?de /Equipe de sa?de da Fam?lia (m?dico, enfermeiro, t?cnico de enfermagem
    ou agente comunit?rio de sa?de)",
    "Local que buscou atendimento foi pronto socorro do SUS/UPA",
    "Local que buscou atendimento foi hospital do SUS",
    "Local que buscou atendimento foi ambulat?rio ou consult?rio privado ou
    ligado ?s for?as armadas",
    "Local que buscou atendimento foi pronto socorro privado ou ligado ?s
    for?as armadas",
    "Local que buscou atendimento foi hospital privado ou ligado ?s for?as armadas",
    "Ao procurar o hospital, teve que ficar internado por um dia ou mais",
    "Durante a interna??o, foisedado, entubado e colocado em
    respira??o artificial com ventilador",
    "Tem algum plano de sa?de m?dico, seja particular,
    de empresa ou de ?rg?o p?blico",
    "Na semana passada, por pelo menos uma hora, trabalhou ou fez algum bico?",
    "Na semana passada, estava temporariamente afastado de algum trabalho?",
    "Qual o principal motivo deste afastamento tempor?rio?",
    "Continuou a ser remunerado (mesmo que parcialmente) por esse trabalho",
    "H? quanto tempo est? afastado desse trabalho?",
    "Tempo que estava afastado (De 1 m?s a menos de 1 ano)",
    "Tempo que estava afastado (De 1 ano a menos de 2 anos)",
    "Tempo que estava afastado (de 02 anos a 98 anos)",
    "Tem mais de um trabalho",
    "No trabalho (t?cnico ou principal) que tinha nessa semana, era:",
    "Esse trabalho era na ?rea:",
    "Tem carteria de trabalho assinada ou ? funcion?rio p?blico estatut?rio?",
    "Que tipo de trabalho, cargo ou fun??o voc? realiza no seu trabalho (t?cnico
    ou principal)?",
    "Qual ? a principal atividade do local ou empresa em que voc? trabalha?",
    "Na semana passada, quantos empregados trabalhavam nesse neg?cio/empresa
    que ... tinha ?",
    "1 a 5 empregados",
    "6 a 10 empregados",
    "Quantas horas, por semana, normalmente trabalhava?",
    "Quantas horas, na semana passada, de fato trabalhou?",
    "Quanto recebia (ou retirava) normalmente em todos os seus trabalhos",
    "Recebia/retirava normalmente em dinheiro",
    "N?mero da faixa do rendimento/retirada em dinheiro",
    "Valor em dinheiro",
    "Recebia normalmente em produtos e mercadorias",
    "N?mero da faixa do rendimento/retirada em produtos e mercadorias",
    "Valor em produtos e mercadorias",
    "Recebia normalmente somente em benef?cios",
    "Era n?o remunerado",
    "Quanto recebia (ou retirava) normalmente em todos os seus trabalhos",
    "Recebia/retirava normalmente em dinheiro",
    "N?mero da faixa do rendimento/retirada em dinheiro",
    "Valor em dinheiro",
    "Recebia normalmente em produtos e mercadorias",
    "N?mero da faixa do rendimento/retirada em produtos e mercadorias",
    "Valor em produtos e mercadorias",
    "Na maior parte do tempo, na semana passada,
                              esse trabalho (t?cnico ou principal) foi exercido
    no mesmo local em que costuma trabalhar?",
    "Na semana passada, o(a) Sr(a) estava em trabalho remoto
    (home office ou teletrabalho)?",
    "O(A) Sr(a) contribui para o INSS?",
    "Na semana passada ___ tomou alguma provid?ncia
    efetiva para conseguir trabalho?",
    "Qual o principal motivo de n?o ter procurado trabalho na semana passada?",
    "Embora voc? n?o tenha procurado trabalho, gostaria de ter trabalhado na semana passada?",
    "Rendimento recebido de aposentadoria epens?o por todos os moradores",
    "Somat?rio dos valores recebidos",
    "Rendimento depens?o aliment?cia, doa??o ou mesada em dinheiro de pessoa
    que n?o morava no domic?lio",
    "Somat?rio dos valores recebidos",
    "Rendimentos de Programa Bolsa Fam?lia",
    "Somat?rio dos valores recebidos",
    "No m?s de ... (m?s de refer?ncia), ... recebeu rendimentos de Benef?cio
    Assistencial de Presta??o Continuada - BPC-LOAS?",
    "Somat?rio dos valores recebidos",
    "Auxilios emergenciais relacionados ao coronav?rus",
    "Somat?rio dos valores recebidos",
    "Seguro desemprego",
    "Somat?rio dos valores recebidos",
    "Outros rendimentos, como aluguel, arrendamento, previd?ncia privada,
    bolsa de estudos, rendimentos de aplica??o financeira etc.",
    "Somat?rio dos valores recebidos",
    "Este domic?lio ?:",
    "Qual foi o valor mensal do aluguel pago, ou que deveria ter sido pago, no m?s de refer?ncia?",
    "N?mero da faixa do aluguel pago",
    "Quem respondeu ao question?rio?",
    "N?mero de ordem do morador que prestou as informa??es"
  )
)


#####


lista <- as.list(Descricoes$descricao)
names(lista) <- as.list(Descricoes$Codigo_da_variavel)

dados <-  dados %>% map(~.x %>% set_variable_labels(.labels = lista))

names(dados) <- paste0('pnad_covid_',datas)
return(dados)
}




