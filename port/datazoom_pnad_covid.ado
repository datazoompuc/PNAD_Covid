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
	if (`month' == 05) {
		di as input "Extraindo arquivo PNAD_COVID_`trim'2020  ..."
		cap infile using "`dic'", using("`original'/PNAD_COVID_`month'.csv") clear			
		if _rc == 0 {
			save PNADC_`trim'`month', replace
		}
		else continue, break
	}
}

*if _rc != 0 continue, break

di _newline "Esta versão do pacote datazoom_pnad_covid é compatível com a última versão dos microdados da PNAD Covid divulgados em 15/05/2020"
di _newline "As bases de dados foram salvas em `c(pwd)'"
end
