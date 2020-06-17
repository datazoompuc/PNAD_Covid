******************************************************
*             datazoom_pnad_covid.ado              *
******************************************************
* version 1.0

program define datazoom_pnad_covid

syntax, weeks(numlist) original(str) saving(str) 

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
foreach week in `weeks'{
	foreach trim in 01 02 03 04 {
			if (`trim' == 04 & `week' == 2019) {
			di as input "Extraindo arquivo PNADC_`trim'`week'  ..."
				cap infile using "`dic'", using("`original'/PNADC_`trim'`week'.txt") clear
				if _rc == 0 {
					qui capture egen hous_id = concat(UPA V1008 V1014), format(%14.0g)
					qui destring hous_id, replace
					qui capture egen ind_id = concat(UPA V1008 V1014 V2003), format(%16.0g)
					qui destring ind_id, replace
					save PNADC_`trim'`week', replace
					}
				else continue, break
			}	
			else if (`trim' == 03 & `week' == 2019) {
			di as input "Extraindo arquivo PNADC_`trim'`week'  ..."
				cap infile using "`dic'", using("`original'/PNADC_`trim'`week'.txt") clear
				if _rc == 0 {
					qui capture egen hous_id = concat(UPA V1008 V1014), format(%14.0g)
					qui destring hous_id, replace
					qui capture egen ind_id = concat(UPA V1008 V1014 V2003), format(%16.0g)
					qui destring ind_id, replace
					save PNADC_`trim'`week', replace
					}
				else continue, break
			}
			else if (`trim' == 02 & `week' == 2019) {
			di as input "Extraindo arquivo PNADC_`trim'`week'  ..."
				cap infile using "`dic'", using("`original'/PNADC_`trim'`week'.txt") clear
				if _rc == 0 {
					qui capture egen hous_id = concat(UPA V1008 V1014), format(%14.0g)
					qui destring hous_id, replace
					qui capture egen ind_id = concat(UPA V1008 V1014 V2003), format(%16.0g)
					qui destring ind_id, replace
					save PNADC_`trim'`week', replace
					}
				else continue, break
			}						
			else  {
			di as input "Extraindo arquivo PNADC_`trim'`week'  ..."
				cap infile using "`dic'", using("`original'/PNADC_`trim'`week'_20190729.txt") clear
				if _rc == 0 {
					qui capture egen hous_id = concat(UPA V1008 V1014), format(%14.0g)
					qui destring hous_id, replace
					qui capture egen ind_id = concat(UPA V1008 V1014 V2003), format(%16.0g)
					qui destring ind_id, replace
					save PNADC_`trim'`week', replace
					}
				else continue, break
			}
			}

	}

*if _rc != 0 continue, break

di _newline "Esta versão do pacote datazoom_pnad_covid é compatível com a última versão dos microdados da PNAD Covid divulgados em 15/05/2020"
di _newline "As bases de dados foram salvas em `c(pwd)'"
end
