*********************************************
*
*         Determinantes de desnutrición
*         crónica infantil - ENDES 2023
*
**********************************************

clear all
set more off

***********       RECH0_2023   ***************

cd "C:\Users\Usuario\Documents\desnutrición_infantil"
use "RECH0_2023.dta", clear

* Exploramos las variables 
describe
codebook HHID HV014 HV024 HV025 HV026 HV040 HV022
sum HHID HV014 HV024 HV025 HV026 HV040 HV022

* Conservamos las variables necesariasr *
keep HHID HV014 HV024 HV025 HV026 HV040 HV022

* Filtramos solo Lima *
label list
keep if HV024 == 15

* Creamos una nueva variable 'rural_urbano' 
gen rural_urbano = .
replace rural_urbano = 1 if HV026 == 0 | HV026 == 1
replace rural_urbano = 0 if HV026 == 2 | HV026 == 3

* Eliminamos la etiqueta automática para luego establecer una propia 
label drop rural_urbano_lbl
label define rural_urbano_lbl 0 "Rural" 1 "Urbano"
label values rural_urbano rural_urbano_lbl

* Guardamos esta nueva base limpia *
save "C:\Users\Usuario\Documents\desnutrición_infantil\RECH0_2023_Limpio.dta", replace

***********      RECH1_2023    ***************
use "RECH1_2023.dta", clear

* Exploramos las variables 
describe
codebook HHID HV014 HV024 HV025 HV026 HV040 HV022
sum HHID HV014 HV024 HV025 HV026 HV040 HV022

* variables de interés
keep HHID HVIDX HV101 HV104 HV105 HV106 HV108 HV109

* Nos quedamos solo con los jefes de hogar y sus esposas(os) 
keep if HV101 == 1 | HV101 == 2

* Creamos variables para almacenar los años de educación de hombres y mujeres 
gen educ_hombre_tmp = .
gen educ_mujer_tmp = .
* Asignamos los años de educación a las nuevas variables temporales 
replace educ_hombre_tmp = HV108 if HV104 == 1
replace educ_mujer_tmp = HV108 if HV104 == 2

* Colapsamos los datos para obtener una sola observación por hogar 
collapse (max) educ_hombre_tmp educ_mujer_tmp, by(HHID)

* Renombramos las variables para clarificar y evitamos valores extremos 
rename educ_hombre_tmp Educ_Padre
rename educ_mujer_tmp Educ_Madre
keep if Educ_Madre < 25
keep if Educ_Padre < 25

* Guardar el nuevo dataset *
save "C:\Users\Usuario\Documents\desnutrición_infantil\RECH1_2023_Limpio.dta", replace

***********     RECH23_2023    ***************
use "RECH23_2023.dta", clear

* Explorando las variables
describe
codebook HHID HV201 HV205 HV206 HV207 HV208 HV213 HV214 HV215 HV219 SHREGION SH70 SH227 HV270 HV271

* Variables de interés
keep HHID HV201 HV205 HV206 HV207 HV208 HV213 HV214 HV215 HV219 SHREGION SH70 SH227 HV270 HV271

* Guardar el nuevo dataset *
save "C:\Users\Usuario\Documents\desnutrición_infantil\RECH23_2023_Limpio.dta", replace

***********    RECH6_2023    ***************
use "RECH6_2023.dta", clear

* Revisamos si hay outlayers *
histogram HC70
tab HC70

* Los outlayers se encuentran entre -582 y 531 
keep if HC70 <= 560
* Comprobamos *
tab HC70

*********          MERGE         **********
* Unimos primero con RECH0 que son las características del hogar *

merge m:1 HHID using "RECH0_2023_Limpio.dta"

* Mantenemos los que hicieron merge(3) porque los otros están fuera de la región que queremos estudiar (Lima) *
keep if _merge == 3
* Eliminamos la variable _merge para hacer otro merge *
drop _merge

* Ahora el merge con las características del padre y madre en educación *
merge m:1 HHID using "\RECH1_2023_Limpio.dta"
keep if _merge == 3
drop _merge

* Ahora el merge con las condiciones del hogar en cuanto a servicios 
merge m:1 HHID using "RECH23_2023_Limpio.dta"
keep if _merge == 3
drop _merge

***********   Base de trabajo ***************
save "C:\Users\Usuario\Documents\desnutrición_infantil\Base_trabajo.dta", replace

*                   Modelo 1 
use "Base_trabajo.dta", clear

* Variables interactivas
generate tv_radio = HV207 * HV208

* Regresión lineal
reg HC70 HV014 rural_urbano Educ_Madre, noc
outreg2 using tabla.doc, replace ctitle(Modelo 1) addstat(R2, e(r2), "R2 ajustado", e(r2_a), "Num. Obs", e(N), SCR, e(rss), F, e(F), "Prob > F", e(p))
estimates store Modelo1

* Prueba de Multicolinialidad 
reg HC70 HV014 rural_urbano Educ_Madre
vif

* Deteccion de heterocedasticidad
reg HC70 HV014 rural_urbano Educ_Madre
* Prueba de White
estat imtest 

reg HC70 HV014 rural_urbano Educ_Madre
* Prueba B-P
estat hettest

*                  Modelo 2 
reg HC70 HV040 HV271 Educ_Madre tv_radio, noc
outreg2 using tabla.doc, append ctitle(Modelo 2) addstat(R2, e(r2), "R2 ajustado", e(r2_a), "Num. Obs", e(N), SCR, e(rss), F, e(F), "Prob > F", e(p))
estimates store Modelo2

* Prueba de Multicolinialidad 
reg HC70 HV040 HV271 Educ_Madre tv_radio
vif

* Deteccion de heterocedasticidad
reg HC70 HV040 HV271 Educ_Madre tv_radio
* Prueba de White
estat imtest 

reg HC70 HV040 HV271 Educ_Madre tv_radio
* Prueba B-P
estat hettest

*                  Modelo 3 
tab HV219, gen(sexo_jefe)
reg HC70 tv_radio Educ_Madre sexo_jefe1, noc
outreg2 using tabla.doc, append ctitle(Modelo 3) addstat(R2, e(r2), "R2 ajustado", e(r2_a), "Num. Obs", e(N), SCR, e(rss), F, e(F), "Prob > F", e(p))
estimates store Modelo3

* Prueba de Multicolinialidad 
reg HC70 tv_radio Educ_Madre sexo_jefe1
vif

* Deteccion de heterocedasticidad
reg HC70 tv_radio Educ_Madre sexo_jefe1
* Prueba de White
estat imtest 

reg HC70 tv_radio Educ_Madre sexo_jefe1
* Prueba B-P
estat hettest

*Mostramos resultados
estimates table Modelo1 Modelo2 Modelo3, star(.05 .01 .001)

