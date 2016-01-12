program define metodo3

args data1 data2

*---------------------------------------------------------------------------------------------------------------------------------*
*B.                                                   Nombres con soundex                                                         *
*---------------------------------------------------------------------------------------------------------------------------------*

* Identificadores finales:
* p2, p222, p3

* Pasos:
* 1. Concateno los dos nombres y los dos apellidos, para pegar por ellos.
* 2. Los transformo con "soundex", que hace correcciones fonéticas (gringas). Esto lo hago para las dos bases.
* 3. Pego por la variable de soundex, municipio de nacimiento y fecha de nacimiento.
* 4. Los que pegan 1:1 puede que sean los mismos, entonces hago una comparación de nombres con superlevenshtein y me quedo
*    con los que cumple superlev<0.15
* 5. Los que pegan m:m pueden ser distintos, entonces me quedo con los que tienen nombre más cercano y borro los que tienen superlev<0.15
* 6. Repito los pasos 4. y 5. pero haciendo comparación de levenshtein entre documentos de identidad.
* NOTA: Este proceso lo hago con nombres transformados por "beto".

cap include superlevenshtein.do
cap include filtros.do

* Creo un local con los años de las bases de datos, para utilizar en adelante.
local year1=substr("`data1'",-2,.)
local year2=substr("`data2'",-2,.)

* Pegar los nombres, nombre a nombre, es muy difícil. Hay fonemas que, aunque se escriben distinto, son iguales. Lo que hago es transformar los
* nombres a su equivalente en fonema, con "soundex" (es gringo, pero puede aportar algo). Luego pego por estos nombres transformados.
* Pegar nombre a nombre multiplicaría demasiado la base de datos (pues hay miles de "Juan Gómez", por ejemplo). Entonces me toca hacer el 
* pegado por nombre, fecha de nacimiento, género y lugar de nacimiento. 


* Creo macros para todos los cruces de un nombre y un apellido, en ese orden. Después hago un append de todos
* estos merges, y para cada identificador me quedo con los que me den el superlevenshtein más cercano.
* Si hay repetidos (que seguramente los habrá, pues muchas observaciones van a pegar igual), hago duplicates grop al final.
* Hago combinaciones con nombre primero y después apellido en las dos, y con un nombre y apellido
* intercambioados, para cad aaño.
forvalues gra=1/2{
	forvalues m=1/2{
		forvalues i=1/2{
			forvalues j=1/2{
				forvalues k=1/2{
					forvalues l=1/2{
						if `m'==1{
							global pegado1`i'`j'`k'`l' "beto_nombre`i'_ beto_apellido`j'_"
							global pegado2`i'`j'`k'`l' "beto_nombre`k'_ beto_apellido`l'_"
						}
						if `m'==2{
							global pegado1`i'`j'`k'`l' "beto_apellido`j'_ beto_nombre`i'_"
							global pegado2`i'`j'`k'`l' "beto_nombre`k'_ beto_apellido`l'_"
						}
						use identificador_z_`year1' dane_sed_`year1' genero_`year1' grado_`year1' ${pegado1`i'`j'`k'`l'} using `data1'_limpio_rena_once.dta, clear
						
						egen nom_1_`year1'=concat(${pegado1`i'`j'`k'`l'})
						gen sound_1=soundex(nom_1_`year1')
						drop nom_1_`year1'
						* Renombro las variables sobre las que voy a pegar (género y sede de colegio).
						
						rename dane_sed_`year1' dane_sed_`year2'
						rename genero_`year1' genero_`year2'
						if `gra'==1{
							replace grado_`year1'=grado_`year1'+1
						}
						rename grado_`year1' grado_`year2'
						
						keep sound_1 dane_sed* genero_* grado_* identificador_z_* 
						
						saveold `data1'_sound.dta, replace
						
						use identificador_z_`year2' dane_sed_`year2' genero_`year2' grado_`year2' ${pegado2`i'`j'`k'`l'} using `data2'_limpio_rena_once.dta, clear
						egen nom_1_`year2'=concat(${pegado2`i'`j'`k'`l'})
						gen sound_1=soundex(nom_1_`year2')
						drop nom_1_`year2'
						keep sound_1 dane_sed* genero_* grado_* identificador_z_* 
						
						mmerge sound_1 dane_sed_`year2' genero_`year2' grado_`year2' using `data1'_sound.dta, ukeep( identificador_z_* ) unmatched(none)
						
						keep identificador_z_`year1' identificador_z_`year2'  
						
						if `l'==2{
							local h=`l'-1
							append using prov_`i'`j'`k'`h'`m'`gra'
							erase prov_`i'`j'`k'`h'`m'`gra'.dta
							duplicates drop identificador_z_`year2' identificador_z_`year1', force

						}
						
						save prov_`i'`j'`k'`l'`m'`gra'.dta, replace
					}

					if `k'==2{
						local h=`k'-1
						append using prov_`i'`j'`h'2`m'`gra'
						erase prov_`i'`j'`h'2`m'`gra'.dta
						duplicates drop identificador_z_`year2' identificador_z_`year1', force
					}
					save prov_`i'`j'`k'2`m'`gra'.dta, replace
				}
				if `j'==2{
					local h=`j'-1
					append using prov_`i'`h'22`m'`gra'
					erase prov_`i'`h'22`m'`gra'.dta
					duplicates drop identificador_z_`year2' identificador_z_`year1', force

				}
				save prov_`i'`j'22`m'`gra'.dta, replace
			}
			if `i'==2{
				local h=`i'-1
				append using prov_`h'222`m'`gra'
				erase prov_`h'222`m'`gra'.dta
				duplicates drop identificador_z_`year2' identificador_z_`year1', force

			}
			save prov_`i'222`m'`gra'.dta, replace
		}
		if `m'==2{
			local h=`m'-1
			append using prov_2222`h'`gra'
			erase prov_2222`h'`gra'.dta
			duplicates drop identificador_z_`year2' identificador_z_`year1', force

		}
		save prov_2222`m'`gra'.dta, replace
	}
	if `gra'==2{
		local h=`gra'-1
		append using prov_22222`h'
		erase prov_22222`h'.dta
		duplicates drop identificador_z_`year2' identificador_z_`year1', force
	}
	save prov_22222`gra'.dta, replace
}

merge m:1 identificador_z_`year1' using `data1'_limpio_rena_once.dta, keepusing(nombre* apellido* nro_docu_*)
drop if _m==2
drop _merge
merge m:1 identificador_z_`year2' using `data2'_limpio_rena_once.dta, keepusing(nombre* apellido* nro_docu_*)
drop if _m==2
drop _merge

superlevenshtein nombre documento

erase prov_222222.dta
erase `data1'_sound.dta

save metodo3_`year1'_`year2'.dta,replace

filtros nombre

keep identificador_z_`year1' identificador_z_`year2'

merge m:1 identificador_z_`year1' using `data1'_limpio_rena_once.dta
drop if _m==2
drop _merge
merge m:1 identificador_z_`year2' using `data2'_limpio_rena_once.dta
drop if _m==2
drop _merge

superlevenshtein nombre beto fecha documento grado genero lugar diferencia

save m3_nom_`year1'_`year2'.dta,replace

use metodo3_`year1'_`year2'.dta, clear

filtros documento

keep identificador_z_`year1' identificador_z_`year2'

merge m:1 identificador_z_`year1' using `data1'_limpio_rena_once.dta
drop if _m==2
drop _merge
merge m:1 identificador_z_`year2' using `data2'_limpio_rena_once.dta
drop if _m==2
drop _merge

superlevenshtein nombre beto fecha documento grado genero lugar diferencia

save m3_doc_`year1'_`year2'.dta,replace



end
