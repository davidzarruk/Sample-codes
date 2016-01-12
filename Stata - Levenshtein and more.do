program define superlevenshtein
version 9.2, missing

args lev1 lev2 lev3 lev4 lev5 lev6 lev7 lev8

* nombre beto documento grado genero nacimiento residencia diferencia colegio

* Este programa me crea distintos métodos de desempate, que usaré en distintas combinaciones.
* 1. nombrelev (betolev): levenshtein de nombres (normal y beto)
* 2. documentolev: levenshtein de documento
* 3. fechalev: indicador de diferencia en fechas de nacimiento (=1 si se descachó en día, mes o año, =2 si se descachó en 2, etc.)
* 4. lugarlev: indicador de diferencia en municipio de nacimiento (=1 si reportan distintos municipios, 0 si igual)
* 5. gradolev: diferencia de más de dos grados de un año al siguiente (=1 si al siguiente año cayó dos grados o más o subió tres o más)
* 6. generolev: diferencia de géneros
* 7. diferencialev (sólo se usa en el método1 de pegado)

quietly{

*-------------------------------*
*    LEVENSHTEIN DE NOMBRES     *
*-------------------------------*
* Creo un local con los años de las bases de datos, para utilizar en adelante.
aorder 
describe nombre1_*, varlist 
local a=r(varlist)
display "`a'"
tokenize "`a'"

* Estos local son de los años
local year1=substr("`1'",-2,.)
local year2=substr("`2'",-2,.)

global nombre ""
global beto ""

if "`lev1'"=="nombre" | "`lev2'"=="nombre" | "`lev3'"=="nombre" | "`lev4'"=="nombre" | "`lev5'"=="nombre" | "`lev6'"=="nombre" | "`lev7'"=="nombre" | "`lev8'"=="nombre"{
	global nombre "nombre"
}

if "`lev1'"=="beto" | "`lev2'"=="beto" | "`lev3'"=="beto" | "`lev4'"=="beto" | "`lev5'"=="beto" | "`lev6'"=="beto" | "`lev7'"=="beto" | "`lev8'"=="beto"{
	global beto "beto"
}

foreach tipo in $nombre $beto{
	if "`tipo'"=="beto"{
		rename beto_nombre1_`year1' n1_`year1'
		rename beto_nombre2_`year1' n2_`year1'
		rename beto_apellido1_`year1' n3_`year1'
		rename beto_apellido2_`year1' n4_`year1'
		rename beto_nombre1_`year2' n1_`year2'
		rename beto_nombre2_`year2' n2_`year2'
		rename beto_apellido1_`year2' n3_`year2'
		rename beto_apellido2_`year2' n4_`year2'
	}

	if "`tipo'"!="beto"{
		rename nombre1_`year1' n1_`year1'
		rename nombre2_`year1' n2_`year1'
		rename apellido1_`year1' n3_`year1'
		rename apellido2_`year1' n4_`year1'
		rename nombre1_`year2' n1_`year2'
		rename nombre2_`year2' n2_`year2'
		rename apellido1_`year2' n3_`year2'
		rename apellido2_`year2' n4_`year2'
	}

	* Encuentro las longitudes de las palabras
	foreach k in `year1' `year2'{
		forvalues y=1/4{
			gen l_`y'_`k'=length(n`y'_`k')
		}
	}
	
	* Encuentro el levenshtein de cada nombre/apellido entre todos con todos.
	forvalues i=1/4{
		global temporal ""
		forvalues j=1/4{
			levenshtein n`i'_`year1' n`j'_`year2', gen(p_`i'_`j')
			
			* Voy a arreglar el levenshtein cuando un nombre es substring de otro (pasa, por ejemplo, con "ma" y "maria").
			gen sub1=strpos(n`j'_`year2',n`i'_`year1')
			gen sub2=strpos(n`i'_`year1',n`j'_`year2')
			
			replace p_`i'_`j'=0 if (sub1>0 | sub2>0) & (n`i'_`year1'!="" & n`j'_`year2'!="")
			drop sub1 sub2
			
			* Cuando el levenshtein es igual a la diferencia en longitudes, y la diferencia es pequeña, es porque se comieron letras.
			* Aquí arreglo eso
			gen diferencia_long=abs(l_`i'_`year1' - l_`j'_`year2')
			replace p_`i'_`j'=0 if (diferencia_long==p_`i'_`j') & (n`i'_`year1'!="" & n`j'_`year2'!="") & diferencia_long<=3
			drop diferencia_long
			
			if `j'!=4{
				global temporal "$temporal p_`i'_`j',"
			}
			if `j'==4{
				global temporal "$temporal p_`i'_`j'"
			}
		}
		* El mínimo lo uso inmediatamente para encontrar el levenshtein inicial
		gen min_`i'=min($temporal)
	}
	gen min_suma=min_1+min_2+min_3+min_4
	rowsort min_1 min_2 min_3 min_4, gen (max4_`year1' max3_`year1' max2_`year1' max_`year1')
	
	forvalues j=1/4{
		global temporal ""
		forvalues i=1/4{
			if `i'!=4{
				global temporal "$temporal p_`i'_`j',"
			}
			if `i'==4{
				global temporal "$temporal p_`i'_`j'"
			}
		}
		* El mínimo lo uso inmediatamente para encontrar el levenshtein inicial
		gen min2_`j'=min($temporal)
		drop p_*_`j'
	}
	gen min_suma2=min2_1+min2_2+min2_3+min2_4
	rowsort min2_1 min2_2 min2_3 min2_4, gen (max4_`year2' max3_`year2' max2_`year2' max_`year2')
	
	* Genero la longitud de cada uno de los nombres
	foreach k in `year1' `year2'{
		gen min_`k'=l_1_`k'+l_2_`k'+l_3_`k'+l_4_`k'
	}
	* Esta es la longitud total del nombre, mínima
	egen min_total=rowmean(min_`year1'  min_`year2')
	
	* Cuento el número de palabras no vacías, para corregir el levenshtein
	foreach j in `year1' `year2'{
		forvalues i=1/4{
			gen zero_`i'_`j'=(n`i'_`j'!="")
		}
		gen palabras_`j'=zero_1_`j'+zero_2_`j'+zero_3_`j'+zero_4_`j'
	}

	gen diferencia=palabras_`year1' - palabras_`year2'

	* Este es el superlevenshtein normal, teniendo en cuenta que puede haber distinto número de nombres
	gen superlev=(min_suma)/min_total 
	replace superlev=(min_suma-max_`year1')/min_total if diferencia==1
	replace superlev=(min_suma-max_`year1'-max2_`year1')/min_total if diferencia==2
	replace superlev=(min_suma-max_`year1'-max2_`year1'-max3_`year1')/min_total if diferencia==3
	replace superlev=(min_suma2-max_`year2')/min_total if diferencia==-1
	replace superlev=(min_suma2-max_`year2'-max2_`year2')/min_total if diferencia==-2
	replace superlev=(min_suma2-max_`year2'-max2_`year2'-max3_`year2')/min_total if diferencia==-3
	replace superlev=(min_suma)/min_total if 0==diferencia
	
	gen super2lev=(min_suma-max_`year1')/min_total if max_`year1'>max_`year2' & diferencia==0
	replace super2lev=(min_suma2-max_`year2')/min_total if max_`year1'<=max_`year2' & diferencia==0
	
	replace super2lev=(min_suma-max_`year1'-max2_`year1')/min_total if diferencia==1
	replace super2lev=(min_suma-max_`year1'-max2_`year1'-max3_`year1')/min_total if diferencia==2
	replace super2lev=. if diferencia==3
	replace super2lev=(min_suma2-max_`year2'-max2_`year2')/min_total if diferencia==-1
	replace super2lev=(min_suma2-max_`year2'-max2_`year2'-max3_`year2')/min_total if diferencia==-2
	replace super2lev=. if diferencia==-3

	drop min_suma min_1 min_2 min_3 min_4 max*_`year1' min_suma2 min2_1 min2_2 min2_3 min2_4 max*_`year2' min_total palabras_* diferencia zero* l_* min_`year1' min_`year2'

	rename superlev `tipo'lev
	rename super2lev `tipo'2lev
	
	if "`tipo'"=="beto"{
		rename n1_`year1' beto_nombre1_`year1' 
		rename n2_`year1' beto_nombre2_`year1' 
		rename n3_`year1' beto_apellido1_`year1' 
		rename n4_`year1' beto_apellido2_`year1' 
		rename n1_`year2' beto_nombre1_`year2' 
		rename n2_`year2' beto_nombre2_`year2' 
		rename n3_`year2' beto_apellido1_`year2' 
		rename n4_`year2' beto_apellido2_`year2' 
	}

	if "`tipo'"!="beto"{
		rename n1_`year1' nombre1_`year1' 
		rename n2_`year1' nombre2_`year1' 
		rename n3_`year1' apellido1_`year1' 
		rename n4_`year1' apellido2_`year1' 
		rename n1_`year2' nombre1_`year2' 
		rename n2_`year2' nombre2_`year2' 
		rename n3_`year2' apellido1_`year2' 
		rename n4_`year2' apellido2_`year2' 
	}
}
	

*-------------------------------*
*    INDICADOR DE FECHAS        *
*-------------------------------*
if "`lev1'"=="fecha" | "`lev2'"=="fecha" | "`lev3'"=="fecha" | "`lev4'"=="fecha" | "`lev5'"=="fecha" | "`lev6'"=="fecha" | "`lev7'"=="fecha" | "`lev8'"=="fecha"{
	gen dia_na_`year1'=day(fecha_na_`year1')
	gen dia_na_`year2'=day(fecha_na_`year2')

	gen mes_na_`year1'=month(fecha_na_`year1')
	gen mes_na_`year2'=month(fecha_na_`year2')

	gen ano_na_`year1'=year(fecha_na_`year1')
	gen ano_na_`year2'=year(fecha_na_`year2')

	gen dif_dia=(dia_na_`year1'!=dia_na_`year2')
	gen dif_mes=(mes_na_`year1'!=mes_na_`year2')
	gen dif_ano=(ano_na_`year1'!=ano_na_`year2')

	gen fechalev=dif_dia+dif_mes+dif_ano
	drop dia_* mes_* ano_* dif_*
}

*-------------------------------*
*    LEVENSHTEIN DE DOCUMENTOS  *
*-------------------------------*
if "`lev1'"=="documento" | "`lev2'"=="documento" | "`lev3'"=="documento" | "`lev4'"=="documento" | "`lev5'"=="documento" | "`lev6'"=="documento" | "`lev7'"=="documento" | "`lev8'"=="documento"{
	levenshtein nro_docu_`year2' nro_docu_`year1', gen(documentolev)

	gen l1=length(nro_docu_`year1')
	gen l2=length(nro_docu_`year2')
	egen minim=rowmean(l1 l2)
	replace documentolev=documentolev/minim
	drop minim
	
	gen p1=strpos(nro_docu_`year2', nro_docu_`year1')
	gen p2=strpos(nro_docu_`year1', nro_docu_`year2')
	replace documentolev=0 if (p1>0 | p2>0) & (nro_docu_`year1'!="" & nro_docu_`year2'!="")
	drop p1 p2

	gen diferencia_long=abs(l1 - l2)
	replace documentolev=0 if (diferencia_long==documentolev) & (nro_docu_`year1'!="" & nro_docu_`year2'!="") & diferencia_long<3
	drop diferencia_long l1 l2
}

*-------------------------------*
*    INDICADOR DE GRADOS        *
*-------------------------------*
if "`lev1'"=="grado" | "`lev2'"=="grado" | "`lev3'"=="grado" | "`lev4'"=="grado" | "`lev5'"=="grado" | "`lev6'"=="grado" | "`lev7'"=="grado" | "`lev8'"=="grado"{
	gen gradolev=1-((grado_`year2'<=grado_`year1'+2) & (grado_`year2'>=grado_`year1'-1))
}

*-------------------------------*
*    INDICADOR DE GÉNERO        *
*-------------------------------*
if "`lev1'"=="genero" | "`lev2'"=="genero" | "`lev3'"=="genero" | "`lev4'"=="genero" | "`lev5'"=="genero" | "`lev6'"=="genero" | "`lev7'"=="genero" | "`lev8'"=="genero"{
	gen generolev=1-(genero_`year1'==genero_`year2')
}

*--------------------------------------------*
*    INDICADOR DE LUGAR DE NACIMIENTO        *
*--------------------------------------------*
if "`lev1'"=="nacimiento" | "`lev2'"=="nacimiento" | "`lev3'"=="nacimiento" | "`lev4'"=="nacimiento" | "`lev5'"=="nacimiento" | "`lev6'"=="nacimiento" | "`lev7'"=="nacimiento" | "`lev8'"=="nacimiento"{
	gen nacimientolev=1-((nac_mun_`year1'==nac_mun_`year2') & (nac_dpto_`year1'==nac_dpto_`year2'))
}

*--------------------------------------------*
*    INDICADOR DE LUGAR DE RESIDENCIA        *
*--------------------------------------------*
if "`lev1'"=="residencia" | "`lev2'"=="residencia" | "`lev3'"=="residencia" | "`lev4'"=="residencia" | "`lev5'"=="residencia" | "`lev6'"=="residencia" | "`lev7'"=="residencia" | "`lev8'"=="residencia"{
	gen residencialev=1-((res_mun_`year1'==res_mun_`year2') & (res_dpto_`year1'==res_dpto_`year2'))
}


*--------------------------------------------*
*    INDICADOR DIFERENCIA DE NOMBRES         *
*--------------------------------------------*
if "`lev1'"=="diferencia" | "`lev2'"=="diferencia" | "`lev3'"=="diferencia" | "`lev4'"=="diferencia" | "`lev5'"=="diferencia" | "`lev6'"=="diferencia" | "`lev7'"=="diferencia" | "`lev8'"=="diferencia"{
	forvalues p=1/2{
		forvalues q=1/2{
			levenshtein nombre`p'_`year1' nombre`q'_`year2', gen(nombre_`p'_`q')
			gen d1=length(nombre`p'_`year1')
			gen d2=length(nombre`q'_`year2')
			egen meann=rowmean(d1 d2)
			gen minn=min(d1, d2)
			
			replace nombre_`p'_`q'=nombre_`p'_`q'/minn
			
			gen sub1=strpos(nombre`q'_`year2',nombre`p'_`year1')
			gen sub2=strpos(nombre`p'_`year1',nombre`q'_`year2')
			
			replace nombre_`p'_`q'=0 if (sub1>0 | sub2>0) & (nombre`p'_`year1'!="" & nombre`q'_`year2'!="")
			drop sub1 sub2
			
			* Cuando el levenshtein es igual a la diferencia en longitudes, y la diferencia es pequeña, es porque se comieron letras.
			* Aquí arreglo eso
			gen diferencia_long=abs(d1 - d2)
			replace nombre_`p'_`q'=0 if (diferencia_long==nombre_`p'_`q') & (nombre`p'_`year1'!="" & nombre`q'_`year2'!="") & diferencia_long<=3
			drop diferencia_long d1 d2

			replace nombre_`p'_`q'=1-(nombre_`p'_`q'<=0.4) if minn>4
			replace nombre_`p'_`q'=1-(nombre_`p'_`q'<=0.5) if minn<=4
			drop min meann

		}
	}
	gen nome=min(nombre_1_1, nombre_1_2, nombre_2_1, nombre_2_2)
	drop nombre_1_1 nombre_1_2 nombre_2_1 nombre_2_2
	
	forvalues p=1/2{
		forvalues q=1/2{
			levenshtein apellido`p'_`year1' apellido`q'_`year2', gen(apellido_`p'_`q')
			gen d1=length(apellido`p'_`year1')
			gen d2=length(apellido`q'_`year2')
			egen meann=rowmean(d1 d2)
			gen minn=min(d1, d2)
			
			replace apellido_`p'_`q'=apellido_`p'_`q'/minn
			
			gen sub1=strpos(apellido`q'_`year2',apellido`p'_`year1')
			gen sub2=strpos(apellido`p'_`year1',apellido`q'_`year2')
			
			replace apellido_`p'_`q'=0 if (sub1>0 | sub2>0) & (apellido`p'_`year1'!="" & apellido`q'_`year2'!="")
			drop sub1 sub2
			
			* Cuando el levenshtein es igual a la diferencia en longitudes, y la diferencia es pequeña, es porque se comieron letras.
			* Aquí arreglo eso
			gen diferencia_long=abs(d1 - d2)
			replace apellido_`p'_`q'=0 if (diferencia_long==apellido_`p'_`q') & (apellido`p'_`year1'!="" & apellido`q'_`year2'!="") & diferencia_long<=3
			drop diferencia_long d1 d2
			
			replace apellido_`p'_`q'=1-(apellido_`p'_`q'<=0.4) if minn>4
			replace apellido_`p'_`q'=1-(apellido_`p'_`q'<=0.5) if minn<=4
			drop min meann

		}
	}
	gen apel=min(apellido_1_1, apellido_1_2, apellido_2_1, apellido_2_2)
	drop apellido_1_1 apellido_1_2 apellido_2_1 apellido_2_2
	
	gen suma=nome+apel
	drop nome apel
	gen diferencialev=1-(suma==0)
	drop suma
}


*-------------------------------*
*    INDICADOR COLEGIOS         *
*-------------------------------*

if "`lev1'"=="colegio" | "`lev2'"=="colegio" | "`lev3'"=="colegio" | "`lev4'"=="colegio" | "`lev5'"=="colegio" | "`lev6'"=="colegio" | "`lev7'"=="colegio" | "`lev8'"=="colegio"{
	gen colegiolev=1-(dane_sed_`year1'==dane_sed_`year2')
}

}

end


















