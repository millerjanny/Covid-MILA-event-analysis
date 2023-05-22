***************Análisis Estudio de Eventos COVID
use "C:finaldataset_IPSA.dta", clear

generate nombre_evento=cond(EVENT_DATE==td(11/03/2020),1, cond(EVENT_DATE==td(18/03/2020),2,3))

label define nombre_evento 1 "OMS DECLARA COVID" 2 "PRIMER CONFINAMIENTO" 3 "PRIMER DÍA VACUNACIÓN"
label values nombre_evento nombre_evento

sort group_id DATE
by group_id: gen datenum=_n
by group_id: gen target=datenum if DATE== EVENT_DATE

egen td=min(target), by( group_id )
drop target
gen dif=datenum-td
by group_id: gen event_window=1 if dif>=-10 & dif<=10
egen count_event_obs=count( event_window), by( group_id )
by group_id: gen estimation_window=1 if dif<-30 & dif>=-280
egen count_est_obs=count( estimation_window), by( group_id )
replace event_window=0 if event_window==.
replace estimation_window=0 if estimation_window==.
tab group_id if count_event_obs<5
tab group_id if count_est_obs<30
drop if count_event_obs<5
drop if count_est_obs<30

gen predicted_return=.
egen id=group( group_id )
egen id2=group( COMPANY_ID )
 encode COMPANY_ID, generate(Company_id)

gen retorno_anormal_estimacion=.

gen retorno_anormal_evento=.

gen tiempo=_n
tsset tiempo


MODELO TEORICO PARA CALCULAR RETORNO ANORMALES V1/V2 Y ARCH-M
*****************************************************************************************************************
*********************************************VOLATILIDAD ARCH EN MEDIA*********************************************************
************************EVENTO 1

arch RET MARKET_RETURN V1 V2 if Company_id==1 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==1, residuals dynamic(.)
replace retorno_anormal_evento=r if Company_id==1 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==1 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_1  


arch RET MARKET_RETURN V1 V2 if Company_id==2 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==2, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==2 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==2 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_2  

arch RET MARKET_RETURN V1 V2 if Company_id==3 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==3, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==3 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==3 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_3  

arch RET MARKET_RETURN V1 V2 if Company_id==4 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==5, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==4 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==4 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_4  

arch RET MARKET_RETURN V1 V2 if Company_id==5 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==5, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==5 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==5 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_5  

arch RET MARKET_RETURN V1 V2 if Company_id==6 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==6, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==6 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==6 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_6  

arch RET MARKET_RETURN V1 V2 if Company_id==7 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==7, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==7 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==7 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_7  

arch RET MARKET_RETURN V1 V2 if Company_id==8 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==8, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==8 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==8 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_8  

arch RET MARKET_RETURN V1 V2 if Company_id==9 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==9, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==9 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==9 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_9  

arch RET MARKET_RETURN V1 V2 if Company_id==10 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==10, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==10 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==10 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_10 

arch RET MARKET_RETURN V1 V2 if Company_id==11 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==11, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==11 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==11 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_11 

arch RET MARKET_RETURN V1 V2 if Company_id==12 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==12, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==12 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==12 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_12 

arch RET MARKET_RETURN V1 V2 if Company_id==13 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==13, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==13 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==13 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_13 

arch RET MARKET_RETURN V1 V2 if Company_id==14 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==14, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==14 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==14 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_14 

arch RET MARKET_RETURN V1 V2 if Company_id==15 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==15, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==15 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==15 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_15 

arch RET MARKET_RETURN V1 V2 if Company_id==16 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==16, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==16 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==16 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_16 

arch RET MARKET_RETURN V1 V2 if Company_id==17 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==17, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==17 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==17 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_17 

arch RET MARKET_RETURN V1 V2 if Company_id==18 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==18, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==18 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==18 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_18 

arch RET MARKET_RETURN V1 V2 if Company_id==19 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==19, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==19 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==19 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_19 

arch RET MARKET_RETURN V1 V2 if Company_id==20 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==20, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==20 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==20 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_20 

arch RET MARKET_RETURN V1 V2 if Company_id==21 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==21, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==21 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==21 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_21 

arch RET MARKET_RETURN V1 V2 if Company_id==22 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==22, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==22 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==22 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_22 

arch RET MARKET_RETURN V1 V2 if Company_id==23 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==23, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==23 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==23 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_23 

arch RET MARKET_RETURN V1 V2 if Company_id==24 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==24, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==24 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==24 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_24  


arch RET MARKET_RETURN V1 V2 if Company_id==25 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==25, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==25 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==25 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_25  

arch RET MARKET_RETURN V1 V2 if Company_id==26 & estimation_window==1 & nombre_evento==1, arch(1/1) archm nolog
predict r if Company_id==26, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==26 & event_window==1 & nombre_evento==1
replace retorno_anormal_estimacion=r if Company_id==26 & estimation_window==1 & nombre_evento==1
drop r
estimate store Modelo1_26



*************************************EVENTO 2**********************************************************
arch RET MARKET_RETURN V1 V2 if Company_id==1 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==1, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==1 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==1 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_1  


arch RET MARKET_RETURN V1 V2 if Company_id==2 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==2, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==2 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==2 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_2  

arch RET MARKET_RETURN V1 V2 if Company_id==3 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==3, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==3 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==3 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_3  

arch RET MARKET_RETURN V1 V2 if Company_id==4 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==5, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==4 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==4 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_4  

arch RET MARKET_RETURN V1 V2 if Company_id==5 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==5, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==5 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==5 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_5  

arch RET MARKET_RETURN V1 V2 if Company_id==6 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==6, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==6 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==6 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_6  

arch RET MARKET_RETURN V1 V2 if Company_id==7 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==7, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==7 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==7 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_7  

arch RET MARKET_RETURN V1 V2 if Company_id==8 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==8, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==8 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==8 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_8  

arch RET MARKET_RETURN V1 V2 if Company_id==9 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==9, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==9 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==9 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_9  

arch RET MARKET_RETURN V1 V2 if Company_id==10 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==10, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==10 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==10 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_10 

arch RET MARKET_RETURN V1 V2 if Company_id==11 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==11, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==11 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==11 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_11 

arch RET MARKET_RETURN V1 V2 if Company_id==12 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==12, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==12 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==12 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_12 

arch RET MARKET_RETURN V1 V2 if Company_id==13 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==13, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==13 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==13 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_13 

arch RET MARKET_RETURN V1 V2 if Company_id==14 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==14, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==14 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==14 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_14 

arch RET MARKET_RETURN V1 V2 if Company_id==15 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==15, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==15 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==15 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_15 

arch RET MARKET_RETURN V1 V2 if Company_id==16 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==5, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==16 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==16 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_16 

arch RET MARKET_RETURN V1 V2 if Company_id==17 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==5, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==17 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==17 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_17 

arch RET MARKET_RETURN V1 V2 if Company_id==18 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==18, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==18 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==18 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_18 

arch RET MARKET_RETURN V1 V2 if Company_id==19 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==19, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==19 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==19 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_19 

arch RET MARKET_RETURN V1 V2 if Company_id==20 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==20, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==20 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==20 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_20 

arch RET MARKET_RETURN V1 V2 if Company_id==21 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==21, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==21 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==21 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_21 

arch RET MARKET_RETURN V1 V2 if Company_id==22 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==22, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==22 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==22 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_22 

arch RET MARKET_RETURN V1 V2 if Company_id==23 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==23, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==23 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==23 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_23 

arch RET MARKET_RETURN V1 V2 if Company_id==24 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==24, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==24 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==24 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_24  

arch RET MARKET_RETURN V1 V2 if Company_id==25 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==25, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==25 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==25 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_25  


arch RET MARKET_RETURN V1 V2 if Company_id==26 & estimation_window==1 & nombre_evento==2, arch(1/1) archm nolog
predict r if Company_id==26, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==26 & event_window==1 & nombre_evento==2
replace retorno_anormal_estimacion=r if Company_id==26 & estimation_window==1 & nombre_evento==2
drop r
estimate store Modelo2_26

********************************EVENTO 3******************************************************


regress RET MARKET_RETURN V1 V2 if Company_id==1 & estimation_window==1 & nombre_evento==3
predict r if Company_id==1, residuals 
replace retorno_anormal_evento=r if Company_id==1 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==1 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_1  

arch RET MARKET_RETURN V1 V2 if Company_id==2 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==2, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==2 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==2 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_2  
 
arch RET MARKET_RETURN V1 V2 if Company_id==3 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==3, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==3 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==3 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_3  

arch RET MARKET_RETURN V1 V2 if Company_id==4 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==4, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==4 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==4 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_4  

arch RET MARKET_RETURN V1 V2 if Company_id==5 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==5, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==5 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==5 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_5   

arch RET MARKET_RETURN V1 V2 if Company_id==6 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==6, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==6 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==6 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_6  

arch RET MARKET_RETURN V1 V2 if Company_id==7 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==7, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==7 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==7 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_7  

arch RET MARKET_RETURN V1 V2 if Company_id==8 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==8, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==8 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==8 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_8  

arch RET MARKET_RETURN V1 V2 if Company_id==9 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==9, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==9 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==9 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_9  

arch RET MARKET_RETURN V1 V2 if Company_id==10 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==10, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==10 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==10 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_10 

arch RET MARKET_RETURN V1 V2 if Company_id==11 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==11, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==11 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==11 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_11 

arch RET MARKET_RETURN V1 V2 if Company_id==12 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==12, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==12 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==12 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_12 

arch RET MARKET_RETURN V1 V2 if Company_id==13 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==13, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==13 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==13 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_13 

arch RET MARKET_RETURN V1 V2 if Company_id==14 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==14, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==14 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==14 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_14 

arch RET MARKET_RETURN V1 V2 if Company_id==15 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==15, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==15 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==15 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_15 

arch RET MARKET_RETURN V1 V2 if Company_id==16 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==16, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==16 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==16 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_16 

arch RET MARKET_RETURN V1 V2 if Company_id==17 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==5, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==17 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==17 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_17 

arch RET MARKET_RETURN V1 V2 if Company_id==18 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==18, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==18 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==18 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_18 

arch RET MARKET_RETURN V1 V2 if Company_id==19 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==19, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==19 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==19 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_19 

arch RET MARKET_RETURN V1 V2 if Company_id==20 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==20, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==20 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==20 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_20 

arch RET MARKET_RETURN V1 V2 if Company_id==21 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==21, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==21 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==21 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_21 

arch RET MARKET_RETURN V1 V2 if Company_id==22 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==22, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==22 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==22 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_22 

arch RET MARKET_RETURN V1 V2 if Company_id==23 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==23, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==23 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==23 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_23 


arch RET MARKET_RETURN V1 V2 if Company_id==24 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog 
predict r if Company_id==24, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==24 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==24 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_24  

arch RET MARKET_RETURN V1 V2 if Company_id==25 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==25, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==25 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==25 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_25  


arch RET MARKET_RETURN V1 V2 if Company_id==26 & estimation_window==1 & nombre_evento==3, arch(1/1) archm nolog
predict r if Company_id==26, residuals dynamic(.) 
replace retorno_anormal_evento=r if Company_id==26 & event_window==1 & nombre_evento==3
replace retorno_anormal_estimacion=r if Company_id==26 & estimation_window==1 & nombre_evento==3
drop r
estimate store Modelo3_26

***************EXPORTAR RESULTADOS WORD
etable, estimates(Modelo1_3 Modelo1_14 Modelo1_15 Modelo1_19 Modelo1_21) column(dvlabel)                        ///
        cstat(_r_b, nformat(%4.2f))                                   ///
        showstars showstarsnote                                       /// 
        stars(.10 "*" .05 "**" .01 "***", attach(_r_b))              ///
        mstat(N,   nformat(%8.0fc) label("Observations"))             ///
        title("Example Regression Model For NR")      ///
        titlestyles(font(Lucida Console, size(14) bold))              ///
        notestyles(font(Lucida Console, size(10) italic))             ///  
        export(IPSA1.docx, replace) 
 
 etable, estimates(Modelo2_3 Modelo2_14 Modelo2_15 Modelo2_19 Modelo2_21) ///
 column(dvlabel)                        ///
        cstat(_r_b, nformat(%4.2f))                                   ///
        showstars showstarsnote                                       /// 
        stars(.10 "*" .05 "**" .01 "***", attach(_r_b))              ///
        mstat(N,   nformat(%8.0fc) label("Observations"))             ///
        title("Example Regression Model For NR")      ///
        titlestyles(font(Lucida Console, size(14) bold))              ///
        notestyles(font(Lucida Console, size(10) italic))             ///  
        export(IPSA2.docx, replace) 
  
etable, estimates(Modelo3_3 Modelo3_14 Modelo3_15 Modelo3_19 Modelo3_21) column(dvlabel)                        ///
        cstat(_r_b, nformat(%4.2f))                                   ///
        showstars showstarsnote                                       /// 
        stars(.10 "*" .05 "**" .01 "***", attach(_r_b))              ///
        mstat(N,   nformat(%8.0fc) label("Observations"))             ///
        title("Example Regression Model For NR")      ///
        titlestyles(font(Lucida Console, size(14) bold))              ///
        notestyles(font(Lucida Console, size(10) italic))             ///  
        export(IPSA3.docx, replace) 


*EXTRAER LA DATA DE LA VENTANA DEL EVENTO****
snapshot save, label("datacompleta")
keep if event_window ==1
keep COMPANY_ID nombre_evento dif retorno_anormal_evento 
tostring dif, replace
replace dif= subinstr(dif, "-", "_",.)
reshape wide retorno_anormal_evento, i(COMPANY_ID nombre_evento) j(dif) string
order COMPANY_ID nombre_evento retorno_anormal_evento_10 retorno_anormal_evento_9 retorno_anormal_evento_8 retorno_anormal_evento_7 retorno_anormal_evento_6 retorno_anormal_evento_5 retorno_anormal_evento_4 retorno_anormal_evento_3 retorno_anormal_evento_2 retorno_anormal_evento_1 retorno_anormal_evento0 retorno_anormal_evento1 retorno_anormal_evento2 retorno_anormal_evento3 retorno_anormal_evento4 retorno_anormal_evento5 retorno_anormal_evento6 retorno_anormal_evento7 retorno_anormal_evento8 retorno_anormal_evento9 retorno_anormal_evento10

export excel using "C:Estudio_eventosIPSA.xlsx", sheet("eventoscompletos") firstrow(variables)

export excel using "C:Estudio_eventosIPSA.xlsx" if nombre_evento==1, sheet("OMS Declara COVID") firstrow(variables)

export excel using "C:Estudio_eventosIPSA.xlsx" if nombre_evento==2, sheet("Primer Confinamiento") firstrow(variables)

export excel using "C:Estudio_eventosIPSA.xlsx" if nombre_evento==3, sheet("Primer día Vacunación") firstrow(variables)

*Exportación de la varianza de cada activo, cada evento en la ventana de estimación

*Exportación de resultados de cada modelo
snapshot restore 1
outreg2 [Modelo*] using resultadosmodeloIPSA.xls, replace

bys group_id: egen sd1_abnormal_return=sd(retorno_anormal_estimacion)
generate sd_abnormal_return=sd1_abnormal_return if estimation_window==1
generate est_Var_Ar=sd_abnormal_return^2

collapse (mean) est_Var_Ar, by(COMPANY_ID nombre_evento)
export excel using "C:Estudio_eventosIPSA.xlsx", sheet("Estat_var_AR") firstrow(variables) 













