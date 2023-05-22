import excel "C:Estudio_eventosMexbol.xlsx", sheet("GRAFICOS (2)") cellrange(X1:AY22) firstrow
tsset Tiempo
foreach variable of varlist CAARFE- clusteringconfLímiteInferior99 {
replace `variable'=`variable'*100
}

twoway (tsline CAARFE) (tsrline confLímiteSuperior99FE confLímiteInferior99FE, recast(rarea) fcolor(%10) lpattern(dash)) (tsrline confLímiteSuperior95FE confLímiteInferior95FE, recast(rarea) fcolor(%30) lpattern(dash)), ytitle(CAAR (%)) ytitle(, size(small)) ylabel(#10, labels labsize(vsmall) angle(horizontal) format(%9.1f)) ttitle(Event Window (Days)) ttitle(, size(small)) tlabel(#21, labsize(small)) legend(order(1 "CAAR First Event" 2 "99% Confidence" 3 "95% Confidence") cols(1) size(small) box fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))

graph export "C:Mexico1.pdf", as(jpg) name("Graph") quality(100) replace

twoway (tsline CAARSE) (tsrline confLímiteSuperior99SE confLímiteInferior99SE, recast(rarea) fcolor(%10) lpattern(dash)) (tsrline confLímiteSuperior95SE confLímiteInferior95SE, recast(rarea) fcolor(%30) lpattern(dash)), ytitle(CAAR (%)) ytitle(, size(small)) ylabel(#10, labels labsize(vsmall) angle(horizontal) format(%9.1f)) ttitle(Event Window (Days)) ttitle(, size(small)) tlabel(#21, labsize(small))  legend(order(1 "CAAR Second Event" 2 "99% Confidence" 3 "95% Confidence") cols(1) size(small) box fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))

graph export "C:Mexico2.pdf", as(jpg) name("Graph") quality(100) replace

twoway (tsline CAARTE) (tsrline confLímiteSuperior99TE confLímiteInferior99TE, recast(rarea) fcolor(%10) lpattern(dash)) (tsrline confLímiteSuperior95TE confLímiteInferior95TE, recast(rarea) fcolor(%30) lpattern(dash)), ytitle(CAAR (%)) ytitle(, size(small)) ylabel(#10, labels labsize(vsmall) angle(horizontal) format(%9.1f)) ttitle(Event Window (Days)) ttitle(, size(small)) tlabel(#21, labsize(small)) legend(order(1 "CAAR Third Event" 2 "99% Confidence" 3 "95% Confidence") cols(1) size(small) box fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))

graph export "C:Mexico3.pdf", as(jpg) name("Graph") quality(100) replace
clear
