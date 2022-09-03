import delimited "C:\Users\PC\Documents\MEGAsync\2022-09-15_atanasova_bam\dat.csv", numericcols(12) clear 

gen admt_day =date( admit_day ,"YMD")
drop admit_day

gen death_dat = date( death_date ,"YMD")
drop death_date

gen dsch_day = date( disch_day ,"YMD")
drop disch_day

format admt_day %td
format death_dat %td
format dsch_day %td

encode sex, gen(gender)
drop sex

encode variant_type, gen (c_variant)


encode code, gen(c_code)

gen death = 0
replace death = 1 if outocome == "Починал"


generate age_p = real(age)
drop age

gen delta = 0
replace delta = 1 if variant_type == "Delta"

keep if variant_type == "Delta" | variant_type == "Omicron" 

gen hospitalisation = dsch_day - admt_day

logistic death  i.c_variant#c.age#c.gender
margins h.c_variant
margins g.c_variant

contrast gw.c_variant, effects nowald

margins c_variant, over(gender) at(age=(5(5)95))

**# Графика с две оси - хоспитализирани и починали #1
* Импортиране на данните по дни


import delimited "C:\Users\PC\Documents\GitHub\2022-09-15_atanasova_bam\2022-09-15_atanasova_bam\data\all_patients_by_date.csv", numericcols(12) clear 

gen admt_day =date( admit_day ,"YMD")
drop admit_day

format admt_day %td
tsset admt_day

graph twoway (tsline deaths , yaxis(1)) (tsline total ,yaxis(2)) 






