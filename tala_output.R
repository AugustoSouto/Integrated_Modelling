rm(list = ls())

path<-  
"C:/Users/Augusto/Desktop/SWATala_junio/Modelo_intento2000_ROTACIONES_16_grass_agrl_cambio_clima_90_20_saco_Qmin_embalse/Embalse01/Scenarios/Default/TxtInOut/"

project_path <-
  "C:/Users/Augusto/Desktop/Git/Integrated_Modelling/"

setwd(path)


library(SWATplusR)

salida<-
  run_swat2012(project_path = path, 
               output = list(LULC = define_output(file = 'hru',
                                                  variable = 'LULC',
                                                  unit = 1:150),
                             SUB =  define_output(file = 'hru',
                                                  variable = 'SUB',
                                                  unit = 1:150),
                             IRR = define_output(file = "hru",
                                                 variable = "IRRmm",
                                                 unit = 1:150),
                             Yield = define_output(file = "hru",
                                                   variable = "YLDt/ha",
                                                   unit = 1:150)))

#sacar p y no3
#ponerle el area adjuntando
#ponerle el luse 


#salida %>% View()


lulc <-
salida[ grepl("^(LULC|date)", colnames(salida)) ] |> tidyr::gather(unit, LULC, -c(date)) |>
  dplyr::mutate(unit=stringr::str_remove(unit,"LULC_"))

sub <-
  salida[ grepl("^(SUB|date)", colnames(salida)) ] |> tidyr::gather(unit, SUB, -c(date)) |>
  dplyr::mutate(unit=stringr::str_remove(unit,"SUB_"))

irr <-
  salida[ grepl("^(IRR|date)", colnames(salida)) ] |> tidyr::gather(unit, IRR, -c(date)) |>
  dplyr::mutate(unit=stringr::str_remove(unit,"IRR_"))

yld <-
  salida[ grepl("^(Yield|date)", colnames(salida)) ] |> tidyr::gather(unit, YLD, -c(date)) |>
  dplyr::mutate(unit=stringr::str_remove(unit,"Yield_"))


#put all data frames into list
lista <- list(lulc,sub, irr, yld)

#merge all data frames in list
salida <-
  Reduce(function(x, y) merge(x, y, all=TRUE), lista)



#mat <-
#matrix(data=c(1,2,3,4), ncol=2)

#lista<-
#replicate(5, mat, simplify=FALSE)
#Reduce(function(x, y) rbind(x,y), lista)




output_ambiental <-
  run_swat2012(project_path = path,
                     output = list(flow = define_output(file = "rch",
                                                        variable = "FLOW_OUT",
                                                        unit = 26),
                                   flow1 = define_output(file = "rch",
                                                         variable = "FLOW_OUT",
                                                         unit = 22),
                                   flow2 = define_output(file = "rch",
                                                         variable = "FLOW_OUT",
                                                         unit = 23),
                                   flow3 = define_output(file = "rch",
                                                         variable = "FLOW_OUT",
                                                         unit = 24),
                                   flow4 = define_output(file = "rch",
                                                         variable = "FLOW_OUT",
                                                         unit = 25),
                                   PT = define_output(file = "rch",
                                                      variable = "TOT_Pkg",
                                                      unit = 26),
                                   NO3 = define_output(file = "rch",
                                                       variable = "NO3_OUT",
                                                       unit = 26),
                                   SED_OUT = define_output(file = "rch",
                                                           variable = "SED_OUT",
                                                           unit = 26),
                                   SEDCONC = define_output(file = "rch",
                                                           variable = "SEDCONC",
                                                           unit = 26),
                                   SEDCONC1 = define_output(file = "rch",
                                                            variable = "SEDCONC",
                                                            unit = 10)))
                     
                     
setwd(project_path)
rm(irr, lista, lulc, sub, yld)
save.image(paste0("swat_output.RData"))


