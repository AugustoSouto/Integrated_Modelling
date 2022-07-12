####outline de la lectura del output.hru###

#1- SWATplusR::run_swat2012
#1.1 define output, es de este formato por ej: 
#   SWATplusR:output<-define_output(file = "rch",
#                            variable = "FLOW_OUT",
#                           unit = c(1,5))
#1.2 check output saca info del output y pone un nombre nomas

#2-read_file_meta: te da un listado en formato df de todos los files
    #2.1 read_hru
      #2.1.1 get_hru_meta  saca la meta info del outpout.hru              
#2.2-read_swat2012_files le indica que leer (por ej, el output.hru)

#3: read_swat2012_output lee la salida del .hru
#3.1 usa get_file_header: esa funcion saca los nombres de las columnas de output.hru
#3.1.1 split_by_units: con esa funcion se arreglan las columnas. 
#Luego de arreglado eso se prosigue en la funcion read_swat2012_output


