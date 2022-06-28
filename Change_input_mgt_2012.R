rm(list = ls())

####INTRO####

path_tala <-
"/Users/augustosouto/Dropbox/Mac/Desktop/SWATala_junio/Modelo_intento2000_ROTACIONES_16_grass_agrl_cambio_clima_90_20_saco_Qmin_embalse/Embalse01/Scenarios/Default/TxtInOut/"

path_tala <-
"/Users/augustosouto/Dropbox/Mac/Desktop/SWATala/Modelo_intento2000_ROTACIONES/Embalse01/Scenarios/Default/TxtInOut/"

setwd(path_tala)

files_mgt <-
list.files(path = path_tala, pattern = ".mgt")  

n_hru <-
length(files_mgt)-1 ;n_hru #el ultimo mgt es el output.mgt que no va


####GET HRU_INFO####

luse <- vector(length = n_hru)
subbasin <- vector(length = n_hru)
hru_1 <- vector(length = n_hru) #identificador del hru en total
hru_2 <- vector(length = n_hru) #identificador del hru dentro del subbasin

for (mgt in 1:n_hru){ #el ultimo mgt es el output.mgt que no va
 
 file <- 
   files_mgt[mgt] |> readLines() #cambiar por el iterador
  
 luse_position <-
    file[1] |> stringr::str_locate("Luse:")

subbasin_position <-
    file[1] |> stringr::str_locate("Subbasin:")

hru_position <-
  file[1] |> stringr::str_locate("HRU:")
 
luse[mgt] <- 
  file[1] |> substr(luse_position[1]+5, luse_position[2]+4) 

subbasin[mgt] <- 
  file[1] |> substr(subbasin_position[2]+1, subbasin_position[2]+2) 

hru_1[mgt] <-
file[1] |> substr(hru_position[2]+1, hru_position[2]+3) 

hru_2[mgt] <-
  file[1] |> substr(luse_position[1]-2 , luse_position[1]-1) 

}


#hola

hru_1 <-
stringr::str_remove(hru_1, "S")

hru_info <-
cbind(luse, hru_1, subbasin, hru_2 )

land_uses <- luse |> unique() ; land_uses

#####LAND USES####

n_hru_soyb <-
which(luse=="SOYB") |> length(); n_hru_soyb

n_hru_past <-
which(luse=="PAST") |> length() ; n_hru_past

n_hru_agrl <-
which(luse=="AGRL") |> length() ; n_hru_agrl

n_hru_rice <-
which(luse=="RICE") |> length() ; n_hru_rice

n_hru_corn <-
which(luse=="CORN") |> length() ; n_hru_corn

####OP TABLES####

soyb_table<-
files_mgt[which(luse=="SOYB")[1]] |> readLines() #cambiar por el iterador
past_table<-
files_mgt[which(luse=="PAST")[1]] |> readLines() #cambiar por el iterador
agrl_table<-
files_mgt[which(luse=="AGRL")[1]] |> readLines() #cambiar por el iterador
rice_table<-
files_mgt[which(luse=="RICE")[1]] |> readLines() #cambiar por el iterador
corn_table<-
files_mgt[which(luse=="CORN")[1]] |> readLines() #cambiar por el iterador


#codigo operaciones (3er columna)
#1-planting
#2-irrigation
#3-fertilizer application
#4-pesticide application
#5-harvest and kill operation
#6-tillage operation
#7-harvest only operation
#8-kill/end of growing season
#9-grazing operation
#10-auto irrigation initialization
#11-auto fertilization initialization
#12-sweet sweeping operation
#13-release/impoung water
#14-continous fertilization
#15-continous pesticides
#0-end of year rotation flag
#17-end of op (According to Willem)

##parameters##
#1-planting parameters
#month, day, plant id, current age of trees, heat units to reach maturity, 
#init leaf area, harvest ind target, initial biomass, biomass target, scs runoff number

#5-harv & kill parameters
#not used

#8-kill parameters
#not used

#10-auto irr init patameter
#month, day, operation, water stress identifier (1oplant water demand, 2 soul water content),
#auto irr code (0 no irr, 1 divert water from reach, 2 from reservoir, 3 from shallow aquigier, 
# 4 from deep aquifier, 5 from unlimited source), 5 aut irr source location, 
#6 water stress threshold, irr_eff(0a100), irr_asq (surface runoff ratio)  


#wtstress 47 51, irr eff 55 63


#### CHANGE IRR_EFF####

#1-soybean#
n_row_soyb <-
soyb_table |> length()

soyb_management <- soyb_table[30:n_row_soyb]; soyb_management

n_row_soyb <-
soyb_management |> length()

#files mgt y hru info tienen que estar en la memoria para la funcion#
soyb_irr_eff <- function(soyb_file, irr_eff_1="50.000", irr_eff_2="50.000"){
  
  for (unit in 1:n_hru) {
    print(unit)
    file <- 
      files_mgt[unit] |> readLines() #cambiar por el iterador
    
    hru_luse=hru_info[,"luse"][unit]
    hru_1=hru_info[,"hru_1"][unit]
    hru_sub=hru_info[,"subbasin"][unit]
    if(hru_luse=="SOYB"){
      print("SOYB")
      if(file[29+4] |> substr(29+17, 29+18)=="10"){
        file[29+4]  |> substr(55, 29+60)=irr_eff_1}
      if(file[29+12] |> substr(17, 29+18)=="10"){
        file[29+12] |> substr(55, 29+60)=irr_eff_2}
    } 
    return(file)
  }  
  
write()
  
}
  
#operaciones: 10, 1, 10 riego, plant, riego
#             10, 5, 1 riego, harv & kill, plant
#             8, 1, 10 kill, plant, auto irr init


#2-past#
n_row_past <-
  past_table |> length()

past_table[30:n_row_past]

past_management <- past_table[30:n_row_past]; past_management

n_row_past <-
  past_management |> length()

past_irr_eff <- function(irr_eff="50.000"){

    for (unit in 1:n_hru) {
    print(unit)
    file <- 
      files_mgt[unit] |> readLines() #cambiar por el iterador
    
    hru_luse=hru_info[,"luse"][unit]
    hru_1=hru_info[,"hru_1"][unit]
    hru_sub=hru_info[,"subbasin"][unit]
    if(hru_luse=="PAST"){
      if(file[29+11] |> substr(29+17, 29+18)=="10"){
         file[29+11] |> substr(29+55, 29+60)=irr_eff}
                        }

  return(file)
    }
  }

irr_eff(soyb_file = soyb_management, irr_eff = "60.000")


#operaciones: 10, 5, 1 auto irr init, harv & kill, plant
#             8, 1, kill, plant
#             5, 1, 10 harv & kill, plant, riego

#3-agrl#
n_row_agrl <-
  agrl_table |> length()

agrl_table[30:n_row_agrl] #plant


#operaciones: 1

#4-rice#
n_row_rice <-
  rice_table |> length()

rice_table[30:n_row_rice]

rice_management <- rice_table[30:n_row_rice]; rice_management

rice_irr_eff <- function(irr_eff=" 50.000"){
  
  for (unit in 1:n_hru) {
    print(unit)
    file <- 
      files_mgt[unit] |> readLines() #cambiar por el iterador
    
    hru_luse=hru_info[,"luse"][unit]
    hru_1=hru_info[,"hru_1"][unit]
    hru_sub=hru_info[,"subbasin"][unit]
    if(hru_luse=="RICE"){
     if(file[29+4] |> substr(29+17, 29+18)=="10"){
        file[29+4]  |> substr(29+54, 29+60)=irr_eff}
                        }
    return(file)
}
}
#operaciones: 8, 1, 10 kill, plant, auto irr init
#             5, 10, 1 harv & kill, auto irr, plant
#             1,  plant

#5-corn#
n_row_corn <-
  corn_table |> length()

corn_table[30:n_row_corn]

corn_management <- corn_table[30:n_row_corn]; corn_management

corn_irr_eff <- function(irr_eff_1="50.000", irr_eff_2="50.000", irr_eff_3="50.000"){

  for (unit in 1:n_hru) {
    print(unit)
    file <- 
      files_mgt[unit] |> readLines() #cambiar por el iterador
    
    hru_luse=hru_info[,"luse"][unit]
    hru_1=hru_info[,"hru_1"][unit]
    hru_sub=hru_info[,"subbasin"][unit]
    if(hru_luse=="CORN"){  
      if(file[29+5]  |> substr(29+17, 29+18)=="10"){
         file[29+5]  |> substr(29+54, 29+60)=irr_eff_1}
      if(file[29+10] |> substr(29+17, 29+18)=="10"){
         file[29+4]  |> substr(29+54, 29+60)=irr_eff_2}
      if(file[29+15] |> substr(29+17, 29+18)=="10"){
         file[29+15] |> substr(29+54, 29+60)=irr_eff_3}
      }
      return(file)
}
}

#operaciones: 5, 10, 1, 10 harv & kill, auto irr init, plant, auto irr init
#             10, 5, 1, 10 auto irr init, harv & kill, plant, auto irr init
#             10, 5, 1, 10 auto irr init, harv & kill, plant, auto irr init


##ver plant id, 
##hacer cambio en irr eff
#ver escenarios de land use 
#ver tema de escenarios de clima

for (mgt in 1:n_hru){ #el ultimo mgt es el output.mgt que no va
  
  
  file <- 
    files_mgt[mgt] |> readLines() #cambiar por el iterador
  print(mgt)
  print(file[1])
}


#hacer un cambio de irr_eff para un uso dado#

hru_info

irr_eff_change<- function(irr_eff, luse, subbasin){
for (unit in 1:n_hru) {
  
  file <- 
    files_mgt[unit] |> readLines() #cambiar por el iterador
  
  hru_luse=hru_info[,"luse"][unit]
  hru_1=hru_info[,"hru_1"][unit]
  hru_sub=hru_info[,"subbasin"][unit]
  if(hru_luse==luse){cambio}
  
}

}
  

for (mgt in 1:n_hru){ #el ultimo mgt es el output.mgt que no va
  
  file <- 
    files_mgt[mgt] |> readLines() #cambiar por el iterador
  
  
  

}

