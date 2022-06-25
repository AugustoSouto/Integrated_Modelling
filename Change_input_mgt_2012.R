rm(list = ls())

path_tala <-
"/Users/augustosouto/Dropbox/Mac/Desktop/SWATala/Modelo_intento2000_ROTACIONES/Embalse01/Scenarios/Default/TxtInOut/"

setwd(path_tala)

files_mgt <-
list.files(path = path_tala, pattern = ".mgt")  

n_hru <-
length(files_mgt)-1 ;n_hru #el ultimo mgt es el output.mgt que no va

luse <- vector(length = n_hru)

for (mgt in 1:n_hru){ #el ultimo mgt es el output.mgt que no va
 
 file <- 
   files_mgt[mgt] |> readLines() #cambiar por el iterador
  
 luse_position <-
    file[1] |> stringr::str_locate("Luse:")

luse[mgt] <- 
  file[1] |> substr(luse_position[1]+5, luse_position[2]+4) 

}

land_uses <- luse |> unique() ; land_uses

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

#1-soybean#
n_row <-
soyb_table |> length()

soyb_table[30:n_row]

#operaciones: 10, 1, 10 riego, plant, riego
#             10, 5, 1 riego, harv & kill, plant
#             8, 1, 10 kill, plant, auto irr init


#2-past#
n_row <-
  past_table |> length()

past_table[30:n_row]

#operaciones: 10, 5, 1 auto irr init, harv & kill, plant
#             8, 1, kill, plant
#             5, 1, 10 harv & kill, plant, riego

#3-agrl#
n_row <-
  agrl_table |> length()

agrl_table[30:n_row] #plant


#operaciones: 1

#4-rice#
n_row <-
  rice_table |> length()

rice_table[30:n_row]

#operaciones: 8, 1, 10 kill, plant, auto irr init
#             5, 10, 1 harv & kill, auto irr, plant
#             1,  plant

#5-corn#
n_row <-
  corn_table |> length()

corn_table[30:n_row]

#operaciones: 5, 10, 1, 10 harv & kill, auto irr init, plant, auto irr init
#             10, 5, 1, 10 auto irr init, harv & kill, plant, auto irr init
#             10, 5, 1, 10 auto irr init, harv & kill, plant, auto irr init


##ver plant id, 
##hacer cambio en irr eff
f#ver escenarios de land use 
#ver tema de escenarios de clima



