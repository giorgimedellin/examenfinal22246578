


# instalar packages y libraries -------------------------------------------

install.packages("pacman")
install.packages("httr")
install.packages("tidyverse")
install.packages("janitor")
install.packages("jsonlite")
install.packages("leaflet")
library("tidyverse")
library("pacman")
p_load("leaflet")


# importar datos ----------------------------------------------------------

#importar datos usando el URL del pagina web de gasolineras de España
url_ <- "https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/"
httr::GET(url_)


# A -----------------------------------------------------------------------
# i -----------------------------------------------------------------------

#aplicar jsonlite para trabajar con los datos
ds<-jsonlite::fromJSON(url_)

#limpiar los datos para que este formateada bien
ds_f <- ds_raw<-ds$ListaEESSPrecio %>% janitor::clean_names() %>% type_convert(locale = locale(decimal_mark = ",")) %>% as_tibble()


# ii ----------------------------------------------------------------------

#La razon por las anamolias son por problemas presupuestarias.


# iii ---------------------------------------------------------------------

#crear una columna que se llama low-cost definiando las gasolineras que no son low-cost gasolineras
no_low_cost <- c("REPSOL", "CEPSA", "GALP", "SHELL", "BP", "PETRONOR", "AVIA", "Q8", "CAMPA", "BONAREA")

#crear una tabla que se llama ds_low_cost que muestra los datos con la nueva columna
ds_low_cost <- ds_f %>% mutate(low_cost = !rotulo %in% no_low_cost) %>% view()

#contar cuantas gasolineras son low-cost y que no lo son. 6,171 son low cost y 5,693 no lo son
ds_low_cost %>% count(low_cost)

#usando la tabla que muestra low-cost/no-low-cost, calcular los precios promedios de todos tipos de combustibles, ordenado de los comunidades autonomas
precio_promedio_ccaa <- ds_low_cost %>% select(precio_gasoleo_a,precio_gasolina_95_e5,
                                               precio_gas_natural_comprimido,precio_gases_licuados_del_petroleo,
                                               precio_hidrogeno,precio_gasolina_98_e5,precio_gasolina_98_e10,
                                               precio_gasolina_95_e10,precio_gasolina_95_e5_premium,
                                               precio_gasoleo_premium,precio_gasoleo_b,precio_gas_natural_licuado,
                                               precio_bioetanol,precio_biodiesel,idccaa,rotulo)  %>% group_by(idccaa)
summarise(precio_promedio_ccaa, gasoleo_a = mean(precio_gasoleo_a, na.rm = TRUE),
            gasolina_95_e5 = mean(precio_gasolina_95_e5, na.rm = TRUE),
            gas_natural_comprimido = mean(precio_gas_natural_comprimido, na.rm = TRUE),
            gases_licuados_del_petroleo = mean(precio_gases_licuados_del_petroleo, na.rm = TRUE),
            hidrogeno = mean(precio_hidrogeno, na.rm = TRUE),
            gasolina_98_e5 = mean(precio_gasolina_98_e5, na.rm = TRUE),
            gasolina_98_e10 = mean(precio_gasolina_98_e10, na.rm = TRUE),
            gasolina_95_e10 = mean(precio_gasolina_95_e10, na.rm = TRUE),
            gasolina_95_e5_premium = mean(precio_gasolina_95_e5_premium, na.rm = TRUE),
            gasoleo_premium = mean(precio_gasoleo_premium, na.rm = TRUE),
            gasoleo_b = mean(precio_gasoleo_b, na.rm = TRUE),
            gas_natural_licuado = mean(precio_gas_natural_licuado, na.rm = TRUE),
            bioetanol = mean(precio_bioetanol, na.rm = TRUE),
            biodiesel = mean(precio_biodiesel, na.rm = TRUE)
          ) %>% view()

#crear una tabla excel_csv para mostrar los resultados
write_excel_csv(precio_promedio_ccaa,"promedios_por_ccaa.xls") 


# iv --------------------------------------------------------------------

#crear la tabla mas enfocada para usar en esta pregunta
top_ten_data <- ds_f %>% select(c_p,direccion,latitud,longitud_wgs84,precio_gasoleo_a,rotulo)

#mostrar las 10 gasolineras mas caras para gasoleo_a  
top_10 <- top_ten_data[order(desc(top_ten_data$precio_gasoleo_a)), ] %>% head(10) %>% view()

#renombrar el column de el precio para tener columnas unicas cuando las junto en el proximo paso
r_top_10 <- rename(top_10, top_cara = precio_gasoleo_a) %>% view()

#mostrar las 20 gasolineras mas baratas para gasoleo_a. 
top_20 <- top_ten_data[order(top_ten_data$precio_gasoleo_a), ] %>% head(20) %>% view()

#renombrar el column de el precio para tener columnas unicas cuando las junto en el proximo paso
r_top_20 <- rename(top_20, top_barato = precio_gasoleo_a) %>% view()

#crear un mapa para las 10 mas caras
top_10 %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84,lat = ~latitud)

#crear un mapa para las 20 mas baratas
top_20 %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84,lat = ~latitud)


# v -----------------------------------------------------------------------

#juntar el top_10 y top_20 en una tabla
lowcost_num_expediente <- full_join(r_top_10,r_top_20) %>% view()

#crear un excel con la ultima tabla para exportar
write_excel_csv(lowcost_num_expediente, "lowcost_num_expediente.xls")


# B -----------------------------------------------------------------------
# i -----------------------------------------------------------------------

#Murcia tiene 455 gasolineras.
ds_low_cost %>% filter(idccaa == "14") %>% count()
 
#Andalucia tiene 2,302 gasolineras
ds_low_cost %>% filter(idccaa == "01") %>% count()

#De las gasolineras de Madrid, 190 son low cost y 265 no lo son.
ds_low_cost %>% filter(idccaa == "14") %>% count(low_cost)

#De las gasolineras de Cataluña, 996 son low cost y 1,306 no lo son.
ds_low_cost %>% filter(idccaa == "01") %>% count(low_cost)



# ii ----------------------------------------------------------------------

#GASOLEO_A

#seleccionar el precio de gasoleo_a por idccaa
gas_a <- ds_f %>% select(precio_gasoleo_a,idccaa) %>% group_by(idccaa)%>% view()

#mostrar lo minimo, maximo, y promedio precios para gasoleo_A para los municipios: Murcia y Andalucia.
gasoleo_a <- summarise(gas_a, gasoleo_a_min = min(precio_gasoleo_a, na.rm = TRUE),
          gasoleo_a_max = max(precio_gasoleo_a, na.rm = TRUE),
          gasoleo_a_promedio = mean(precio_gasoleo_a, na.rm = TRUE)) %>% filter(idccaa=="14"| idccaa=="01") %>% view()

#GASOLINA_95_E5_PREMIUM

#seleccionar el precio de gasolina_95_e5_premium por idccaa
gas_95e5premium <- ds_f %>% select(precio_gasolina_95_e5_premium,idccaa) %>% group_by(idccaa)%>% view()

#mostrar lo minimo, maximo, y promedio precios para gasolina_95_e5_premium para los municipios: Madrid y Barcelona.
gasolina_95_e5_premium <- summarise(gas_95e5premium, precio_gasolina_95_e5_premium_min = min(precio_gasolina_95_e5_premium, na.rm = TRUE),
          precio_gasolina_95_e5_premium_max = max(precio_gasolina_95_e5_premium, na.rm = TRUE),
          precio_gasolina_95_e5_premium_promedio = mean(precio_gasolina_95_e5_premium, na.rm = TRUE)) %>% filter(idccaa=="14"| idccaa=="01") %>% view()


# iii ---------------------------------------------------------------------

#juntar todos los datos de parte bii para mostrar los precios mas bajos, caros, y promedios de gasoleo_a y gasolina_95_e5_premium en una tabla
informe_MUR_AND_expediente <- full_join(gasoleo_a, gasolina_95_e5_premium) %>% view()

#crear una tabla/archivo de excel con esa tabla
write_csv(informe_MUR_AND_expediente, "informe_MUR_AND_expediente")

# C -----------------------------------------------------------------------
# i -----------------------------------------------------------------------

#crear una tabla para mostrar todos municipios y cuantos low cost y no low cost tiene cada uno
ds_low_cost %>% group_by(municipio) %>% count(low_cost) %>% view()

#crear una tabla para mostrar los precios mas bajos de gasoleo_a y gasolina_95_e5_premium entre todos los municipios exceptuando 
#las ciudades(Madrid, Barcelona, Sevilla, y Valencia) usando los codigos de iccaa para filtrar. 
precio_min_ci <- ds_f %>% select(precio_gasoleo_a, precio_gasolina_95_e5_premium, municipio, idccaa) %>% 
  arrange(precio_gasoleo_a, by_group = FALSE) %>% arrange(precio_gasolina_95_e5_premium, by_group = FALSE) %>% 
  drop_na() %>% filter(municipio != "%Sevilla%") %>% filter(municipio != "%Madrid%") %>% filter(municipio != "%Barcelona%") %>% filter(municipio != "%Valencia%") %>% 
  head(precio_gasolina_95_e5_premium, n = 1) %>% head(precio_gasoleo_a, n = 1) %>% view()

#crear una tabla para mostrar los precios mas caras de gasoleo_a y gasolina_95_e5_premium entre todos los municipios exceptuando 
#las ciudades(Madrid, Barcelona, Sevilla, y Valencia) usando los codigos de iccaa para filtrar.
precio_max_ci <- ds_f %>% select(precio_gasoleo_a, precio_gasolina_95_e5_premium, municipio, idccaa) %>% 
  arrange(desc(precio_gasoleo_a), by_group = FALSE) %>% arrange(desc(precio_gasolina_95_e5_premium), by_group = FALSE) %>% 
  drop_na() %>% filter(municipio != "%Sevilla%") %>% filter(municipio != "%Madrid%") %>% filter(municipio != "%Barcelona%") %>% filter(municipio != "%Valencia%") %>% 
  head(precio_gasolina_95_e5_premium, n = 1) %>% head(precio_gasoleo_a, n = 1) %>% view()

#crear una tabla para mostrar los precios promedios de gasoleo_a y gasolina_95_e5_premium entre todos los municipios exceptuando 
#las ciudades(Madrid, Barcelona, Sevilla, y Valencia) usando los municipios para filtrar.
precio_medio_ci <- ds_f %>% select(precio_gasoleo_a, precio_gasolina_95_e5_premium, municipio, idccaa) %>% arrange(mean(precio_gasoleo_a), by_group = FALSE) %>% arrange(mean(precio_gasolina_95_e5_premium), by_group = FALSE) %>% drop_na() %>% filter(municipio != "%Sevilla%") %>% filter(municipio != "%Madrid%") %>% filter(municipio != "%Barcelona%") %>% filter(municipio != "%Valencia%") %>% head(precio_gasolina_95_e5_premium, n = 1) %>% head(precio_gasoleo_a, n = 1) %>% view()


# ii ----------------------------------------------------------------------

#guardar el resulto de c.i. en una tabla nueva 
informe_no_grandes_ciudades_expediente <- c(precio_min_ci, precio_max_ci, precio_medio_ci) %>% view()

#crear una tabla del excel para exportar
write_csv(informe_no_grandes_ciudades_expediente, "informe_no_grandes_ciudades_expediente.xls")

# D -----------------------------------------------------------------------
# i -----------------------------------------------------------------------

#Contar cuantas gasolineras estan abiertas 24 horas. Hay 5,033 gasolineras abiertas por 24 horas.
ds_low_cost %>% count(horario, sort = TRUE)

#mostrar una tabla de las 5,036 gasolineras que estan abierta 24 horas pero sin mostrar la columna de horario
no_24hrs <- ds_low_cost %>% filter(horario == 'L-D: 24H') %>% select(!horario) %>% view()

# ii ----------------------------------------------------------------------

#crear una tabla de excel para mostrar los datos de d.i.
write_excel_csv(no_24hrs, "no_24_horas.xls")

# E -----------------------------------------------------------------------
# i -----------------------------------------------------------------------

#coger datos de poblacion de ines.es
pob <- readxl::read_excel("pobmun21.xlsx", skip = 1) %>% rename(Poblacion=POB21) %>% view()

#limpiar nombres y hacer una columna en comun para juntar las tablas
pob_def <- pob %>% select(NOMBRE, Poblacion) %>% janitor::clean_names() %>% rename(municipio=nombre) %>% arrange(poblacion) %>% view()

#juntar la tabla de poblacion con low-cost
ds_w_pob <- inner_join(ds_low_cost,pob_def,by="municipio") %>% filter(idccaa != 05, idccaa != 04, id_municipio != 8111,id_municipio != 8110,poblacion > 15000 ) %>%  view()
