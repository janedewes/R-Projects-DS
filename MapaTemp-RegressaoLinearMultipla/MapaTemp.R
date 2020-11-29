

# Mapa de Temperatura através de Regressão Linear Múltipla


# Ajustar um modelo econstruir um mapa de temperatura.
# A partir de dados de temperatura média anual de estações, prever a temperatura para regiões que não possuem observações. 
# Ou seja, estimar a temperatura para regiões que não possuem observações. 
# Podemos fazer isso através de uma equação mtm que represete a temperatura em função de outras variáveis. 

# A temperatura do ar varia em fç da latitude da longitude e da altitude, portanto existe uma relação linear entre essas variáveis. 
# Para fazer essa relação usamos a Regressão Linear Múlltipla


# À partit dos dados, definir os Betas da equação(coeficientes): pesos - usar a equação para construir o mapa. 
# x1 = longitude
# x2 = latitude
# x3 = altitude

# -----------------------------------------------------------------------------------------------------------------------------------


# Parte I - Ler e processar os dados -------------------------------------------

# 1- Carregar os pacotes
install.packages("geobr")
install.packages("raster")
install.packages("ggspatial")
install.packages("fields")

library(ggplot2)
library(geobr)
library(raster)
library(ggspatial)
library(fields)


# 2 - Def dir 
setwd("")
getwd()

# 3 - Ler os dados 
dados_temp <- read.csv('dados_temperatura.csv')

# 4 - Ler o mapa de relevo (MDE) (dados do tipo topografia .tif) (pacote raster lê esse tipo de dados TIF)
relevo <- raster('relevo_minas_gerais.tif')


# 5 - Ler os dados do limite terrotorial de Minas Gerais 
plot(relevo)

# 6 - Determinar os coef da equação de regressão -------------------------------
names(dados_temp) # nomes das cols

modelo <- lm(formula = temp~longitude+latitude+altitude, data = dados_temp) # fórmula 


# OBS: com a fórmula determinamos os valores dos betas da equação - com essa eq podemos determinar os valores 
# de Temperatura.
# OBS: PARA DETERMINAR A REGRESSÃO LINEAR MÚLTIPLA USAMOS A FÇ "lm"



summary(modelo)
# NOTE o valor de Multiple R-squared:  0.915 
# ------------------------------------------------------------------------------

# 7 - Converter os dados do formato espacial (raster - .tif) para df
top_df <- as.data.frame(relevo, xy=TRUE) # xy = add tb os valores de lon(x) e lat(y)


# 8 - Remover os dados faltantes
top_df <- na.omit(top_df)


# 9 - Renomenar colunass de dados 
names(top_df) <- c("lon", "lat", "alt")
# acessar <- atribuir novos valores
head(top_df)


# ------------------------------------------------------------------------------
# 10 - Calcular a temperatura para o Estado de Minas Gerais (USANDO OS VALORES DE BETA CALCULADOS COM A FÇ lm!!)

# 1) acessar os valores dos coef:
summary(modelo)


# Trocar df, pois agora iremos trabalhar com a temp
temp_mg <- top_df
head(temp_mg)



# 2) Montar a equação:
23.49 - 0.25*top_df$lon + 0.48*top_df$lat - 0.0053*top_df$alt
# Temperatura = 23.49 - 0.25*lon + 0.48*lat - 0.0053*alt



# Add uma nova col ao df (col das temperaturas calculadas com a eq acima)
temp_mg$temp <- 23.49 - 0.25*top_df$lon + 0.48*top_df$lat - 0.0053*top_df$alt
View(temp_mg)



# ------------------------------------------------------------------------------
# Parte II - Plotar os Dados ---------------------------------------------------

# 11 - Estabelecer valores categóricos de temperatura

# (criar categorias para os conjuntos de valores)
# (essa fç considera intervalo aberto na esquerda e intervalo fechado na direita) - cut(dados, breaks=, labels=) 
# Intervalo = 2ºC
# Mínimo = 8ºC
# Máximo = 26ºC
# Para def os valores acima, é preciso analisar os dados!


cut(temp_mg$temp, breaks = c(8,10,12,14,16,18,20,22,24,26), labels = c('8-10', '10-12', '12-14', '14-16', '16-18', '18-20', '20-22', '22-24', '24-26'))
# dados da tabela temp_mg col temp


# Add uma nova col com os dados categóricos:
temp_mg$temp_cat <- cut(temp_mg$temp, breaks = c(8,10,12,14,16,18,20,22,24,26), labels = c('8-10', '10-12', '12-14', '14-16', '16-18', '18-20', '20-22', '22-24', '24-26'))
View(temp_mg)



# ------------------------------------------------------------------------------
# 12 - Plotar os valores calculados de temperatura 

# CONTRÁRIO - transf o df em raster(.tif)
ggplot(data = temp_mg)+
  geom_raster(aes(x=lon,y=lat, fill=temp_cat))+
  geom_sf(data = mg, fill="transparent")+
  scale_fill_manual(values = tim.colors(9))+
  annotation_scale()+
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  labs(x=NULL, y=NULL, 
       title = "Mapa: Temperatura Média Anual para o Estado de Minas Gerais - BRASIL", fill= "ºC")+
  theme_light()





# ------------------------------------------------------------------------------
# 13 - Sobrepor os limites territoriais de MG 
#(pacote geobr -> Dados do IBGE que dá acesso aos dados dos limites territoriais do BR)

mg <- read_state(code_state = "MG")
plot(mg)
class(mg) # os dados do IBGE vem no formato "sf"
# Existe uma fç no pacote ggplot2 para plotar dados do tipo "sf" -> geom_sf(data = mg) 
# (ADD ESSA CAMADA AO GRFICO ACIMA!)




# ------------------------------------------------------------------------------
# 14 - Modificar barra de cores 

# O pacote fields já possui uma paleta de cores para ser usada -> [tim.colors(9)]
# SERÁ MAIS UMA CAMADA DO GRÁFICO ACIMA: "scale_fill_manual(values = tim.colors(9)"
# 9 -> numero de cores.


# ------------------------------------------------------------------------------
# 15 - Add a escala do mapa 

# pacote ggspatial -> annotation_scale() - (ADD CAMADA AO GRAFICO ACIMA!)


# ------------------------------------------------------------------------------
# 16 - Add a orientação do  mapa 

# pacote ggspatial - (ADD CAMADA AO GRAFICO ACIMA!)
# annotation_north_arrow(location='tl', style = north_arrow_fancy_orienteering()) 
# top e left (tl)


# ------------------------------------------------------------------------------
# 17 -  Definir títulos do mapa

# labs(x=NULL, y=NULL, title = "Mapa: Temperatura Média Anual", fill= "ºC")

#(ADD CAMADA AO GRAFICO ACIMA!)
# NULL -> retura titulos dos eixos x e y.


# ------------------------------------------------------------------------------
# 18 - Def o tema do mapa

# theme_light()


# ------------------------------------------------------------------------------
# 19 - Salvar o mapa

# Por ser sido feito com ggplot2 usar:
ggsave(filename = 'mapa_temp_MG.png')



# End
