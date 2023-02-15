######################################################
#                                                    #
# O PANORAMA DA PESQUISA SOBRE GÁS NATURAL NO BRASIL #
#                                                    #
######################################################

# Installing
install.packages("readr")
install.packages("basicTrendline")
install.packages("gridExtra")
install.packages("grid")


# Loading
library("readr")
library("basicTrendline")
library("gridExtra")
library("grid")

# Definir diretório
setwd("~/Diretório - R")


#######################################
#                                     #
# ANÁLISE DA AMOSTRA GERAL            #
# n = 843                             #
# TS=("gas natural" OR "natural gas") #
#                                     #
# ARQUIVOS                            #
# >843_anos.txt                       #
# >843_catwos.txt                     #
# >843_autores.txt                    #
# >843_afiliacoes.txt                 #
# >843_financiadoras.txt              #
# >843_revistas.txt                   #
# >843_areas.txt                      #
# >843_paises.txt                     #
#                                     #
#######################################


# Anos ################################
#######################################
brasil_anos = read.delim("843_anos.txt", header = TRUE, sep = "\t", dec = ".")

colnames(brasil_anos) = c("anos",
                          "artigos",
                          "pct_s/_843")

plot(brasil_anos$anos, brasil_anos$artigos, 
     xlab = "Ano", ylab = "Artigos", 
     main = "Gráfico 1 - Artigos publicados por ano", 
     sub = substitute(paste("Fonte: Elaborado pelo autor a partir do ",italic('World of Science.'))),
     type = "o")

hist(brasil_anos$artigos)

plot(x, y, xlab = "My X label", ylab = "My Y label")
#######################################
#######################################


# Categorias World of Science #########
#######################################
brasil_catwos = read.delim("843_catwos.txt", header = TRUE, sep = "\t", dec = ".")
colnames(brasil_catwos) = c("catwos",
                            "artigos",
                            "pct_s/_843")
pct_s_tot= cbind(brasil_catwos$artigos/1743)
brasil_catwos = cbind(brasil_catwos$catwos, brasil_catwos$artigos, pct_s_tot)
colnames(brasil_catwos) = c("catwos",
                            "artigos",
                            "pct_s_tot")
brasil_catwos = data.frame(as.matrix(brasil_catwos))
brasil_catwos = cbind(brasil_catwos, cumsum(brasil_catwos$pct_s_tot))
colnames(brasil_catwos) = c("catwos",
                            "artigos",
                            "pct_s_tot",
                            "soma pct")
brasil_catwos$pct_s_tot

plot(brasil_catwos)


colnames(brasil_catwos) = c("Categoria do WOS",
                            "Artigos",
                            "Percentual",
                            "Soma dos Percent")

plot(brasil_autores$artigos[1:50], 
     xlab = "Dez maiores publicadores", ylab = "Artigos", 
     main = "Gráfico 4 - Quantidade de trabalhos dos dez maiores publicadores", 
     sub = substitute(paste("Fonte: Elaborado pelo autor a partir do ",italic('World of Science.'))),
     type = "h")
#######################################
#######################################



# Autores ###############################
#########################################
# ver "análise dos autores em separado"

brasil_autores = read.delim("843_autores.txt", header = TRUE, sep = "\t", dec = ".")
brasil_autores = data.frame(as.matrix(brasil_autores))
colnames(brasil_autores) = c("autores",
                             "artigos",
                             "pct_s_843")

plot(brasil_autores$artigos[1:10])

plot(brasil_autores$artigos[1:50], 
     xlab = "Dez maiores publicadores", ylab = "Artigos", 
     main = "Gráfico 4 - Quantidade de trabalhos dos dez maiores publicadores", 
     sub = substitute(paste("Fonte: Elaborado pelo autor a partir do ",italic('World of Science.'))),
     type = "h")
########################################
#########################################




# Afiliações ##########################
#######################################
brasil_afiliacoes = read.delim("843_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
colnames(brasil_afiliacoes) = c("afiliacoes",
                             "artigos",
                             "pct_s/_843")
brasil_afiliacoes = cbind(brasil_afiliacoes$afiliacoes, brasil_afiliacoes$artigos)
brasil_afiliacoes = data.frame(brasil_afiliacoes)
colnames(brasil_afiliacoes) = c("afiliacoes",
                                "artigos")

plot(brasil_afiliacoes$artigos)

nrow(brasil_afiliacoes)
#653

brasil_afiliacoes$afiliacoes[1:20]

#######################################
#######################################


# Revistas ############################
#######################################
brasil_revistas = read.delim("843_revistas.txt", header = TRUE, sep = "\t", dec = ".")
colnames(brasil_revistas) = c("revistas",
                "artigos",
                "pct_s/_843")
brasil_revistas = data.frame(cbind(brasil_revistas$revistas, brasil_revistas$artigos))
colnames(brasil_revistas) = c("revistas",
                                "artigos")

plot(brasil_revistas$artigos[1:50])

brasil_revistas$revistas[1:10]
#######################################
#######################################



# AGÊNCIAS FINANCIADORAS ##############
#######################################

brasil_financiadoras = read.delim(
  "843_financiadoras.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = "."
)

colnames(brasil_financiadoras) = c(
  "financiadoras", 
  "artigos",
  "pct843"
)

sum(brasil_financiadoras[1:391,2])

brasil_financiadoras = data.frame(
  cbind(
    brasil_financiadoras[,1],
    brasil_financiadoras[,2],
    brasil_financiadoras[,2]/sum(brasil_financiadoras[1:391,2]),
    cumsum(brasil_financiadoras[,2]/sum(brasil_financiadoras[1:391,2]))
  )
)

colnames(brasil_financiadoras) = c(
  "financiadoras", 
  "artigos",
  "pct s tot",
  "soma pct"
)

grid.table(
  brasil_financiadoras
)

sum(brasil_financiadoras[2,1:391])


brasil_financiadoras = data.frame
                        (cbind
                          (financiadoras[,1], 
                           as.numeric(financiadoras[,2])
                            )
                            )

sum(brasil_financiadoras[1:392,2])


as.numeric(brasil_financiadoras[,2])
brasil_financiadoras2 = brasil[,2]
sum(brasil_financiadoras$artigos)
plot(brasil_financiadoras$artigos)
#######################################
#######################################



# Áreas de Pesquisa ###################
#######################################
brasil_areas = read.delim(
  "843_areas.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = "."
  )

colnames(brasil_areas) = c(
  "areas",
  "artigos",
  "pct_s/_843"
  )

brasil_areas = data.frame (
  cbind (
    brasil_areas[,1],
    brasil_areas[,2],
    brasil_areas[,2]/sum(brasil_areas[1:61,2]),
    cumsum(brasil_areas[,2]/sum(brasil_areas[1:61,2]))
)
)

colnames(brasil_areas) = c(
  "areas", 
  "artigos",
  "pct s tot",
  "soma pct"
)


plot(brasil_areas$artigos)
#######################################
#######################################


# Países Parceiros ####################
#######################################

brasil_paises = read.delim(
  "843_paises.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = "."
  )

colnames(brasil_paises) = c(
  "paises",
  "artigos",
  "pct_s/_843"
  )

brasil_paises = data.frame (
  cbind (
    brasil_paises[,1],
    brasil_paises[,2],
    brasil_paises[,2]/sum(brasil_paises[1:42,2]),
    cumsum(brasil_paises[,2]/sum(brasil_paises[1:42,2]))
  )
)

colnames(brasil_paises) = c(
  "paises", 
  "artigos",
  "pct s tot",
  "soma pct"
)

plot(brasil_areas$artigos)

#######################################
#######################################



#######################################
#                                     #
#   ANÁLISE DOS PRINCIPAIS AUTORES    #
#                                     #
#######################################

principais_nomes = data.frame(cbind(c("De Medeiros",
                                      "Araujo",
                                      "Bastos Neto",
                                      "Szklo",
                                      "Azevedo",
                                      "Schaeffer",
                                      "Tavares",
                                      "Arinelli",
                                      "De Oliveira",
                                      "Cavalcante")))

### DE MEDEIROS JL ####################
demedeiros_anos = read.delim("De Medeiros_anos.txt", header = TRUE, sep = "\t", dec = ".")
demedeiros_catwos = read.delim("De Medeiros_catwos.txt", header = TRUE, sep = "\t", dec = ".")
demedeiros_parceiros = read.delim("De Medeiros_parceiros.txt", header = TRUE, sep = "\t", dec = ".")
demedeiros_afiliacoes = read.delim("De Medeiros_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
demedeiros_revistas = read.delim("De Medeiros_revistas.txt", header = TRUE, sep = "\t", dec = ".")
demedeiros_financiadoras = read.delim("De Medeiros_financiadoras.txt", header = TRUE, sep = "\t", dec = ".")
demedeiros_areas = read.delim("De Medeiros_areas.txt", header = TRUE, sep = "\t", dec = ".")
demedeiros_paises = read.delim("De Medeiros_paises.txt", header = TRUE, sep = "\t", dec = ".")



### ARAUJO ODF ########################
araujo_anos = read.delim("Araujo ODF_anos.txt", header = TRUE, sep = "\t", dec = ".")
araujo_catwos = read.delim("Araujo ODF_catwos.txt", header = TRUE, sep = "\t", dec = ".")
araujo_parceiros = read.delim("Araujo ODF_parceiros.txt", header = TRUE, sep = "\t", dec = ".")
araujo_afiliacoes = read.delim("Araujo ODF_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
araujo_revistas = read.delim("Araujo ODF_revistas.txt", header = TRUE, sep = "\t", dec = ".")
araujo_financiadoras = read.delim("Araujo ODF_financiadoras.txt", header = TRUE, sep = "\t", dec = ".")
araujo_areas = read.delim("Araujo ODF_areas.txt", header = TRUE, sep = "\t", dec = ".")
araujo_paises = read.delim("Araujo ODF_paises.txt", header = TRUE, sep = "\t", dec = ".")


### BASTOS-NETO M ####################
bastosneto_anos = read.delim("Bastos-neto M_anos.txt", header = TRUE, sep = "\t", dec = ".")
bastosneto_catwos = read.delim("Bastos-neto M_catwos.txt", header = TRUE, sep = "\t", dec = ".")
bastosneto_parceiros = read.delim("Bastos-neto M_parceiros.txt", header = TRUE, sep = "\t", dec = ".")
bastosneto_afiliacoes = read.delim("Bastos-neto M_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
bastosneto_revistas = read.delim("Bastos-neto M_revistas.txt", header = TRUE, sep = "\t", dec = ".")
bastosneto_financiadoras = read.delim("Bastos-neto M_financiadoras.txt", header = TRUE, sep = "\t", dec = ".")
bastosneto_areas = read.delim("Bastos-neto M_areas.txt", header = TRUE, sep = "\t", dec = ".")
bastosneto_paises = read.delim("Bastos-neto M_paises.txt", header = TRUE, sep = "\t", dec = ".")


### SZKLO A ##########################
szklo_anos = read.delim("Szklo A_anos.txt", header = TRUE, sep = "\t", dec = ".")
szklo_catwos = read.delim("Szklo A_catwos.txt", header = TRUE, sep = "\t", dec = ".")
szklo_parceiros = read.delim("Szklo A_parceiros.txt", header = TRUE, sep = "\t", dec = ".")
szklo_afiliacoes = read.delim("Szklo A_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
szklo_revistas = read.delim("Szklo A_revistas.txt", header = TRUE, sep = "\t", dec = ".")
szklo_financiadoras = read.delim("Szklo A_financiadoras.txt", header = TRUE, sep = "\t", dec = ".")
szklo_areas = read.delim("Szklo A_areas.txt", header = TRUE, sep = "\t", dec = ".")
szklo_paises = read.delim("Szklo A_paises.txt", header = TRUE, sep = "\t", dec = ".")


### AZEVEDO DCS ######################
azevedo_anos = read.delim("Azevedo DCS_anos.txt", header = TRUE, sep = "\t", dec = ".")
azevedo_catwos = read.delim("Azevedo DCS_catwos.txt", header = TRUE, sep = "\t", dec = ".")
azevedo_parceiros = read.delim("Azevedo DCS_parceiros.txt", header = TRUE, sep = "\t", dec = ".")
azevedo_afiliacoes = read.delim("Azevedo DCS_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
azevedo_revistas = read.delim("Azevedo DCS_revistas.txt", header = TRUE, sep = "\t", dec = ".")
azevedo_financiadoras = read.delim("Azevedo DCS_financiadoras.txt", header = TRUE, sep = "\t", dec = ".")
azevedo_areas = read.delim("Azevedo DCS_areas.txt", header = TRUE, sep = "\t", dec = ".")
azevedo_paises = read.delim("Azevedo DCS_paises.txt", header = TRUE, sep = "\t", dec = ".")


### SCHAEFFER R #######################
schaeffer_anos = read.delim("Schaeffer R_anos.txt", header = TRUE, sep = "\t", dec = ".")
schaeffer_catwos = read.delim("Schaeffer R_catwos.txt", header = TRUE, sep = "\t", dec = ".")
schaeffer_parceiros = read.delim("Schaeffer R_parceiros.txt", header = TRUE, sep = "\t", dec = ".")
schaeffer_afiliacoes = read.delim("Schaeffer R_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
schaeffer_revistas = read.delim("Schaeffer R_revistas.txt", header = TRUE, sep = "\t", dec = ".")
schaeffer_financiadoras = read.delim("Schaeffer R_financiadoras.txt", header = TRUE, sep = "\t", dec = ".")
schaeffer_areas = read.delim("Schaeffer R_areas.txt", header = TRUE, sep = "\t", dec = ".")
schaeffer_paises = read.delim("Schaeffer R_paises.txt", header = TRUE, sep = "\t", dec = ".")


### TAVARES FW ########################
tavares_anos = read.delim("Tavares FW_anos.txt", header = TRUE, sep = "\t", dec = ".")
tavares_catwos = read.delim("Tavares FW_catwos.txt", header = TRUE, sep = "\t", dec = ".")
tavares_parceiros = read.delim("Tavares FW_parceiros.txt", header = TRUE, sep = "\t", dec = ".")
tavares_afiliacoes = read.delim("Tavares FW_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
tavares_revistas = read.delim("Tavares FW_revistas.txt", header = TRUE, sep = "\t", dec = ".")
tavares_financiadoras = read.delim("Tavares FW_financiadoras.txt", header = TRUE, sep = "\t", dec = ".")
tavares_areas = read.delim("Tavares FW_areas.txt", header = TRUE, sep = "\t", dec = ".")
tavares_paises = read.delim("Tavares FW_paises.txt", header = TRUE, sep = "\t", dec = ".")


### ARINELLI LD #######################
arinelli_anos = read.delim("Arinelli LD_anos.txt", header = TRUE, sep = "\t", dec = ".")
arinelli_catwos = read.delim("Arinelli LD_catwos.txt", header = TRUE, sep = "\t", dec = ".")
arinelli_parceiros = read.delim("Arinelli LD_parceiros.txt", header = TRUE, sep = "\t", dec = ".")
arinelli_afiliacoes = read.delim("Arinelli LD_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
arinelli_revistas = read.delim("Arinelli LD_revistas.txt", header = TRUE, sep = "\t", dec = ".")
arinelli_financiadoras = read.delim("Arinelli LD_financiadoras.txt", header = TRUE, sep = "\t", dec = ".")
arinelli_areas = read.delim("Arinelli LD_areas.txt", header = TRUE, sep = "\t", dec = ".")
arinelli_paises = read.delim("Arinelli LD_paises.txt", header = TRUE, sep = "\t", dec = ".")


### DE OLIVEIRA S #####################
deoliveira_anos = read.delim("De Oliveira S_anos.txt", header = TRUE, sep = "\t", dec = ".")
deoliveira_catwos = read.delim("De Oliveira S_catwos.txt", header = TRUE, sep = "\t", dec = ".")
deoliveira_parceiros = read.delim("De Oliveira S_parceiros.txt", header = TRUE, sep = "\t", dec = ".")
deoliveira_afiliacoes = read.delim("De Oliveira S_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
deoliveira_revistas = read.delim("De Oliveira S_revistas.txt", header = TRUE, sep = "\t", dec = ".")
deoliveira_financiadoras = read.delim("De Oliveira S_financiadoras.txt", header = TRUE, sep = "\t", dec = ".")
deoliveira_areas = read.delim("De Oliveira S_areas.txt", header = TRUE, sep = "\t", dec = ".")
deoliveira_paises = read.delim("De Oliveira S_paises.txt", header = TRUE, sep = "\t", dec = ".")


### CAVALCANTE CL #####################
cavalcante_anos = read.delim("Cavalcante CL_anos.txt", header = TRUE, sep = "\t", dec = ".")
cavalcante_catwos = read.delim("Cavalcante CL_catwos.txt", header = TRUE, sep = "\t", dec = ".")
cavalcante_parceiros = read.delim("Cavalcante CL_parceiros.txt", header = TRUE, sep = "\t", dec = ".")
cavalcante_afiliacoes = read.delim("Cavalcante CL_afiliacoes.txt", header = TRUE, sep = "\t", dec = ".")
cavalcante_revistas = read.delim("Cavalcante CL_revistas.txt", header = TRUE, sep = "\t", dec = ".")
cavalcante_financiadoras = read.delim("Cavalcante CL_financiadoras.txt", header = TRUE, sep = "\t", dec = ".")
cavalcante_areas = read.delim("Cavalcante CL_areas.txt", header = TRUE, sep = "\t", dec = ".")
cavalcante_paises = read.delim("Cavalcante CL_paises.txt", header = TRUE, sep = "\t", dec = ".")



#######################################
#### D E D U Ç Õ E S ##################
#######################################



### PARCEIROS #########################
#######################################
principais_parceiros = data.frame(as.matrix(c(nrow(demedeiros_parceiros),
                                              nrow(araujo_parceiros),
                                              nrow(bastosneto_parceiros),
                                              nrow(szklo_parceiros),
                                              nrow(azevedo_parceiros),
                                              nrow(schaeffer_parceiros),
                                              nrow(tavares_parceiros),
                                              nrow(arinelli_parceiros),
                                              nrow(deoliveira_parceiros),
                                              nrow(cavalcante_parceiros))))

principais_parceiros = cbind(principais_nomes, principais_parceiros)
posicao = cbind.data.frame(c(1:10))
principais_parceiros = cbind(posicao, principais_parceiros)

colnames(principais_parceiros) = (c("Posição", "Autores", "Qtd de Parceiros"))

summary(principais_parceiros)
#Autores          Qtd de Parceiros
#Length:10          Min.   :11.00   
#Class :character   1st Qu.:22.25   
#Mode  :character   Median :30.00   
#Mean   :30.30   
#3rd Qu.:35.50   
#Max.   :60.00

plot(principais_parceiros$`Qtd de Parceiros`, 
     xlab = "Dez maiores publicadores", ylab = "Parceiros", 
     main = "Gráfico 2 - Quantidade de parceiros por autor", 
     sub = substitute(paste("Fonte: Elaborado pelo autor a partir do ",italic('World of Science.'))),
     type = "h")

abline(lm(principais_parceiros$`Qtd de Parceiros` ~ principais_parceiros$Posição))

summary(lm(principais_parceiros$`Qtd de Parceiros` ~ principais_parceiros$Posição))
#######################################
#######################################




### AFILIAÇÕES ########################
#######################################
principais_afiliacoes = data.frame(as.matrix(c(nrow(demedeiros_afiliacoes),
                                              nrow(araujo_afiliacoes),
                                              nrow(bastosneto_afiliacoes),
                                              nrow(szklo_afiliacoes),
                                              nrow(azevedo_afiliacoes),
                                              nrow(schaeffer_afiliacoes),
                                              nrow(tavares_afiliacoes),
                                              nrow(arinelli_afiliacoes),
                                              nrow(deoliveira_afiliacoes),
                                              nrow(cavalcante_afiliacoes))))

principais_afiliacoes = cbind(principais_nomes, principais_afiliacoes)
posicao = cbind.data.frame(c(1:10))
principais_afiliacoes = cbind(posicao, principais_afiliacoes)

colnames(principais_afiliacoes) = (c(
  "Posição",
  "Autores", "Qtd de Afiliações"))

summary(principais_afiliacoes)
#Posição        Autores          Qtd de Parceiros
#Min.   : 1.00   Length:10          Min.   : 3.00   
#1st Qu.: 3.25   Class :character   1st Qu.: 6.25   
#Median : 5.50   Mode  :character   Median : 8.00   
#Mean   : 5.50                      Mean   : 9.40   
#3rd Qu.: 7.75                      3rd Qu.: 9.75   
#Max.   :10.00                      Max.   :24.00   

plot(principais_afiliacoes$`Qtd de Afiliações`, 
     xlab = "Dez maiores publicadores", ylab = "Afiliações", 
     main = "Gráfico 3 - Quantidade de afiliações por autor", 
     sub = substitute(paste("Fonte: Elaborado pelo autor a partir do ",italic('World of Science.'))),
     type = "h")

abline(lm(principais_afiliacoes$`Qtd de Afiliações` ~ principais_afiliacoes$Posição))

summary(lm(principais_afiliacoes$`Qtd de Afiliações` ~ principais_afiliacoes$Posição))

#Call:
#  lm(formula = principais_afiliacoes$`Qtd de Afiliações` ~ principais_afiliacoes$Posição)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-5.6424 -3.4076 -1.3545  0.7818 14.1455 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)                     11.067      4.244   2.607   0.0313 *
#  principais_afiliacoes$Posição   -0.303      0.684  -0.443   0.6695  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 6.213 on 8 degrees of freedom
#Multiple R-squared:  0.02394,	Adjusted R-squared:  -0.09806 
#F-statistic: 0.1962 on 1 and 8 DF,  p-value: 0.6695
#######################################
#######################################


########################################
#                                      #
# ANÁLISE DAS PRINCIPAIS FINANCIADORAS #
#                                      #
########################################

financiadoras_anos_cnpq = read.delim(
  "financiadoras_anos_cnpq.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = ".")
financiadoras_anos_capes = read.delim(
  "financiadoras_anos_capes.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = ".")
financiadoras_anos_petrobras = read.delim(
  "financiadoras_anos_petrobras.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = ".")
financiadoras_anos_fapeap = read.delim(
  "financiadoras_anos_fapeap.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = ".")
financiadoras_anos_fapesp = read.delim(
  "financiadoras_anos_fapesp.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = ".")
financiadoras_anos_finep = read.delim(
  "financiadoras_anos_finep.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = ".")
financiadoras_anos_faperj = read.delim(
  "financiadoras_anos_faperj.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = ".")
financiadoras_anos_anp = read.delim(
  "financiadoras_anos_anp.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = ".")
financiadoras_anos_fapemig = read.delim(
  "financiadoras_anos_fapemig.txt", 
  header = TRUE, 
  sep = "\t", 
  dec = ".")

financiadoras_anos = data.frame(
  cbind(
    (2020:2003),
    financiadoras_anos_cnpq[,2],
    financiadoras_anos_capes[,2],
    financiadoras_anos_petrobras[,2],
    financiadoras_anos_fapeap[,2],
    financiadoras_anos_fapesp[,2],
    financiadoras_anos_finep[,2],
    financiadoras_anos_faperj[,2],
    financiadoras_anos_anp[,2],
    financiadoras_anos_fapemig[,2]
  )
)

colnames(financiadoras_anos) = (c(
  "Data",
  "CNPq",
  "CAPES",
  "Petrobrás",
  "FAPEAP",
  "FAPESP",
  "FINEP",
  "FAPERJ",
  "ANP",
  "FAPEMIG"
  )
  )

plot(financiadoras_anos$Data, financiadoras_anos$CNPq, 
     xlab = "Ano", ylab = "Artigos financiados", 
     main = "Gráfico 2 - Principais fontes de financiamento da ciência brasileira do gás natural", 
     sub = substitute(paste("Fonte: Elaborado pelo autor a partir do ",italic('Web of Science.'))),
     type = "l",
     )

lines(financiadoras_anos$Data, financiadoras_anos$CAPES,
      
)

lines(financiadoras_anos$Data, financiadoras_anos$Petrobrás,
      col = "yellow"
)

lines(financiadoras_anos$Data, financiadoras_anos$FAPEAP,
      col = "brown"
)

lines(financiadoras_anos$Data, financiadoras_anos$FAPESP,
      col = "purple"
)

lines(financiadoras_anos$Data, financiadoras_anos$FINEP,
      col = "gray"
)

lines(financiadoras_anos$Data, financiadoras_anos$FAPERJ,
      col = "red"
)

lines(financiadoras_anos$Data, financiadoras_anos$ANP,
      col = "green"
)

lines(financiadoras_anos$Data, financiadoras_anos$FAPEMIG,
      col = "blue"
)





financiadoras_anos_capes[,2],
financiadoras_anos_petrobras[,2],
financiadoras_anos_fapeap[,2],
financiadoras_anos_fapesp[,2],
financiadoras_anos_finep[,2],
financiadoras_anos_faperj[,2],
financiadoras_anos_anp[,2],
financiadoras_anos_fapemig[,2]


#######################################
############# R A S C U N H O #########
#######################################


# Revistas####
revistas <- read.delim("843_revistas.txt", header = TRUE, sep = "\t", dec = ".")
colnames(revistas) = c("titulo",
                       "qtd",
                       "pct")
soma <- 
revistas <- cbind(revistas)

head(revistas)

plot(revistas$qtd)
plot(density(revistas$qtd))
hist(revistas$qtd)

summary(revistas$qtd)
###############


##########
plot(lm(principais_parceiros$`Qtd de Parceiros`, 
        xlab = "Dez maiores publicadores", ylab = "Parceiros", 
        main = "GRÁFICO 2 - Quantidade de parceiros por autor", 
        sub = substitute(paste("Fonte: Elaborado pelo autor a partir do ",italic('World of Science.'))),
))

trendline(principais_parceiros$`Qtd de Parceiros`, principais_parceiros$Autores,model="line2P",plot=FALSE,ePos="none",linecolor="red")


lines(predict(lm(principais_parceiros$Autores~principais_parceiros$`Qtd de Parceiros`, data = principais_parceiros), col='green'))


lm(principais_parceiros$`Qtd de Parceiros`~principais_parceiros$Autores, data = principais_parceiros)
#########

########

A = data.frame(
  matrix(
    c(1,2,3,4,5,6,7,8,9),
    3,
    3
)
)

colnames(A) = c(
  "A", 
  "B", 
  "C"
)

#para uma tabela ao lado da outra

             
grid.table(A)

             

             
##########


