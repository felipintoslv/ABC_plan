# importar os dados CSV

setwd("~/IPEA II")

library(readr)

library(readxl)

# Importar os dados de Quantidade Exportada

export_qtd <- read_csv("Dados/FAOSTAT_data_7-17-2019 (1).csv")


# Importar os dados de EmissÃµes totais para os paÃ­ses selecionados


emissions_AT <- read_csv("Dados/FAOSTAT_data_7-17-2019.csv")

names(emissions_AT)[grep('Value', names(emissions_AT))] <- 'emissions'

# Importar o excel

agri <- read_csv("Dados/FAOSTAT_data_7-17-2019 (2).csv")

names(agri)[grep('Value', names(agri))] <- 'prod_agri'

livestock <- read_csv("Dados/FAOSTAT_data_7-17-2019 (3).csv")

names(livestock)[grep('Value', names(livestock))] <- 'livestock'

dat <- read_excel("Dados/dat.xlsx")

df <- merge(x = emissions_AT[, c("Area Code", "Area", "Year", "emissions")], y =  agri[ ,c("Area Code", "Year", "prod_agri")], by = c("Area Code", "Year"), all.x = TRUE)

df <- merge(x = df, y =  livestock[ ,c("Area Code", "Year", "livestock")], by = c("Area Code", "Year"), all.x = TRUE)

em <- read_excel("Dados/em.xlsx", sheet = "Plan2")

df <- merge(x = df, y =  dat[ ,c("Area Code", "Year", "hc")], by = c("Area Code", "Year"), all.x = TRUE)

df <- merge(x = df, y =  em[ ,c("Area Code", "Year", "agri_labor","mach_farm","fert", "ag_land")], by = c("Area Code", "Year"), all.x = TRUE)

library(dplyr)

df_almostfinal <- filter(df, Year %in% c(1992:2015))
df_final <- filter(df_almostfinal, Area %in% c("Argentina", "Australia", "Brazil", "Canada", "Uruguay", "Paraguay", "Chile", "China", "Colombia","France", "Germany", "India", "Indonesia", "Italy", "Malaysia", "Mexico", "Netherlands", "Spain", "Thailand", "United Kingdom", "United States of America"))

df_final <- mutate(df_final, logemission = log(emissions), logprod_agri = log(prod_agri), loglivestock = log(livestock), loghc = log(hc), logagri_labor = log(agri_labor), 
                   logmach_farm = log(mach_farm), logfert = log(fert), logag_land = log(ag_land), emissionsIntensity = ( prod_agri / emissions ), logemissionsIntensity = log((prod_agri / emissions )) )


# Análise Gráfica
library(dplyr)
library(ggplot2)

library(Synth)

x <- df_final %>%
  filter(`Area Code` %in% c(9, 10, 21, 100, 231, 351 )) %>%
  group_by(Year) %>%
  summarise(mean(logemissionsIntensity))

y <- df_final %>%
  filter(`Area Code` %in% 21) 

cod = rep("Média da Amostra", 24)

df_x <- data.frame(x,cod )

df_w = data.frame(y$Year, y$logemissionsIntensity, cod = rep("Brasil", 24))

#rename a columm
names(df_x)[names(df_x) == "Year"] <- "year"
names(df_x)[names(df_x) == "cod"] <- "cod"
names(df_x)[names(df_x) == "mean.logemissionsIntensity."] <- "emissions"

names(df_w)[names(df_w) == "y.Year"] <- "year"
names(df_w)[names(df_w) == "y.cod"] <- "cod"
names(df_w)[names(df_w) == "y.logemissionsIntensity"] <- "emissions"

# Juntar os dataframe
df_total<- rbind(df_x, df_w)
df_total$cod <- factor(df_total$cod)
ggplot(filter(df_total, year %in% c(2000:2015)), aes(x= year, y = (emissions), color = cod)) + 
  geom_line() + 
  labs(y="Log da Produção por Unidade de Emissão da Agricultura", x = " Ano", color ="Séries") +
  geom_rect(
    xmin = Inf,
    xmax = 2009,
    ymin = -Inf,
    ymax = Inf,
    # deixar o retangulo mais transparente
    alpha = 0.006
  )



# Juntar os dataframe
df_total<- rbind(df_x, df_w)
df_total$cod <- factor(df_total$cod)
ggplot(filter(df_total, year %in% c(2000:2015)), aes(x= year, y = (emissions), color = cod)) + 
  geom_line() + 
  labs(y="Log da Produção por Unidade de Emissão da Agricultura", x = " Ano", color ="Séries") +
  geom_rect(
    xmin = Inf,
    xmax = 2009,
    ymin = -Inf,
    ymax = Inf,
    # deixar o retangulo mais transparente
    alpha = 0.006
  )


# Com log
dataprep.out <- dataprep(
  foo = df_final,
  predictors = c("logprod_agri",  "logag_land", "logfert",
                 "logagri_labor", "logmach_farm", "loglivestock"
                 , "loghc"),
  predictors.op = "mean",
  time.predictors.prior = 2000:2009,
  special.predictors = list(
    list("logemissionsIntensity", 2000:2009, "mean")),
  #list("loghc", 2000:2009, "mean"),
  #list("logprod_agri",  2000:2009, "mean"),
  #list("logag_land", 2000:2009, "mean"),
  #list("logfert", 2000:2009, "mean"),
  #list("logagri_labor", 2000:2009, "mean"),
  #list("logmach_farm",  2000:2009, "mean"),
  #list("loglivestock", 2000:2009, "mean"),
  dependent = "logemissionsIntensity",
  unit.variable = "Area Code",
  unit.names.variable = "Area",
  time.variable = "Year",
  treatment.identifier = 21,
  controls.identifier = c(9, 10, 100, 231, 351),
  time.optimize.ssr = 2000:2009,
  time.plot = 2000:2015)
dataprep.out$X1

dataprep.out$Z1

synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")



gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)


path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "log da Produção por Unidade de Emissão Agricultura", Xlab = "Ano",
          Ylim = c(5, 6.5), 
          Legend = c("Brasil",
                     "Brasil Sintético"), Legend.position = "bottomright", tr.intake = 2009)
gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "gap da Produção por Unidade de Emissão da Agricultura", Xlab = "Ano",
          Ylim = c(-1.00000, 1.0), 
          Main = NA, tr.intake = 2009)

print(synth.tables$tab.pred)
print(synth.tables$tab.w)
print(gaps)



dataprep.out1 <- dataprep(
  foo = df_final,
  predictors = c("logprod_agri",  "logag_land", "logfert",
                 "logagri_labor", "logmach_farm", "loglivestock", "hc"),
  predictors.op = "mean",
  time.predictors.prior = 2000:2009,
  special.predictors = list(
    list("logemissionsIntensity", 2002:2009 , "mean")),
  #list(, 2002:2009, "mean")),
  #list("sec.energy", seq(1961, 1969, 2), "mean"),
  #list("sec.industry", seq(1961, 1969, 2), "mean"),
  #list("sec.construction", seq(1961, 1969, 2), "mean"),
  #list("sec.services.venta", seq(1961, 1969, 2), "mean"),
  #list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
  #list("popdens", 1969, "mean")),
  dependent = "logemissionsIntensity",
  unit.variable = "Area Code",
  unit.names.variable = "Area",
  time.variable = "Year",
  treatment.identifier = 100,
  controls.identifier = c(9, 10, 21,  231, 351),
  time.optimize.ssr = 2000:2009,
  time.plot = 2000:2015)
dataprep.out$X1

dataprep.out$Z1

synth.out1 <- synth(data.prep.obj = dataprep.out1, method = "BFGS")



gaps1 <- dataprep.out1$Y1plot - (dataprep.out1$Y0plot %*% synth.out1$solution.w)


synth.tables1 <- synth.tab(dataprep.res = dataprep.out1, synth.res = synth.out1)


path.plot(synth.res = synth.out1, dataprep.res = dataprep.out1,
          Ylab = "log da Produção por Unidade de Emissão da Agricultura", Xlab = "Ano",
          #Ylim = c(4.5, 6.57),
          Legend = c("India", "India Sintética"), Legend.position = "bottomright", tr.intake = 2009)

gaps.plot(synth.res = synth.out1, dataprep.res = dataprep.out1,
          Ylab = "gap da Produção por Unidade de Emissão da Agricultura", Xlab = "Ano",
          Ylim = c(-1.00000, 1.), Main = NA, tr.intake = 2009)

```




```{r placebo_2}
###################################################
### Placebo Test - Number 2 Intervention in 2006
###################################################

# Com log
dataprep.out <- dataprep(
  foo = df_final,
  predictors = c("logprod_agri",  "logag_land", "logfert",
                 "logagri_labor", "logmach_farm", "loglivestock", "loghc"),
  predictors.op = "mean",
  time.predictors.prior = 2000:2005,
  special.predictors = list(
    list("logemissionsIntensity", 2000:2005 , "mean")),
  #list(, 2002:2009, "mean")),
  #list("sec.energy", seq(1961, 1969, 2), "mean"),
  #list("sec.industry", seq(1961, 1969, 2), "mean"),
  #list("sec.construction", seq(1961, 1969, 2), "mean"),
  #list("sec.services.venta", seq(1961, 1969, 2), "mean"),
  #list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
  #list("popdens", 1969, "mean")),
  dependent = "logemissionsIntensity",
  unit.variable = "Area Code",
  unit.names.variable = "Area",
  time.variable = "Year",
  treatment.identifier = 21,
  controls.identifier = c(9, 10, 100, 231, 351),
  time.optimize.ssr = 2000:2006,
  time.plot = 2000:2015)
dataprep.out$X1

dataprep.out$Z1

synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")

gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)


path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "log da Produção por Unidade de Emissão da Agricultura", Xlab = "Ano",
          Ylim = c(4.5, 6.5), Legend = c("Brasil",
                                         "Brasil Sintético"), Legend.position = "bottomright", tr.intake = 2006)
gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "gap da Produção por Unidade de Emissão da Agricultura", Xlab = "Ano",
          Ylim = c(-1.00000, 1.0), Main = NA, tr.intake = 2006)


```


```{r placebo_3}

###################################################
### Placebo Test - Number 3 another interest variable
###################################################


# Com log
dataprep.out <- dataprep(
  foo = df_final,
  predictors = c("logprod_agri",  "logag_land", "logfert",
                 "logagri_labor", "logmach_farm", "loglivestock",
                 "logemissionsIntensity"),
  predictors.op = "mean",
  time.predictors.prior = 2000:2009,
  special.predictors = list(
    list("loghc", 2000:2009 , "mean")),
  #list(, 2002:2009, "mean")),
  #list("sec.energy", seq(1961, 1969, 2), "mean"),
  #list("sec.industry", seq(1961, 1969, 2), "mean"),
  #list("sec.construction", seq(1961, 1969, 2), "mean"),
  #list("sec.services.venta", seq(1961, 1969, 2), "mean"),
  #list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
  #list("popdens", 1969, "mean")),
  dependent = "loghc",
  unit.variable = "Area Code",
  unit.names.variable = "Area",
  time.variable = "Year",
  treatment.identifier = 21,
  controls.identifier = c(9, 10, 100, 231, 351),
  time.optimize.ssr = 2000:2009,
  time.plot = 2000:2014)

dataprep.out$X1

dataprep.out$Z1

synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")



gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)


synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)


path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "log do capital humano", Xlab = "Ano",
          #Ylim = c(4.5, 6.57), 
          Legend = c("Brasil",
                     "Brasil SintÃ©tico"), Legend.position = "bottomright", tr.intake = 2009)
gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "gap do Capital Humano", Xlab = "year",
          #Ylim = c(-1.00000, 1.0), 
          Main = NA, tr.intake = 2009)

print(synth.tables$tab.pred)
print(synth.tables$tab.w)
print(gaps)

```

```{r placebo_4}

###################################################
### Placebo Test - Number 4 Permutation
###################################################

df_final1 =  filter(df_final, `Area Code` %in%  c(9, 10, 21, 100, 231, 351))

df_final3 = df_final1 %>%
  filter(`Area Code` == 9) %>%
  mutate(`Area Code` = 2 )


df_final3 = rbind (df_final3,  df_final1 %>%
                     filter(`Area Code` == 10) %>%
                     mutate(`Area Code` = 3 )) 


df_final3 = rbind (df_final3,  df_final1 %>%
                     filter(`Area Code` == 21) %>%
                     mutate(`Area Code` = 4 ))

df_final3 = rbind (df_final3,  df_final1 %>%
                     filter(`Area Code` == 100) %>%
                     mutate(`Area Code` = 5 ))



df_final3 = rbind (df_final3,  df_final1 %>%
                     filter(`Area Code` == 231) %>%
                     mutate(`Area Code` = 6 ))

df_final3 = rbind (df_final3,  df_final1 %>%
                     filter(`Area Code` == 351) %>%
                     mutate(`Area Code` = 7 ))

store <- matrix(NA, length(2000:2015),6)
colnames(store) <- unique(df_final3$Area)

# run placebo test
for(iter in 2:7)
{
  dataprep.out <-
    dataprep(foo = df_final3,
             predictors = c("logprod_agri",  "logag_land", "logfert",
                            "logagri_labor", "logmach_farm", "loglivestock", "loghc"),
             predictors.op = "mean" ,
             time.predictors.prior = 2000:2009,
             special.predictors = list(
               list("logemissionsIntensity", 2000:2009 , "mean")),
             #list(, 2002:2009, "mean")),
             #list("sec.energy", seq(1961, 1969, 2), "mean"),
             #list("sec.industry", seq(1961, 1969, 2), "mean"),
             #list("sec.construction", seq(1961, 1969, 2), "mean"),
             #list("sec.services.venta", seq(1961, 1969, 2), "mean"),
             #list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
             #list("popdens", 1969, "mean")),
             dependent = "logemissionsIntensity",
             unit.variable = "Area Code",
             unit.names.variable = "Area",
             time.variable = "Year",
             treatment.identifier = iter,
             controls.identifier = c(2:7)[-iter+1],
             time.optimize.ssr = 2000:2009,
             time.plot = 2000:2015
    )
  
  
  # run synth
  synth.out <- synth(
    data.prep.obj = dataprep.out,
    method = "BFGS"
  )
  
  # store gaps
  store[,iter-1] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
} 

# now do figure
data <- store
rownames(data) <- 2000:2015

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 2000:2015
gap.end.pre  <- which(rownames(data)=="2009")

#  MSPE Pre-Treatment
mse        <-             apply(data[ gap.start:gap.end.pre,]^2,2,mean)
brasil.mse <- as.numeric(mse[2])
# Exclude states with 5 times higher MSPE than basque
data <- data[,mse<5*brasil.mse]
Cex.set <- .75

# Plot
plot(years,data[gap.start:gap.end,which(colnames(data)=="Brazil")],
     ylim=c(-1,1),
     xlab="year",
     xlim=c(2000,2015), ylab="da Produção por Unidade de Emissão da Agricultura",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

# Add lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }

## Add Brazil Line
#lines(years,data[gap.start:gap.end,which(colnames(data)=="Brazil)")],lwd=2,col="black")

# Add grid
abline(v=2009,lty="dotted",lwd=2)
abline(h=0,lty="dashed",lwd=2)
legend("bottomright",legend=c("Brasil","RegiÃµes de Controle"),
       lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
arrows(2009,-1.5,2012,-1.5,col="black",length=.1)
text(2009,-1.5,"Terrorism Onset",cex=Cex.set)
abline(v=2000)
abline(v=2015)
abline(h=-2)
abline(h=2)