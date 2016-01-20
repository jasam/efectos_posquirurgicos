# Getting data
setwd("C:/Users/jasam/Google Drive/Laboratorios/Datascience/nena")
data <- read.table("BASE DE DATOS 26 julio 2015.csv", sep = ";", header = TRUE)
sum(is.na(data))
summary(data)
boxplot(data$edad)
table(data$sexo)
str(data$sexo)

#grouping data
library(dplyr)
library(ggplot2)
library(corrplot)
data$sexo <- as.character(data$sexo)
group_data <- summarise(group_by(data, sexo), count = n())
##### plot X = 
#cleanning data
clean_data <- data$sexo
data[data$sexo == 0,c("sexo", "casocont")]
new_data <- data[,c("sexo", "casocont")]
new_data$sexo[new_data$sexo == "0"] <- "Masculino" 
new_data$sexo[new_data$sexo == "1"] <- "Femenino" 
new_data$casocont[new_data$casocont == "0"] <- "Control" 
new_data$casocont[new_data$casocont == "1"] <- "Casos" 
people <- table(new_data$sexo, new_data$casocont)
df <- data.frame(people)
df
names(df) <- c("Sexo","Caso_control","Frequencia")
#plot
ggplot(data=df, aes(x=Sexo, y=Frequencia, fill=Caso_control)) + geom_bar(stat="identity")



##### plot X = controls and cases
#cleanning data
clean_data <- data$sexo
new_data <- data[,c("casocont", "sexo")]
new_data$sexo[new_data$sexo == "0"] <- "Masculino" 
new_data$sexo[new_data$sexo == "1"] <- "Femenino" 
new_data$casocont[new_data$casocont == "0"] <- "Control" 
new_data$casocont[new_data$casocont == "1"] <- "Casos" 
people <- table(new_data$casocont, new_data$sexo)
df <- data.frame(people)
df
names(df) <- c("Caso_control","Sexo","Frequencia")
#plot
ggplot(data=df, aes(x=Caso_control, y=Frequencia, fill=Sexo)) + geom_bar(stat="identity") + labs(title = "Distribución por sexo", x = "Agudeza Visual", y = "Frecuencia")



#Boxplots

library(ggplot2)
data_box <- data[,c("equivesf", "equivesfpostq", "casocont")]
data_box$casocont[data_box$casocont == "0"] <- "Controles" 
data_box$casocont[data_box$casocont == "1"] <- "Casos" 
data_box$equivesf <- as.numeric(sub(",", ".", data_box$equivesf, fixed = TRUE))

#cleanning data
dataf1 <- data_box[,c("equivesf", "casocont")]
dataf1$qx <- "PREQUIRURGICO" 
names(dataf1) <- c("equivesf", "caso", "qx")
dataf2 <- data_box[,c("equivesfpostq", "casocont")]
dataf2$qx <- "POSQUIRURGICO" 
names(dataf2) <- c("equivesf", "caso", "qx")
new_data <- rbind(dataf1, dataf2)
new_data$equivesf <- as.numeric(sub(",", ".", new_data$equivesf, fixed = TRUE))
new_data$qx <- factor(new_data$qx, levels = c("PREQUIRURGICO", "POSQUIRURGICO"))
    
ggplot(new_data, aes(x = factor(caso), y = equivesf)) + geom_boxplot(fill = "#F8766D", colour = "#000000") + facet_wrap(~ qx) + labs(title = "Distribución del Defecto Refractivo", x = "Agudeza Visual", y = "Equivalente Esférico") 

# presion intraocular
library(ggplot2)
names(data)
data_box <- data[,c("pioccora", "tonometria", "casocont")]
data_box$casocont[data_box$casocont == "0"] <- "Controles" 
data_box$casocont[data_box$casocont == "1"] <- "Casos" 
data_box$pioccora <- as.numeric(sub(",", ".", data_box$pioccora, fixed = TRUE))
data_box$tonometria <- as.numeric(sub(",", ".", data_box$tonometria, fixed = TRUE))

#cleanning data
dataf1 <- data_box[,c("pioccora", "casocont")]
dataf1$qx <- "PREQUIRURGICO" 
names(dataf1) <- c("pio", "caso", "qx")
dataf2 <- data_box[,c("tonometria", "casocont")]
dataf2$qx <- "POSQUIRURGICO" 
names(dataf2) <- c("pio", "caso", "qx")
new_data <- rbind(dataf1, dataf2)
data_box$pioccora <- as.numeric(sub(",", ".", data_box$pioccora, fixed = TRUE))
data_box$tonometria <- as.numeric(sub(",", ".", data_box$tonometria, fixed = TRUE))
new_data$qx <- factor(new_data$qx, levels = c("PREQUIRURGICO", "POSQUIRURGICO"))

ggplot(new_data, aes(x = factor(caso), y = pio)) + geom_boxplot(fill = "#00BFC4", colour = "#000000") + facet_wrap(~ qx) + labs(title = "Distribución de la Presión Intraocular", x = "Agudeza Visual", y = "Presión Intraocular") 

corrplot(mcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45)
# create fake dataset with additional attributes - sex, sample, and temperature
x <- data.frame(
    values = c(runif(100, min = -2), runif(100), runif(100, max = 2), runif(100)),
    sex = rep(c('M', 'F'), each = 100),
    sample = rep(c('sample_a', 'sample_b'), each = 200),
    temperature = sample(c('15C', '25C', '30C', '42C'), 400, replace = TRUE)
)

# compare different sample populations across various temperatures
ggplot(x, aes(x = sample, y = values, fill = sex)) +
    geom_boxplot() +
    facet_wrap(~ temperature)

as.numeric(sub(",", ".", p, fixed = TRUE))

data
names(data)
sum(is.na(data$equivesfpostq))

#crate a new column for data division
#cast data
data$equivesfpostq <- as.numeric(sub(",", ".", data$equivesfpostq, fixed = TRUE))
data$formula <- as.numeric(data$equivesfpostq) > 0 

#defecto refractivo pos quirurgico    
data$pos <- which(data$equivesfpostq >= 0 | data$equivesfpostq < 0)

#change colors manually
ggplot(data, aes(x = reorder(pos, equivesfpostq), y=equivesfpostq, fill=formula)) + 
    geom_bar(stat="identity", position="identity") +
    scale_x_discrete(breaks=NULL) +
    labs(title = "Defecto Refactivo Posquirúrgico", x = "Pacientes", y = "Equivalente Esférico") +
    scale_fill_manual(values=c("#F8766D", "#00BFC4"), guide=FALSE)

#without legend
title <- "Defecto Refactivo Posquirúrgico"
x_lab <- "Pacientes"
y_lab <- "Equivalente Esférico"
data <- arrange(data, equivesfpostq)
data$pos <- which(data$equivesfpostq >= 0 | data$equivesfpostq < 0)
data$formula <- as.numeric(data$equivesfpostq) > 0 

ggplot(data, aes(x = pos, y=equivesfpostq, fill=formula)) + 
    geom_bar(stat="identity", position="identity") +
    geom_hline(yintercept=0) +
    labs(title = title, x = x_lab, y = y_lab) +
    theme(legend.position="none")

#with area under curve
ggplot(data2, aes(x=pos, y=equivesfpostq)) + 
    geom_area(aes(fill=formula)) +
    geom_line() +
    geom_hline(yintercept=0) +
    labs(title = title, x = x_lab, y = y_lab) +
    theme(legend.position="none")


M <- cor(mtcars)
corrplot(M, method = "circle")

names(data)
data$edad
matrix_data <- data[,c("pioccora", "paquimetria", "ablacin", "equivesf", "chora", "kpd")]
matrix_data$pioccora <- as.numeric(sub(",", ".", matrix_data$pioccora, fixed = TRUE))
matrix_data$paquimetria <- as.numeric(sub(",", ".", matrix_data$paquimetria, fixed = TRUE))
matrix_data$ablacin <- as.numeric(sub(",", ".", matrix_data$ablacin, fixed = TRUE))
matrix_data$equivesf <- as.numeric(sub(",", ".", matrix_data$equivesf, fixed = TRUE))
matrix_data$chora <- as.numeric(sub(",", ".", matrix_data$chora, fixed = TRUE))
matrix_data$kpd <- as.numeric(sub(",", ".", matrix_data$kpd, fixed = TRUE))
matrix_cor <- cor(matrix_data)
corrplot(matrix_cor, method = "shade",shade.col=NA, tl.col="black", tl.srt=45)
