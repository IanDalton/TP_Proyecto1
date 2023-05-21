setwd("S:/Github/TP_Proyecto1")
data <- read.csv("./TP4/datos.csv", header = TRUE, sep = ",")

# Printing all columns in data
print(colnames(data))

# Testing for correlation between origen_financiamiento (categorical variable) and clasificacion_resumen (categorical variable)      # nolint: line_length_linter.

# Chi-squared test
chisq.test(data$origen_financiamiento, data$clasificacion_resumen)
# There's correlation


# Creating a new column with the day of the year of the date
data$day <- as.numeric(format(as.Date(data$fecha_inicio_sintomas), "%j"))

library(dplyr)
library(ggplot2)

data %>%
    group_by(origen_financiamiento) %>%
    summarise(count = n()) %>%
    View()
# There are 3 categories: Privado, Público, Sin especificar
# Showing the evolution of the number of projects over time divided by the 3 categories of origen_financiamiento

data %>%
    group_by(origen_financiamiento, day) %>%
    filter(!is.na(day)) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = day, y = count, color = origen_financiamiento)) +
    geom_line() +
    labs(
        title = "Evolución de la cantidad de proyectos por día y por origen de financiamiento", # nolint: line_length_linter.
        x = "Día del año",
        y = "Cantidad de proyectos",
        color = "Origen de financiamiento"
    )


# Showing the evolution of the number of cases marked as "Confirmado" over time divided by the 3 categories of origen_financiamiento

data %>%
    filter(clasificacion_resumen == "Confirmado") %>%
    group_by(origen_financiamiento, day) %>%
    filter(!is.na(day)) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = day, y = count, color = origen_financiamiento)) +
    geom_line() +
    labs(
        title = "Evolución de la cantidad de casos confirmados por día y por origen de financiamiento", # nolint: line_length_linter.
        x = "Día del año",
        y = "Cantidad de casos confirmados",
        color = "Origen de financiamiento"
    )

data %>%
    group_by(origen_financiamiento, clasificacion_resumen) %>%
    count() %>%
    ggplot(aes(x = origen_financiamiento,
    y = n, fill = as.factor(clasificacion_resumen))) +
        geom_bar(stat = "identity", position = "fill") +
        labs(
            title = "Porcentaje de tipo de casos por origen de financiamiento", # nolint: line_length_linter.
            x = "Origen de financiamiento",
            y = "Porcentaje de casos",
            fill = "Clasificación resumen"
        )

#extracting the first and last day of the data, excluding NA values
first_day <- min(data$day, na.rm = TRUE)
last_day <- max(data$day, na.rm = TRUE)