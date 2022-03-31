library(shiny)
library(hash)
library(plotly)

df <- read.csv("./data/bank-full.csv", header = TRUE, sep = ";", fill = TRUE, stringsAsFactors = FALSE)
users <- read.csv("./data/users.csv", header = TRUE, sep = ";", fill = TRUE, stringsAsFactors = FALSE)
rownames(users) <- c(users[["name"]])
users <- subset(users, select = -c(name))
params <- c("job", "marital", "education", "default", "housing", "loan", "balance", "contact", "month", "poutcome", "age", "day", "duration", "campaign", "pdays", "previous")
classes <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "y")

multi <- c("job", "marital", "education", "contact", "month", "poutcome")
bin <- c("default", "housing", "loan")
numeric <- c("age", "balance", "day", "duration", "campaign", "pdays", "previous")

for (c in classes) {
    df[, c] <- as.factor(df[, c])
}
for (n in numeric) {
    df[, n] <- as.numeric(df[, n])
}

gen_model <- function(params) {
    return(as.formula(paste("y ~ ", paste(params, collapse = " + "))))
}

translate <- function(data) {
    rdata <- c()
    for (i in 1:length(data)) {
        if (data[i] == "job") {
            rdata <- append(rdata, "Beruf")
        }
        if (data[i] == "marital") {
            rdata <- append(rdata, "Ehestatus")
        }
        if (data[i] == "education") {
            rdata <- append(rdata, "Bildung")
        }
        if (data[i] == "default") {
            rdata <- append(rdata, "Verzugskredit")
        }
        if (data[i] == "housing") {
            rdata <- append(rdata, "Baudarlehen")
        }
        if (data[i] == "loan") {
            rdata <- append(rdata, "Privat Kredit")
        }
        if (data[i] == "balance") {
            rdata <- append(rdata, "Kontostand")
        }
        if (data[i] == "contact") {
            rdata <- append(rdata, "Kommunikationsart")
        }
        if (data[i] == "month") {
            rdata <- append(rdata, "Kommunikationsmonat")
        }
        if (data[i] == "poutcome") {
            rdata <- append(rdata, "Kampagnenergebnis")
        }
        if (data[i] == "age") {
            rdata <- append(rdata, "Alter")
        }
        if (data[i] == "day") {
            rdata <- append(rdata, "Kommunikationstag")
        }
        if (data[i] == "duration") {
            rdata <- append(rdata, "Kommunikationsdauer")
        }
        if (data[i] == "campaign") {
            rdata <- append(rdata, "Anzahl der Kontakte insgesamt")
        }
        if (data[i] == "pdays") {
            rdata <- append(rdata, "Tage seit letztem Kontakt")
        }
        if (data[i] == "previous") {
            rdata <- append(rdata, "Anzahl der Kontakte in der aktuellen Kampagne")
        }
    }
    return(rdata)
}

model <- glm(gen_model(params), data = as.data.frame(df), binomial(link = "logit"))

# Vorgefertigte Kunden / Standard Parameter
cParams <- hash()
# default paramter
cParams[["age"]] <- 40
cParams[["marital"]] <- "married"
cParams[["job"]] <- "unknown"
cParams[["education"]] <- "unknown"
cParams[["default"]] <- FALSE
cParams[["housing"]] <- FALSE
cParams[["loan"]] <- FALSE
cParams[["balance"]] <- 1000
cParams[["day"]] <- 1
cParams[["month"]] <- "jan"
cParams[["pdays"]] <- 100
cParams[["duration"]] <- 200
cParams[["campaign"]] <- 2
cParams[["previous"]] <- 1
cParams[["contact"]] <- "unknown"
cParams[["poutcome"]] <- "unknown"

ui <- fluidPage(
                titlePanel("Verkaufskampagne Darlehen"),
                fluidRow(
                         column(1,
                                h4(strong("Bestandskunden"), align="center"),
                                selectInput(inputId = "customer", label = "Bestandskunden:", selectize = FALSE, size = "10", width = "400px",
                                            choices = c(list("Kein Kunde"), rownames(users)),
                                            selected = "Kein Kunde"
                                            ),

                                ),
                         column(2, offset = 1,
                                h4(strong("Persönliche Informationen"), align="center"),
                                #   1 - age (numeric)
                                numericInput(
                                             inputId = "age", 
                                             label = "Alter:",
                                             value = cParams[["age"]],
                                             min = 18,
                                             max = 100,
                                             step = 1
                                             ),
                                #   2 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
                                selectInput(inputId = "marital", label = "Ehestatus:",
                                            choices = list(
                                                           "Verheiratet" = "married",
                                                           "Geschieden" = "divorced",
                                                           "Ledig" = "single"
                                                           ),
                                            selected = cParams[["marital"]]
                                            ),

                                #   3 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
                                #                                       "blue-collar","self-employed","retired","technician","services")
                                selectInput(inputId = "job", label = "Beruf:",
                                            choices = list(
                                                           "Admin" = "admin.",
                                                           "Unbekannt" = "unknown",
                                                           "Arbeitslos" = "unemployed",
                                                           "Management" = "management",
                                                           "Hauswirtschaftshelfer" = "housemaid",
                                                           "Unternehmer" = "entrepreneur",
                                                           "Student" = "student",
                                                           "Handwerker" = "blue-collar",
                                                           "Selbstständig" = "self-employed",
                                                           "Rentner" = "retired",
                                                           "Techniker" = "technician",
                                                           "Services" = "services"
                                                           ),
                                            selected = cParams[["job"]]
                                            ),

                                #   4 - education (categorical: "unknown","secondary","primary","tertiary")
                                selectInput(inputId = "education", label = "Bildung:",
                                            choices = list(
                                                           "Unbekannt" = "unknown",
                                                           "Abitur" = "secondary",
                                                           "Regelschulabschluss" = "primary",
                                                           "Abgeschlossenes Studium" = "tertiary"
                                                           ),
                                            selected = cParams[["education"]]
                                            ),
                                ),
                         column(2,
                                h4(strong("Finanzielle Informationen"), align="center"),
                                #   5 - default: has credit in default? (binary: "yes","no")
                                checkboxInput(inputId = "default", "Gibt es ein Verzugskredit?", cParams[["default"]]),
                                #   7 - housing: has housing loan? (binary: "yes","no")
                                checkboxInput(inputId = "housing", "Gibt es ein Baudarlehnen?", cParams[["housing"]]),
                                #   8 - loan: has personal loan? (binary: "yes","no")
                                checkboxInput(inputId = "loan", "Besteht ein privat Kredit?", cParams[["loan"]]),
                                sliderInput(inputId = "balance", "Kontostand in $:", min = -20000, max = 150000, value = cParams[["balance"]]),
                                numericInput("value", NULL, min = -20000, max = 150000, value = cParams[["balance"]])
                                ),
                         column(2,
                                h4(strong("Informationen zum letzten Kontakt innerhalb der aktuellen Kampagne"), align="center"),
                                h5(strong("Letzter Kontakt Versuch war am")),
                                #  9 - day: last contact day of the month (numeric) current campaign
                                selectInput(
                                            inputId = "day", 
                                            label="Tag des Monats:", 
                                            choices = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                                            selected = cParams[["day"]]
                                            ),

                                #  10 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec") current campaign
                                selectInput(inputId = "month", label = "Monat:",
                                            choices = list(
                                                           "Januar" = "jan",
                                                           "Februar" = "feb",
                                                           "März" = "mar",
                                                           "April" = "apr",
                                                           "Mai" = "may",
                                                           "Juni" = "jun",
                                                           "Juli" = "jul",
                                                           "August" = "aug",
                                                           "September" = "sep",
                                                           "Oktober" = "oct",
                                                           "November" = "nov",
                                                           "Dezember" = "dec"
                                                           ),
                                            selected = cParams[["month"]]
                                            ),

                                #  11 - duration: last contact duration, in seconds (numeric) current campaign
                                numericInput(
                                             inputId = "duration", 
                                             label = "Gesprächsdauer des letzten Kontakt in Sekunden):", 
                                             min = 0, 
                                             max = 5000, 
                                             value = cParams[["duration"]]),


                                #   12 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
                                selectInput(inputId = "contact", 
                                            label = "Wie wurde der Kontakt aufgenommen?",
                                            choices = list(
                                                           "Unbekannt" = "unknown",
                                                           "Festnetz" = "telephone",
                                                           "Handy" = "cellular"
                                                           ),
                                            selected = cParams[["contact"]]
                                )
                                ),
                         column(2 , h4(strong("Weitere Informationen"), align="center"),

                                #  13 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
                                numericInput(
                                             inputId = "pdays", 
                                             label = "Tage seit letztem Kontakt der vorherigen Kampagne (-1 für kein Kontakt):", 
                                             min = -1, 
                                             max = 1000, 
                                             value = cParams[["pdays"]]),

                                #  14 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
                                sliderInput(inputId = "campaign", "Anzahl der Kontakte in der aktuellen Kampagne", min = 0, max = 100, value = cParams[["campaign"]]),

                                #  15 - previous: number of contacts performed before this campaign and for this client (numeric)
                                sliderInput(inputId = "previous", "Anzahl der Kontakte insgesamt", min = 0, max = 100, value = cParams[["previous"]]),


                                #  16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")
                                selectInput(inputId = "poutcome", label = "Ergebnis der letzten Kampagne:",
                                            choices = list(
                                                           "Unbekannt" = "unknown",
                                                           "Anderes" = "other",
                                                           "Nicht erfolgreich" = "failure",
                                                           "Erfolgreich" = "success"
                                                           ),
                                            selected = cParams[["poutcome"]]
                                            ),
                                ),
                         ),
                fluidRow(
                         titlePanel("Lohnt es sich den Kunden anzurufen?"),
                         wellPanel(
                         splitLayout(cellWidths = c("15%", "85%"),
                                     uiOutput("Prognose"),
                                     plotlyOutput("Verteilung")
                         ), style = "background: #565656"
                        )
                )
)
server <- function(input, output, session) {
    observeEvent(input$balance,{
                     updateNumericInput(session, "value", value = input$balance)
}, ignoreInit = TRUE)
    observeEvent(input$value,{
                     updateSliderInput(
                                       session, "balance", value = input$value)
})
    observeEvent(input$customer, {

                     #customer
                     if (input$customer == "Kein Kunde"){
                         updateNumericInput(session, "age", value = cParams[["age"]])
                         updateSelectInput(session, "marital", selected = cParams[["marital"]])
                         updateSelectInput(session, "job", selected = cParams[["job"]])
                         updateSelectInput(session, "education", selected = cParams[["education"]])
                         updateCheckboxInput(session, "default", value = cParams[["default"]])
                         updateCheckboxInput(session, "housing", value = cParams[["housing"]])
                         updateCheckboxInput(session, "loan", value = cParams[["loan"]])
                         updateSliderInput(session, "balance", value = cParams[["balance"]])
                         updateNumericInput(session, "value", value = cParams[["value"]])
                         updateSelectInput(session, "day", selected = cParams[["day"]])
                         updateSelectInput(session, "month", selected = cParams[["month"]])
                         updateNumericInput(session, "pdays", value = cParams[["pdays"]])
                         updateNumericInput(session, "duration", value = cParams[["duration"]])
                         updateSliderInput(session, "campaign", value = cParams[["campaign"]])
                         updateSliderInput(session, "previous", value = cParams[["previous"]])
                         updateSelectInput(session, "contact", selected = cParams[["contact"]])
                         updateSelectInput(session, "poutcome", selected = cParams[["poutcome"]])
                     } else {
                         updateNumericInput(session, "age", value = users[input$customer, "age"])
                         updateSelectInput(session, "marital", selected = users[input$customer, "marital"])
                         updateSelectInput(session, "job", selected = users[input$customer, "job"])
                         updateSelectInput(session, "education", selected = users[input$customer, "education"])
                         updateCheckboxInput(session, "default", value = users[input$customer, "default"])
                         updateCheckboxInput(session, "housing", value = users[input$customer, "housing"])
                         updateCheckboxInput(session, "loan", value = users[input$customer, "loan"])
                         updateSliderInput(session, "balance", value = users[input$customer, "balance"])
                         updateNumericInput(session, "value", value = users[input$customer, "value"])
                         updateSelectInput(session, "day", selected = users[input$customer, "day"])
                         updateSelectInput(session, "month", selected = users[input$customer, "month"])
                         updateNumericInput(session, "pdays", value = users[input$customer, "pdays"])
                         updateNumericInput(session, "duration", value = users[input$customer, "duration"])
                         updateSliderInput(session, "campaign", value = users[input$customer, "campaign"])
                         updateSliderInput(session, "previous", value = users[input$customer, "previous"])
                         updateSelectInput(session, "contact", selected = users[input$customer, "contact"])
                         updateSelectInput(session, "poutcome", selected = users[input$customer, "poutcome"])
                     }
})
    prognose <- reactive({
        pred <- df[params]
        for (el in params) {
            if (el %in% classes) {
                if (typeof(reactiveValuesToList(input)[[el]]) == "logical") {
                    pred[1, el] <- as.factor(ifelse(reactiveValuesToList(input)[[el]] == FALSE, "no", "yes"))
                } else {
                    pred[1, el] <- as.factor(reactiveValuesToList(input)[[el]])
                }
            }
            if (el %in% numeric) {
                pred[1, el] <- as.numeric(reactiveValuesToList(input)[[el]])
            }
        }
        pred <- predict(model, pred[1, ])
        pred
    })

    plot_val <- reactive({
        X_plot <- params
        y_plot <- c()
        for (i in 1:length(X_plot)) {
            if (X_plot[i] %in% multi) {
                name <- paste(X_plot[i], reactiveValuesToList(input)[[X_plot[i]]], sep = "")
                if (name %in% rownames(summary(model)$coefficients)) {
                    y_plot <- append(y_plot, exp(summary(model)$coefficients[name, 1]))
                } else {
                    y_plot <- append(y_plot, exp(1))
                }
            } else if (X_plot[i] %in% bin) {
                if (reactiveValuesToList(input)[[X_plot[i]]] == TRUE) {
                    name <- paste(X_plot[i], "yes", sep = "")
                } else {
                    name <- paste(X_plot[i], "no", sep = "")
                }
                if (name %in% rownames(summary(model)$coefficients)) {
                    y_plot <- append(y_plot, exp(summary(model)$coefficients[name, 1]))
                } else {
                    y_plot <- append(y_plot, exp(1))
                }
            } else if (X_plot[i] %in% numeric) {
                y_plot <- append(y_plot, exp(summary(model)$coefficients[X_plot[i], 1]))
            }
        }
        list(x = translate(X_plot), y = y_plot)
    })

    output$Verteilung <- renderPlotly({
        p <- prognose()
        prog <- exp(p) / (1 + exp(p))
        if (prog <= 1 && prog >= 0) {
            col <- (prog * 110) + 20
        } else {
            col <- 0
        }
        fig <- plot_ly(x = plot_val()$x, y = plot_val()$y, type = 'bar', name = 'Primary Product', marker = list(color = paste('hsl(', col, ',100%,50%)')))
        fig <- fig %>% layout(xaxis = list(title = "", tickangle = -45),
                              yaxis = list(title = "Ausschlagskraft"),
                              margin = list(b = 100),
                              barmode = 'group'
        )
        fig %>% toWebGL()
        fig
    })
    output$Prognose <- renderUI({
        p <- prognose()
        prog <- exp(p) / (1 + exp(p))
        if (prog <= 1 && prog >= 0) {
            col <- (prog * 110) + 20
        } else {
            col <- 0
        }
        Prognose <- tags$h2(paste(if(prog > 0.5) {"Ja, zu "} else {"Nein, zu "}, round(prog, digits = 2) * 100, "%." ), style = paste("color: hsl(", col, ",100%,50%);"))
        
    })
}
shinyApp(ui, server)
