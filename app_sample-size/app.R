# this is a previous version of the web application that allows the user to toggle
# the scenarios shown in the graphic. It does not allow the user to input their
# own scenarios however.

library(shiny)
library(tidyverse)
library(plotly)
library(glue)

RSV_wheeze <- readr::read_csv("./www/scenarios.csv")

ui <- fluidPage(
   
   titlePanel("Effective sample size calculator for maternal vaccinations"),
   hr(),
   HTML(glue("<b>Overview</b><br/>Proposed maternal vaccinations for RSV are 
             currently undergoing clinical trials. While the primary outcome 
             measures will investigate reductions in RSV illness among mothers 
             and infants, there is interest in assessing the relationship with 
             distal childhood outcomes, including recurrent wheeze. We used plausible 
             estimates of the attack rate for vaccine efficacy, the RSV attack 
             rate, the baseline risk of recurrent wheeze, and the risk ratio for 
             recurrent wheeze among infants with an RSV hospitalization vs. no 
             hospitalization to estimate the required minimal sample size (per 
             treatment arm) to detect an effect of maternal RSV vaccination on 
             childhood recurrent wheeze. The results are illustrated in the 
             graphic below. <br>"
             )
        ),
   hr(),
   HTML("<b/>Toggle the selection of scenarios to add or remove them from the plot:</b>"),
   fluidRow(
     column(3,
            HTML(paste0("<b/>Vaccine efficacy (%)</b>")),
            checkboxGroupInput(inputId = "vaccine_efficacy", 
                               label = NULL,
                               choices = unique(RSV_wheeze$vaccine.efficacy)*100, 
                               selected =  unique(RSV_wheeze$vaccine.efficacy)*100)),

     column(3,
            HTML(paste0("<b/>Attack rates (%)</b>")),
            checkboxGroupInput(inputId = "attack_rates",
                               label = NULL,
                               choices = list("2.7" = 0.027, 
                                              "6.0" = 0.06,
                                              "17.0" = 0.17),
                               selected = list(0.027, 0.06, 0.17)
                               )
            ),
     
     column(3,
            HTML(paste0("<b/>Baseline risk of recurrent wheeze (%)</b>")),
            checkboxGroupInput(inputId = "baseline_risk_wheeze", 
                               label = NULL,
                               choices = unique(RSV_wheeze$baseline.risk.wheeze)*100, 
                               selected =  unique(RSV_wheeze$baseline.risk.wheeze)*100)),
     
     column(3,
            HTML(paste0("<b/>Risk ratio for RSV hospitalization and recurrent wheeze</b>")),
            checkboxGroupInput(inputId = "RR_wheeze_rsvh", 
                               label = NULL,
                               choices = unique(RSV_wheeze$RR.wheeze.rsvh), 
                               selected =  unique(RSV_wheeze$RR.wheeze.rsvh)))
     
     ),
   hr(),
   plotOutput("sampleSizePlot2"),
   plotlyOutput("sampleSizePlot3")
   
)

server <- function(input, output) {
  
  RSV_wheeze_toggle <- reactive({
    RSV_wheeze %>% filter(vaccine.efficacy %in% c(as.numeric(input$vaccine_efficacy)/100),
                          baseline.risk.wheeze %in% c(as.numeric(input$baseline_risk_wheeze)/100),
                          RR.wheeze.rsvh %in% c(as.numeric(input$RR_wheeze_rsvh)),
                          rsv.attack.rate %in% input$attack_rates
    )

  }) 
  
  figure.1 <- reactive({
    figure.1 <- ggplot(RSV_wheeze_toggle(), 
                       aes(y = size.one.arm.equal, x = RR.wheeze.vacc)) + 
      geom_line(aes(linetype = factor(baseline.risk.wheeze))) +
      geom_point(shape = 21, alpha = 0.8, aes(fill = as.factor(rsv.attack.rate), size = as.factor(RR.wheeze.rsvh))) +
      facet_wrap(~`Vaccine efficacy`, labeller = label_both) +
      guides(fill = guide_legend(title = "RSV-LRTI attack rate (%)", title.position = "top"),
             linetype = guide_legend(title = "Baseline risk of recurrent wheeze", title.position = "top"),
             size = guide_legend(title = "Risk ratio for wheeze-RSV LRTI", title.position = "top")) +
      #scale_fill_manual(values = c("#fee6ce", "#fdae6b", "#e6550d")) +
      #scale_linetype_manual(values = c(3, 2, 1), labels = c("4.9%", "9.5%", "20%")) +
      scale_y_log10(breaks = c(100, 1000, 10000, 100000, 1000000), 
                    labels = c("100", "1,000", "10,000", "100,000", "1,000,000"), 
                    limits = c(100, 4700000)) +
      ylab("Sample size required (per trial arm)") + 
      xlab("Risk ratio between wheeze and vaccination") +
      theme(strip.background = element_rect(fill="white"), 
            strip.text.x = element_text(size = 12),
            strip.text.y = element_text(size = 12)) +
      theme_minimal() +
      theme(legend.position = "top") 
      
    return(figure.1)
  })
   
   output$sampleSizePlot2 <- renderPlot({
     figure.1()     
   })
   
   output$sampleSizePlot3 <- renderPlotly({
     ggplotly(figure.1()) %>% layout(margin = list(l = 100), showlegend = F)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

