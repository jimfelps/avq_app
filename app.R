
library(shiny)
library(tidyverse)
library(janitor)
library(lubridate)
library(patchwork)
library(readxl)

theme_set(theme_minimal())

labels <- c(
    "Simple" = "Simple (<=100)",
    "Moderate" = "Moderate (100-200)",
    "Complex" = "Complex (200-400)",
    "High Complexity" = "High Complex (400-800)",
    "Very High Complexity" = "Very High Complex (>800)"
)

Engineering_Metrics <- read_excel("Engineering Metrics.xlsx", 
                                  sheet = "Compass Order Summary", col_types = c("text", 
                                                                                 "text", "text", "text", "text", "text", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "text", "text", "text", "text", "text", 
                                                                                 "text", "text", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "text", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "text", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "text", "numeric", "numeric", "numeric", 
                                                                                 "numeric", "text", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "text", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "text", "numeric", "numeric", "numeric", 
                                                                                 "numeric", "text", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "text", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "text", "numeric", "numeric", "numeric", 
                                                                                 "numeric", "text", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "text", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "text", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "text", "numeric", "numeric", "numeric", 
                                                                                 "numeric", "text", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "text", "text"), 
                                  skip = 41) %>%
    clean_names("snake") %>%
    mutate(proj_num = str_sub(order_number, 1,8))

eng_complexity_lookup <- read_csv("eng_complexity_lookup.csv", 
                                  col_types = cols(`Order Number` = col_character())) %>%
    clean_names("snake") %>%
    mutate(proj_num = str_sub(order_number, 1,8)) %>%
    select(proj_num,
           region,
           complexity)

new_oswt <- Engineering_Metrics %>%
    mutate(brand = if_else(str_detect(order_type, "CLAIM"), "CLAIM",
                           if_else(str_detect(order_type, "PART"), "PARTS ORDER",
                                   if_else(str_detect(order_type, "GB"), "BUTLER",
                                           if_else(str_detect(order_type, "VP"), "VP",
                                                   if_else(str_detect(order_type, "CSS/CONV"), "CSS",
                                                           if_else(str_detect(order_type, "CSS/HS"), "HEAVY STRUCTURES",
                                                                   if_else(str_sub(order_type, 1,4) == "ROOF", "BUTLER", "Other"))))))),
           corp = c("BBNA"),
           act_tons = if_else(order_status == "EN", planned_tons, eng_mfst_tons),
           quote_tons = planned_tons) %>%
    select(corp,
           brand,
           project_name,
           order_number,
           proj_num,
           project_manager,
           builder_name,
           actual_hours,
           budgeting_hours,
           act_tons,
           quote_tons) %>%
    filter(brand == "BUTLER" |
               brand == "VP") %>%
    left_join(eng_complexity_lookup, by = c("proj_num" = "proj_num")) %>%
    group_by(corp, brand, project_name, complexity, region) %>%
    summarise(
        act_hours = sum(actual_hours),
        bud_hours = sum(budgeting_hours),
        avq_hours = round(act_hours/bud_hours,2),
        act_tons = sum(act_tons),
        bud_tons = sum(quote_tons),
        avq_tons = round(act_tons/bud_tons,2)
    ) %>%
    ungroup() %>%
    filter(!is.na(complexity),
           !is.nan(avq_hours),
           !is.infinite(avq_hours),
           !is.nan(avq_tons),
           !is.infinite(avq_tons))                                             # removing NA's but need to review in future months (only one in January when developing)

new_oswt$complexity <- factor(new_oswt$complexity,
                              levels = c("CII/FT",
                                         "Simple (<=100)",
                                         "Moderate (100-200)",
                                         "Complex (200-400)",
                                         "High Complex (400-800)",
                                         "Very High Complex (>800)"))

new_oswt$complexity <- fct_recode(new_oswt$complexity, !!! labels)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Actual vs Quote Hours by Complexity"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "avqInput",
                "Act vs Quote Range",
                min = min(new_oswt$avq_hours),
                max = max(new_oswt$avq_hours),
                value = c(1, 2)),
            
            selectInput(
                "brandInput",
                "Brand:",
                choices = unique(new_oswt$brand),
                selected = "BUTLER"
            ),
            
            selectInput(
                "regionInput",
                "Region:",
                choices = unique(new_oswt$region),
                selected = "BCT"
            ),
            
            checkboxGroupInput(
                "complexInput",
                "Complexity:",
                choices = unique(new_oswt$complexity),
                selected = "CII/FT"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("avqPlot"),
           br(),
           tableOutput("avqTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    filtered <- reactive({
        new_oswt %>%
            filter(
                avq_hours >= input$avqInput[1],
                avq_hours <= input$avqInput[2],
                brand == input$brandInput,
                region == input$regionInput,
                complexity %in% input$complexInput
            ) %>%
            arrange(complexity, desc(avq_hours))
    })
    
    output$avqPlot <- renderPlot({
        ggplot(filtered(), aes(corp, avq_hours)) +
            geom_jitter(size = 5) +
            scale_y_continuous(limits = c(min(new_oswt$avq_hours), max(new_oswt$avq_hours))) +
            coord_flip() +
            labs(
                x = "",
                y = ""
            ) + 
            theme(
                axis.text.y = element_blank()
            )
    })
    
    output$avqTable <- renderTable({
        filtered()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
