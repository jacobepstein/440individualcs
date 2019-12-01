
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(httr)
library(purrr)
library(sjPlot)
library(lme4)
library(shiny)
library(shinythemes)


watches = readRDS("watches.rds")
model = glmer(data = watches, log(Price) ~ Size  + (1|Material) + (1|Brand) + Condition + Gender + Year + Gender * Year)


ui = fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("The Market For Luxury Watches"),
  tabsetPanel(
    tabPanel("Visualize Luxury Watch Prices", fluid = TRUE,
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 selectInput(inputId = "variable",
                             label = "Select a Variable:",
                             choices = colnames(watches)[c(-6, -7)], hr()),
                 checkboxInput("groupGender", label = "Group Gender (if Applicable)", value = FALSE),
                 checkboxInput("topBrands", label = "Only Show Top Brands", value = FALSE),
                 checkboxInput("topMaterials", label = "Only Show The Top Materials", value = FALSE),
                 numericInput("priceMin", label = "Minimum Price ($000s)", value = 0),
                 numericInput("priceMax", label = "Maximum Price ($000s)", value = 200),
                 actionButton("plot", "Generate Plot")
               ),
               mainPanel  = mainPanel(
                 tabPanel(title = "abc", plotOutput("plot"))
               )
             )
    ),
    tabPanel("Explore the Data", fluid = TRUE,
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 selectInput(inputId = "gender",
                             label = "Select a Gender",
                             choices = c("Any", watches$Gender %>% unique()), hr()),
                 selectInput(inputId = "material",
                             label = "Select a Material",
                             choices = c("Any", watches$Material %>% unique()), hr()),
                 selectInput(inputId = "brandName",
                             label = "Select a Brand",
                             choices = c("Any", watches$Brand %>% unique()), hr()),
                 selectInput(inputId = "condition",
                             label = "Select a Condition",
                             choices = c("Any", watches$Condition %>% unique()), hr()),
                 sliderInput("yearSlider", label = "Year Range", min = min(watches$Year), 
                             max = max(watches$Year), value = c(min(watches$Year), max(watches$Year)), sep = ""),
                 sliderInput("sizeSlider", label = "Size Range", min = min(watches$Size), 
                             max = max(watches$Size), value = c(min(watches$Size), max(watches$Size))),
                 numericInput("priceMin0", label = "Minimum Price ($000s)", value = 0),
                 numericInput("priceMax0", label = "Maximum Price ($000s)", value = 200),
                 actionButton("find", "Find All Matches"),
                 downloadButton("downloadData", "Download This Data")
               ),
               mainPanel  = mainPanel(
                 fluidRow(title = "message", h3(textOutput("message")), style = "height:75px"),
                 tabPanel(title = "found", dataTableOutput("matches"))
               )
             )
    ),
    tabPanel("What is your Watch Worth?", fluid = TRUE,
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 selectInput(inputId = "worthGender",
                             label = "Gender:",
                             choices = watches$Gender %>% unique() %>% sort(), hr()),
                 selectInput(inputId = "worthMaterial",
                             label = "Material:",
                             choices = watches$Material %>% unique() %>% sort(), hr()),
                 selectInput(inputId = "worthBrand",
                             label = "Brand:",
                             choices = watches$Brand %>% unique() %>% sort(), hr()),
                 selectInput(inputId = "worthCondition",
                             label = "Condition:",
                             choices = watches$Condition %>% unique() %>% sort(),hr()),
                 numericInput("worthSize", label = "Size (mm):", value = 40),
                 numericInput("worthYear", label = "Year:", value = 2019),
                 actionButton("worth", "Find Out")
                 
               ),
               mainPanel = mainPanel(
                 fluidRow(title = "estimate", h3(textOutput("estimate"))),
                 fluidRow(title = "estimate", h4(textOutput("similarity_message"))),
                 tabPanel(title = "similar", dataTableOutput("similar"))
               )
             )
    )
  )
)

server = function(input, output){
  
  
  
  
  
  state = reactiveValues(
    observers = list()
  )
  
  observeEvent({input$worth},
               {
                 for(i in seq_along(state$observers)){
                   state$observers[[i]]$destroy()
                 }           
                 
                 watches = readRDS("watches.rds")
                 
                 worthGender = input$worthGender
                 worthYear = input$worthYear
                 worthSize = input$worthSize
                 worthMaterial = input$worthMaterial
                 worthCondition = input$worthCondition
                 worthBrand = input$worthBrand
                 
                 prediction = predict(model, newdata = data.frame(Gender = worthGender, Size = worthSize, Year = worthYear, Material = worthMaterial,
                                                                  Condition = worthCondition, Brand = worthBrand)) %>% exp()
                 
                 output$estimate = renderText(paste0("Our model estimates your watch is worth $", round(prediction/1000, 2), "k"))
                 
                 min_similar = 4
                 
                 watches$similarity_score = rep(NA, nrow(watches))
                 for(i in 1:nrow(watches)){
                   watches$similarity_score[[i]] = sum(watches$Gender[[i]] == worthGender, abs(watches$Year[[i]] - worthYear) <= 2, 
                                                       abs(watches$Size[[i]] - worthSize) <= 5, watches$Material[[i]] == worthMaterial,
                                                       watches$Condition[[i]] == worthCondition, watches$Brand[[i]] == worthBrand)
                 }
                 
                 watches = watches %>%
                   arrange(desc(similarity_score)) %>%
                   filter(similarity_score > min_similar)
                 
                 if (nrow(watches) == 0){output$similarity_message = renderText("We didn't find any watches similar to yours in our data set")}
                 else {output$similarity_message = renderText(paste0("We found ", nrow(watches), 
                                                                     " watches very similar to yours, with an average price of $", round(mean(watches$Price / 1000), 1),"k:"))}
                 
                 output$similar = renderDataTable(watches)
                 
               })
  
  
  observeEvent({input$find}, 
               {
                 # Destroy existing observers
                 for(i in seq_along(state$observers)){
                   state$observers[[i]]$destroy()
                 }
                 
                 
                 inputGender = input$gender
                 inputMaterial = input$material
                 inputBrand = input$brandName
                 inputCondition = input$condition
                 yearMin = input$yearSlider[[1]]
                 yearMax = input$yearSlider[[2]]
                 sizeMin = input$sizeSlider[[1]]
                 sizeMax = input$sizeSlider[[2]]
                 priceMin0 = input$priceMin0
                 priceMax0 = input$priceMax0
                 
                 watches = readRDS("watches.rds")
                 
                 if (inputGender != "Any"){
                   watches = watches %>%
                     filter(Gender == inputGender)
                 }
                 if (inputMaterial != "Any"){
                   watches = watches %>%
                     filter(Material == inputMaterial)
                 }
                 if (inputBrand != "Any") {
                   watches = watches %>%
                     filter(Brand == inputBrand)
                 }
                 if (inputCondition != "Any") {
                   watches = watches %>%
                     filter(Condition == inputCondition)
                 }
                 watches = watches %>% 
                   filter(Year >= yearMin) %>%
                   filter(Year <= yearMax) %>%
                   filter(Size >= sizeMin) %>%
                   filter(Size <= sizeMax) %>%
                   filter(Price >= 1000*priceMin0) %>%
                   filter(Price <= 1000*priceMax0)
                 
                 output$matches = renderDataTable(watches)
                 
                 output$message = renderText(paste0("We found ", nrow(watches), " watches that fit your criteria, for an average price of $",
                                                    (mean(watches$Price) / 1000 )%>% round(1), "k!"))
                 
                 output$downloadData <- downloadHandler(filename = "watch_data.csv", content = function(file) {
                   write.csv(watches, file, row.names = FALSE)
                 }
                 )
                 
               })
  
  
  
  observeEvent({
    input$plot}, 
    {
      
      # Destroy existing observers
      for(i in seq_along(state$observers)){
        state$observers[[i]]$destroy()
      }
      watches = readRDS("watches.rds")
      
      
      watches = watches %>%
        filter(Price >= 1000 * input$priceMin) %>%
        filter(Price <= 1000 * input$priceMax)
      
      
      varname = input$variable
      
      binwidth = .3
      
      if(input$topBrands){
        top_brands = watches %>%
          group_by(Brand) %>%
          summarize("count" = n()) %>%
          arrange(desc(count)) %>%
          slice(1:20) %>%
          .$Brand
        
        watches = watches %>%
          filter(Brand %in% top_brands)
        
        if(varname == "Brand") {binwidth = .8}
      }
      
      if(input$topMaterials){
        top_materials = watches %>%
          group_by(Material) %>%
          summarize("count" = n()) %>%
          arrange(desc(count)) %>%
          slice(1:12) %>%
          .$Material
        
        watches = watches %>%
          filter(Material %in% top_materials)
        
        if(varname == "Material") {binwidth = .9}
        
      }
      
      variable = watches[[input$variable]]
      
      group_gender = input$groupGender
      
      output$plot = renderPlot({
        if (varname %in% c("Size", "Year")){
          if (group_gender){
            ggplot(data = watches, aes(y = Price, x = variable, color = Gender)) +
              geom_point() +
              scale_y_continuous(trans = "log2", breaks = c(1000, 2500, 5000, 10000, 25000, 50000)) +
              geom_smooth(method = "lm") +
              labs(y = "Price (Log Scale)", x = varname, title = paste0("Price vs ", varname)) +
              theme_minimal()
          }else {
            ggplot(data = watches, aes(y = Price, x = variable)) +
              geom_point() +
              scale_y_continuous(trans = "log2", breaks = c(1000, 2500, 5000, 10000, 25000, 50000)) +
              geom_smooth(method = "lm") +
              labs(y = "Price (Log Scale)", x = varname, title = paste0("Price vs ", varname)) +
              theme_minimal()
          }
        } else if(varname %in% c("Brand", "Material")){
          ggplot(watches, aes(x = reorder(variable, Price), fill = reorder(variable, Price), y = Price)) +
            geom_boxplot(width = binwidth, show.legend = FALSE) + 
            scale_y_continuous(trans = "log2", breaks = c(1000, 2500, 5000, 10000, 25000, 50000)) +
            labs(y = "Price (Log Scale)", x = varname, title = paste0("Price vs ", varname)) +
            theme_minimal() +
            coord_flip()
        } else {
          ggplot(watches, aes(x = variable, fill = variable, y = Price)) +
            geom_violin() +
            geom_boxplot(width = .1, fill = "grey") + 
            scale_y_continuous(trans = "log2", breaks = c(1000, 2500, 5000, 10000, 25000, 50000)) +
            labs(y = "Price (Log Scale)", x = varname, title = paste0("Price vs ", varname)) +
            theme_minimal()
        }
      })
    })
}






shinyApp(ui = ui, server = server)

