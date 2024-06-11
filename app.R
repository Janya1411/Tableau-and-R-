######latest


library('shiny')
library('ggplot2')
library('ggiraph')
library('shinyjs')
library("dplyr")
library("DT")  
library("tidyr")
library(sf)
library(stringr)
library(cicerone)
library(reactable)
library(reactablefmtr)
library(stringr)


# Change the directory to the folder where the R file is stored
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#setting working directory
setwd(script_dir)

# Load the GEOM90007 Tableau in Shiny library
source('tableau-in-shiny-v1.0.R')

greater_melb_polygon = st_read("greater_melb_polygon_std_liv_v2.shp", crs = 3577)
greater_melb_polygon <- st_transform(greater_melb_polygon, crs = 4326)
greater_melb_polygon <- st_centroid(greater_melb_polygon)
url="https://public.tableau.com/views/greater_melb/"
greater_melb_polygon$SAL_NAME_2 = str_to_title(greater_melb_polygon$SAL_NAME_2)


filter_list= c("Livablity Score","Feel Safe", "Mental Health", "BMI","House Price", "Schooling","Hospital","Transportation","Green Cover") 

home_tab <- tabPanel(
  #title='Feel Safe',
  fluidRow(id="banner",
           style = "background-color: #84bdc6; padding: 20px;", 
           #style = "background-color: #5ea1ba; padding: 20px;",
           # Column for the image
           column(2, 
                  img(src = "Melb_city.png", alt = "Melbourne City", width = "100%", height = "auto")
           ),
           # Column for the title
           column(3, 
                  h1('  Melbourne Liveablity Analysis', style = "color: white;font-family: Cooper Black;font-size: 20px;")
           )
  ),
  sidebarLayout(
    sidebarPanel(
      #style = "background-color: #D4EAE9;",
      #style = "background-color: #b0d9d6;",
      tags$div(id="suburbinput",
               style = "background-color: #84bdc6; padding: 10px; border-radius: 5px; margin-bottom: 10px;", # Container for selectizeInput
               selectizeInput("suburb", "Which suburb you will be working in?", choices = c("All",sort(unique(greater_melb_polygon$SAL_NAME_2))), 
                              options = list(placeholder = "Select a suburb", 
                                             onInitialize = I("function() { this.setValue(''); }"), 
                                             class = "selectize-dropdown"))
      ),
      tags$div(id="distanceinput",
               style = "background-color: #84bdc6; padding: 10px; border-radius: 5px; margin-bottom: 10px;", # Container for sliderInput
               sliderInput("distance", "How far are you willing to travel (in km)?", min = 5, max = 100, step = 5, sep = ',', value=10, 
                           width = '100%', 
                           pre = icon("arrows-h"))
      ),
      fluidRow(
        column(width=12,
               uiOutput('text_ui'),
               uiOutput('button_ui'),
               uiOutput("buttons_UI"),
               conditionalPanel(
                 condition = "show_table()=='No_Show'",br(),br()),
               uiOutput("tableUI_1"),
               uiOutput("tableUI")               ))
      
      
    ),
    mainPanel(
      div(
        textOutput("displayText"),
        style = "margin-bottom: 20px; font-weight: bold;"  # Add margin to create space below the visualization
      ),
      
      tableauPublicViz(
        id = 'tableauViz',       
        url = 'https://public.tableau.com/views/greater_melb/FeelSafe',
        height = "600px",
        width = "900px"
      )
    )
  )
)

ui <-  fluidPage(
  navbarPage(
    #header=setUpTableauInShiny(),
    header = list(use_cicerone(),setUpTableauInShiny()),
    title='',
    
    enhanced_dark_theme_css <- tags$style(HTML("
    body, html {
    height: 100%;
    background-color: #F1F8F8;
    }
    
    .navbarPage .container-fluid {
      height: 100%;
      display: flex;
      flex-direction: column;
    }
    
    .tab-content {
      flex: 1;
    }
    
    .shiny-sidebarlayout-container, .shiny-sidebarlayout .container-fluid {
      height: 100%;
    }
    
    .sidebar, .main {
      height: 100%;
      overflow: auto;  /* To allow scrolling if content overflows */
    }
    .navbar {
      background-color: #84bdc6;
    }
    
    .btn {
      background-color: #3498db;
      color: white;
      border: none;
    }
    .btn:hover {
      background-color: #2980b9;
    }
    .tableauPublicViz {
      border: none;
    }
    
    img {
      border-radius: 5px;
      box-shadow: 0 0 10px rgba(0, 0, 0, 0.5);
    }
    h3 {
      color: #3498db;
    }
  ")),
    
    home_tab
  ))



server <- function(input, output, session) {
  
  text <- reactiveVal("")
  current_table <- reactiveVal(NULL)
  filter <- reactiveVal(NULL)
  show_table = reactiveVal('No_Show')
  observeEvent(c(input$distance,input$suburb,filter()),{
    tmp2<-greater_melb_polygon
    
    if((input$suburb!="All")&(input$suburb %in% tmp2$SAL_NAME_2)){
      
      target_suburb <- tmp2[tmp2$SAL_NAME_2 == input$suburb, ]
      
      tmp2$distance_to_target <- as.numeric(st_distance(tmp2$geometry, target_suburb$geometry))
      
      suburbs_within_dist <- tmp2[tmp2$distance_to_target <= input$distance*1000, ]
      suburbs_within_dist <- st_drop_geometry(suburbs_within_dist)
      
      runjs(sprintf('let viz = document.getElementById("tableauViz");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("Sal_Name_2021", ["%s"], FilterUpdateType.Replace);', paste(suburbs_within_dist$SAL_NAME_2,collapse='","')))
      
      if(!is.null(filter())){
        selected_columns <- suburbs_within_dist[c("SAL_NAME_2",filter())]
        sorted_data <- selected_columns %>%
          arrange(desc(!!rlang::sym(filter())))
        sorted_data[2]=round(sorted_data[2],4)
        current_table(datatable(sorted_data))
        
      }
    } else if(!is.null(filter())){
      greater_melb_polygon_temp=st_drop_geometry(greater_melb_polygon)
      selected_columns <- greater_melb_polygon_temp[c("SAL_NAME_2",filter())]
      sorted_data <- selected_columns %>%
        arrange(desc(!!rlang::sym(filter())))
      sorted_data[2]=round(sorted_data[2],4)
      current_table(datatable(sorted_data))
    }
    if(input$suburb=="All"){
      runjs(sprintf('let viz = document.getElementById("tableauViz");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("Sal_Name_2021",["All"], FilterUpdateType.All);'))
    }
  })
  observeEvent(input$livability, {
    runjs(sprintf(
      "
    let viz = document.getElementById('tableauViz');
    let workbook =  viz.workbook;
    workbook.changeParameterValueAsync('Color By','Liveablity Score');
    workbook.changeParameterValueAsync('Livablity_par','Yes');
    "
    ))
    
    filter("Livability")
    
    text("Liveablity Score is an overall average liveablity score for a city generated by considering all the factors mentioned on the left ")
    show_table('show')
  })
  observeEvent(input$feelsafe, {
    runjs(sprintf(
      "
    let viz = document.getElementById('tableauViz');
    let workbook =  viz.workbook;
    workbook.changeParameterValueAsync('Color By','Feel Safe');
    workbook.changeParameterValueAsync('Feelsafe','Yes');
    "
    ))
    filter("FeelSafe")
    show_table('show')
    text("'Feel Safe' represents the perception of safety and security experienced by residents in a particular location. It takes into account factors such as crime rates, public safety measures, community engagement, and the overall sense of security in the area. A higher FeelSafe value indicates a greater sense of safety and trust within the community.")
  })
  observeEvent(input$bmi, {
    runjs(sprintf(
      "
    let viz = document.getElementById('tableauViz');
    let workbook =  viz.workbook;
    workbook.changeParameterValueAsync('Color By','BMI');
    workbook.changeParameterValueAsync('BMI_par','Yes');
    "
    ))
    filter("BMI")
    show_table('show')
    
    text("BMI, or Body Mass Index, measures body fat based on height and weight. It provides insights into weight status (underweight, normal weight, overweight, or obese) and associated health risks. Analyzing the distribution of BMI values can provide insights into the overall health and liveability of an area, as it reflects the prevalence of different weight statuses and associated health risks.")
  })
  observeEvent(input$mentalhealth, {
    runjs(sprintf(
      "
    let viz = document.getElementById('tableauViz');
    let workbook =  viz.workbook;
    workbook.changeParameterValueAsync('Color By','Mental Health');
    workbook.changeParameterValueAsync('Mental_hea_par','Yes');
    "
    ))
    filter("MentalHeal")
    show_table('show')
    
    text("MentalHeal attribute focuses on the perceived mental health of individuals within an area, which is influenced by the availability of mental health support and resources. It takes into account factors such as the accessibility of mental health services, the presence of counseling and therapy resources, community programs promoting mental well-being, and the overall awareness and acceptance of mental health issues.")
  })
  observeEvent(input$school, {
    runjs(sprintf(
      "
    let viz = document.getElementById('tableauViz');
    let workbook =  viz.workbook;
    workbook.changeParameterValueAsync('Color By','Schooling');
    workbook.changeParameterValueAsync('Schooling_par','Yes');
    "
    ))
    filter("SchoolSES")
    show_table('show')
    
    text("The SES (Socioeconomic Status) score of a school offers important details on the socioeconomic backgrounds of its students. It considers elements including parental income, level of education, occupation, and pertinent demographics. Greater access to resources, educational support, and enrichment opportunities are frequently linked to higher SES ratings, and these relationships can improve academic achievement and future prospects.")
  })
  observeEvent(input$hospital, {
    runjs(sprintf(
      "
    let viz = document.getElementById('tableauViz');
    let workbook =  viz.workbook;
    workbook.changeParameterValueAsync('Color By','Hospital');
    workbook.changeParameterValueAsync('Hospital_par','Yes');
    "
    ))
    filter("Hospital")
    show_table('show')
    
    text("
The presence of hospitals in a suburb plays a vital role in enhancing its overall liveability and the well-being of its residents. Access to high-quality healthcare services is a fundamental aspect of a thriving community. When hospitals are conveniently located within a suburb, it ensures that individuals have timely access to medical care when they need it most.")
  })
  observeEvent(input$transport, {
    runjs(sprintf(
      "
    let viz = document.getElementById('tableauViz');
    let workbook =  viz.workbook;
    workbook.changeParameterValueAsync('Color By','Transportation');
    workbook.changeParameterValueAsync('Transport_par','Yes');
    "
    ))
    filter("Trans_opti")
    show_table('show')
    text("This Parameter,highlights the significance of public transportation accessibility in enhancing the liveability and sustainability of suburbs. By considering the presence and distribution of tram and bus stops, we gain insights into the level of connectivity and convenience offered to residents, ultimately contributing to a higher quality of life and a more sustainable urban environment.")
  })
  observeEvent(input$green, {runjs(sprintf(
    "
    let viz = document.getElementById('tableauViz');
    let workbook =  viz.workbook;
    workbook.changeParameterValueAsync('Color By','Green Cover');
    workbook.changeParameterValueAsync('Schooling_par','Yes');
    "
  ))
    filter("green_cove")
    show_table('show')
    
    text("The presence of green cover, such as parks, gardens, and natural vegetation, has a positive impact on the liveability of a suburb. Green spaces contribute to a healthier and more sustainable environment, promoting physical and mental well-being among residents.")
  })
  observeEvent(input$house, {
    runjs(sprintf(
      "
    let viz = document.getElementById('tableauViz');
    let workbook =  viz.workbook;
    workbook.changeParameterValueAsync('Color By','House Price');
    workbook.changeParameterValueAsync('House_Price_par','Yes');
    "
    ))
    filter("HousePrice")
    show_table('show')
    text("The House prices for a suburb can be a crucial aspect of liveablity in a suburb, these median house prices are calulated usig the property sales statistics generated by the Valuer-General for Victoria in 2021.")
  })
  
  
  info_val=reactiveVal('no_show')
  
  
  output$tableUI <- renderUI({
    
    if (show_table()== 'show') {
      reactableOutput("table")
    }
    else {
      NULL
    }
  })
  output$tableUI_1 <- renderUI({
    
    if (info_val()=='show') {
      reactableOutput("table_1")
    }
    else {
      NULL
    }
  })
  
  output$button_ui <- renderUI({
    if (show_table()== 'show' || info_val()== 'show') {
      div(
        actionButton("reset_button", "Back"),
        actionButton("info_button", "Information")
      )
    } else {
      
      actionButton("info_button", "Information")
      
    }
  })
  observeEvent(input$info_button, {
    greater_melb_polygon_temp=st_drop_geometry(greater_melb_polygon)
    if(!input$suburb== '' && !input$suburb== 'All'){
      info_val('show')
      sal_name=input$suburb
      filtered_data <- greater_melb_polygon_temp[greater_melb_polygon_temp$SAL_NAME_2 %in% sal_name , ]
      filtered_data <- filtered_data %>%
        gather(key = "Attributes", value = "scores")
      filtered_data <- filtered_data[-1, ]
      filtered_data$Attributes=c("Feel Safe Index", "Mental Health Score", "BMI (Mean Value)","House Price ($)", "Schooling","Transportation(Number of Trans Options)","Hospital","Green Cover Index","Livablity Score")
      filtered_data <- filtered_data %>%
        replace(is.na(.), 0)
      filtered_data$scores <- round(as.numeric(filtered_data$scores),4)
      output$table_1 <- renderReactable({ 
        filtered_data %>%reactable()
      })
    }
    else{
      output$text_ui <- renderUI({
        output$txt <- renderText({
          if (info_val()=='no_show'){
            "Please select a suburb to view its infomation"}
          else{
            NULL
          }
        })
        
        textOutput("txt")
      })
    }
  })
  
  
  output$table <- renderReactable({
    
    if (show_table()=='show'){
      
      
      table_data <- current_table()$x$data
      table_data <- table_data[-1]
      
      # Rename the first column to "Suburb Name" only if table_data is not empty
      if (!is.null(table_data) && ncol(table_data) > 0 && colnames(table_data)[1]=='SAL_NAME_2' ) {
        colnames(table_data)[1] <- "Suburb"
        table_data <- table_data %>%
          mutate(Suburb = str_to_title(Suburb))
      }
      col_names=colnames(table_data)
      
      if(!is.null(table_data) && ncol(table_data) > 0 && colnames(table_data)[2] == "BMI"){
        table_data[is.na(table_data)] <- 0
        table_data %>%
          reactable(
            columns = list(
              BMI = colDef(name="Body Mass Index (BMI)",cell = gauge_chart(
                data = .,
                fill_color = c('#D7191C','#FDAE61','#FFFFBF','#A6D96A','#1A9641'),
                number_fmt = scales::comma,
                bold_text = TRUE,
                text_size = 18,
                show_min_max = TRUE,
                min_value = 0,
                max_value = 50
              ))
            )
          )
        
      }
      else if(!is.null(table_data) && ncol(table_data) > 0 && colnames(table_data)[2] == "HousePrice") {
        table_data %>%
          reactable(
            columns = list(
              HousePrice =   colDef(
                name = 'Average House Price',
                align = 'left',
                minWidth = 200,
                format = colFormat(digits = 0),
                cell = data_bars(
                  data = table_data,
                  fill_color = c('#FAFAFA','#E7E7E7','#D3D3D3','#BFBFBF'),
                  fill_gradient = TRUE,
                  bold_text = TRUE,
                  background = 'transparent',
                  text_position = 'inside-base',
                  text_color = '#222222',
                  number_fmt = scales::dollar,
                  bar_height = 12,
                  icon = "house"
                  # img = 'https://www.pngkit.com/png/detail/1-12791_house-clipart-png-danielbentleyme-transparent-background-house-clipart.png',
                  # img_height = 20,
                  # img_width = 25
                )
              ))
          )
      }
      
      else if(!is.null(table_data) && ncol(table_data) > 0 && colnames(table_data)[2] == "SchoolSES") {
        
        table_data["SchoolSES"] <- table_data["SchoolSES"]
        
        
        table_data %>%
          reactable(
            columns = list(
              SchoolSES = colDef(
                name = 'School Socioeconomic Status',
                cell = data_bars(
                  data = .,
                  fill_color = c('#D7191C','#FDAE61','#FFFFBF','#b6e085','#1A9641'),
                  number_fmt = scales::label_comma(suffix = "\u00A0",prefix = "\u00A0"),
                  bold_text = TRUE,
                  min_value = 0,
                  max_value = 100
                ))
            )
          )
      }
      else if(!is.null(table_data) && ncol(table_data) > 0 && colnames(table_data)[2] == "green_cove") {
        
        table_data["green_cove"] <- round(table_data["green_cove"],2)
        
        table_data %>%
          reactable(
            columns = list(
              green_cove = colDef(
                name = "Green Cover (NDVI)",
                cell = data_bars(data = .,
                                 fill_color = c("navajowhite","green"),
                                 fill_gradient = TRUE,
                                 text_position = "outside-end",
                                 min_value = -1,
                                 max_value = 1
                )
              )
            )
          )
        
        
        
      }
      else if(!is.null(table_data) && ncol(table_data) > 0 && colnames(table_data)[2] == "MentalHeal") {
        
        table_data["MentalHeal"] <- table_data["MentalHeal"] * 10
        
        table_data %>%
          reactable(
            columns = list(
              MentalHeal = colDef(name = 'Mental Health Index',
                                  cell = icon_sets(table_data, 
                                                   icons = c("user"),
                                                   number_fmt = scales::label_comma(accuracy=0.01), 
                                                   colors = c("#E74C3C","#E74C3C")))
              
            )
          )
      }
      else if(!is.null(table_data) && ncol(table_data) > 0 && colnames(table_data)[2] == "FeelSafe"){
        
        table_data["FeelSafe"] <- round(table_data["FeelSafe"],2)
        table_data %>%
          reactable(
            columns = list(
              FeelSafe = colDef(name = 'Feel Safe Score (max 5)',
                                align = 'center',
                                cell = data_bars(
                                  data = .,
                                  text_position = "center",
                                  max_value = 5,
                                  min_value = 0
                                ))
            )
          )
      }
      else if(!is.null(table_data) && ncol(table_data) > 0 && colnames(table_data)[2] == "Trans_opti") {
        
        
        table_data %>%
          reactable(
            columns = list(
              Trans_opti =   colDef(
                name = 'Avaliable Transport Options (Trams and Bus stops)',
                align = 'left',
                minWidth = 200,
                format = colFormat(digits = 0),
                cell = data_bars(
                  data = table_data,
                  fill_color = c('#FAFAFA','#E7E7E7','#D3D3D3','#BFBFBF'),
                  fill_gradient = TRUE,
                  bold_text = TRUE,
                  background = 'transparent',
                  text_position = 'inside-base',
                  text_color = '#222222',
                  bar_height = 12,
                  icon = "car"
                )
                
              ))
          )
        
      }
      else if(!is.null(table_data) && ncol(table_data) > 0 && colnames(table_data)[2] == "Hospital") {
        
        table_data %>%
          reactable(
            columns = list(
              Hospital = colDef(
                cell = function(value) {
                  if (value==1) {
                    div(style = "color: green;", "Present")
                  } else {
                    div(style = "color: red;", "Absent")
                  }
                }
              )
            )
          )
        
      }
      else if(!is.null(table_data) && ncol(table_data) > 0 && colnames(table_data)[2] == "Livability") {
        table_data %>%
          reactable(
            columns = list(
              Livability = colDef(name = "Liveability", cell = function(val) {
                div(paste0(val*100, ' %'))
              })
            )
          )
      }
    } 
  })
  
  
  observeEvent(input$reset_button, {
    current_table(NULL)
    text("")
    filter(NULL)
    show_table('No_Show')
    info_val('no_show')
  })
  output$displayText =  renderText({
    if (show_table()=='show')
    {
      text()
    }
    else{
      NULL
    }
    
  })
  
  output$buttons_UI<-renderUI({
    
    if (show_table()=='No_Show' && info_val()=='no_show') {
      info_val('no_show')
      fluidPage(
        tags$head(
          tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
          tags$head(
            tags$script(src = "https://kit.fontawesome.com/7cc4234c90.js", crossorigin = "anonymous")
          ),
          tags$style(HTML('
      .clock-container {
        position: relative;
        width: 300px;
        height: 300px;
        margin: auto;
      }
      .factor_btn {
        position: absolute;
        font-size: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        margin: 5px; /* Adjust margin as needed */
        color: #E74C3C; /* Text color */
        border: 3px solid #E74C3C; /* Border color */
        background-color: transparent; /* Transparent background */
        border-radius:45%
      }
      
      .factor_btn:hover {
      
        background-color:#b0d9d6
      }
      .center-button {
        position: absolute;
        color: #fff; /* Text color */
        background-color: #84bdc6; /* Background color */
        border: 2px solid #fff; /* Border color */
        border-radius: 8px;
        font-size: 14px;
        display: flex;
        align-items: center;
        justify-content: center;
        top: 60%;
        left: 50%;
        transform: translate(-50%, -50%);
      }
    '))
        ),
        fluidRow(
          column(
            width = 12,
            div(class = "clock-container",
                actionButton("livability", HTML('<b>Liveability</b>'), class = "center-button"),
                actionButton("feelsafe",HTML('<i class="fa-solid fa-hand-holding-hand fa-beat"></i><br> &nbsp;Feel Safe'), class = "factor_btn", style="top: 5%; left: 50%; transform: translateX(-50%);"),
                actionButton("mentalhealth", HTML('<i class="fa-solid fa-user-doctor fa-beat"></i><br> &nbsp;Mental Health'), class = "factor_btn", style="top: 95%; left: 50%; transform: translateX(-50%);"),
                actionButton("school", HTML('<i class="fa-solid fa-graduation-cap fa-beat"></i><br> &nbsp;School'), class = "factor_btn", style="top: 50%; left: 95%; transform: translateX(-50%); color: #E67E22; border: 3px solid #E67E22;"),
                actionButton("hospital", HTML('<i class="fas fa-hospital fa-beat"></i><br> &nbsp;Hospital'), class = "factor_btn", style="top: 50%; left: 5%; transform: translateX(-50%); color: #E67E22; border: 3px solid #E67E22;"),
                actionButton("transport", HTML('<i class="fa-solid fa-train fa-beat"></i><br> &nbsp;Transport'), class = "factor_btn", style="top: 25%; left: 80%; transform: translateX(-50%); color: #34495E; border: 3px solid #34495E;"),
                actionButton("green", HTML('<i class="fa-solid fa-tree fa-beat"></i><br> &nbsp;Green cover'), class = "factor_btn", style="top: 75%; left: 20%; transform: translateX(-50%); color: #34495E; border: 3px solid #34495E;"),
                actionButton("bmi", HTML('<i class="fa-solid fa-weight-scale fa-beat"></i><br> &nbsp;BMI'), class = "factor_btn", style="top: 75%; left: 80%; transform: translateX(-50%); color: #9B59B6; border: 3px solid #9B59B6;"),
                actionButton("house", HTML('<i class="fa-solid fa-house fa-beat"></i><br> &nbsp;House price'), class = "factor_btn", style="top: 25%; left: 20%; transform: translateX(-50%); color: #9B59B6; border: 3px solid #9B59B6; ")
                
            )
          )
        )
      )
    }
    else if(info_val()=='show' || show_table() == 'show'){
      HTML("")
    }
    
    else {
      HTML("")
    }
    
  })
  
  
  observe({
    
    guide <- Cicerone$
      new()$ 
      step(
        el = "banner",
        title = "Melbourne Liveavility",
        description = "This dashboard shows melbourne livability using different parameters."
      )$
      step(
        el = "suburbinput",
        title = "Work Location",
        description = "Choose a suburb where you wish to work."
      )$
      step(
        el = "distanceinput",
        title = "Nearby Suburbs",
        description = "You can select radius to see others suburbs within the selected radius."
      )$
      step(
        "buttons_UI",
        "Liveability Factors",
        "You may select any of the factors to see top suburbs."
      )$
      step(
        "tableauViz",
        "Melbourne Map",
        "You may select any suburb/ multiple suburbs on this map."
      )
    
    
    guide$init()$start()
  })
  
  
  
}

shinyApp(ui, server, options=list(launch.browser=TRUE))
#shinyApp(ui, server)