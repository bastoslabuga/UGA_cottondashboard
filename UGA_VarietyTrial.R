# ==============================================================================
# UGA Cotton Rx Shiny Application
# ==============================================================================
# This application provides an interactive tool for cotton variety selection
# based on University of Georgia trial data across multiple counties and years.
# It includes variety performance rankings, trait comparisons, state-level maps,
# and environmental characterization analyses.

# Required Libraries
# ==============================================================================
library(shiny)          # Core Shiny framework
library(shinythemes)    # Bootstrap themes for Shiny (NOTE: May be redundant with bslib)
library(mapview)        # Interactive maps
library(leafpop)        # Popups for leaflet maps  
library(DT)             # Interactive data tables
library(leaflet)        # Interactive maps (primary mapping package)
library(tidyverse)      # Data manipulation and visualization
library(sf)             # Spatial data handling
library(plotly)         # Interactive plots
library(bslib)          # Bootstrap 5 themes (primary theming package)
library(readr)          # CSV reading

# Data Loading and Preprocessing
# ==============================================================================
# Load main cotton variety trial data
data <- read_csv("data/varietytrials_w_2023_24_loan.csv") %>% 
  # Split site column into county and year components
  separate(col = site, into = c("county", "year"), sep = "_") %>% 
  # Create ordered factor for response variables with user-friendly labels
  mutate(resp_var = factor(resp_var,
                           levels = c("lintyield_lbac", "lintyield_kgha", "gto", "mic", 
                                      "strength_gtex", "rd", "b", "length_in", "length_mm", 
                                      "uniformity", "Q_score", "loan_value"),
                           labels = c("Lint yield (lbs/ac)", "Lint yield (kg/ha)","GTO (%)", "Mic", 
                                      "Str (g/tex)", "Rd", "+b", "UHML (in)", "UHML (mm)", 
                                      "UI (%)", "Q Score", "Loan value (cts/lb)"))) %>% 
  # Sort by county for consistent ordering
  arrange(county)

# Load supplementary experiment data for detailed information table
supp_data <- read_csv("data/supp_data.csv") 

# Load spatial data for mapping
counties <- read_sf("data/shiny_map_georgia_new.geojson")      # All Georgia counties
counties_w <- read_sf("data/shiny_map_counties_new.geojson")   # Counties with trials

# Color Palette for Varieties
# ==============================================================================
# Consistent color scheme for cotton varieties across all visualizations
variety_colors <- c(
  "AR 9371 B3XF" = "#33A02C",    "AR 9831 B3XF" = "#FDBF6F",
  "CP 9608 B3XF" = "#66C2A5",    "DG 3528 B3XF" = "#8DD3C7",
  "DG 3615 B3XF" = "#FC8D62",    "DG 3799 B3XF" = "#8DA0CB",
  "DG H959 B3XF" = "#E78AC3",    "DP 1646 B2XF" = "#E41A1C",
  "DP 2038 B3XF" = "#377EB8",    "DP 2055 B3XF" = "#4DAF4A",
  "DP 2127 B3XF" = "#FF9E9E",    "DP 2333 B3XF" = "#B3DE69",
  "NG 3195 B3XF" = "#B3B3B3",    "NG 4190 B3XF" = "#6A3D9A",
  "NG 4936 B3XF" = "#FF7F00",    "NG 5430 B3XF" = "#CAB2D6",
  "NG 5711 B3XF" = "#984EA3",    "PHY 400 W3FE" = "#FFFF33",
  "PHY 545 W3FE" = "#E5C494",    "Px 5C45 W3FE" = "#A65628",
  "ST 4595 B3XF" = "#B15928",    "ST 4990 B3XF" = "#F781BF",
  "ST 5091 B3XF" = "#FFD92F",    "ST 5471 GLTP" = "#999999",
  "ST 6000 AXTP" = "#BC80BD"
)

# UI Definition
# ==============================================================================
ui <- page_fillable(
  title = "UGA Cotton Variety Rx",
  # Bootstrap 5 theme with custom primary color
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2C3E50"), 
  
  # Custom CSS Styling
  # ============================================================================
  tags$head(
    tags$style(HTML("
        /* Base font styling */
        body { font-family: 'Inter', sans-serif; } 
        
        /* Mobile responsiveness */
        @media (max-width: 768px) {
            .card { margin: 5px 0; }
            .selectize-input { min-height: 44px; }
            .nav-link { padding: 15px 10px; }
            .lab-title { font-size: 18px; }
        }
        
        /* Header styling */
        .title-bar {
          display: flex;
          align-items: center;
          justify-content: space-between;
          position: relative;
          padding: 10px 20px;
          background-color: #fff;
          border-bottom: 1px solid #dee2e6;
        }
        
        .logo-left, .logo-right {
          flex: 0 0 auto;
        }
        
        .title-center {
          position: absolute;
          left: 50%;
          transform: translateX(-50%);
          text-align: center;
          white-space: nowrap;
          pointer-events: none; /* Prevents overlap with clickable side content */
        }
        
        .lab-title {
          margin: 0;
          font-size: 26px;
          font-weight: bold;
          color: #2C3E50;
        }

        .title-content {
            display: flex;
            align-items: center;
            gap: 20px;
        }
        .lab-title { 
            margin: 0; 
            font-size: 30px; 
            font-weight: bold;
            color: #2C3E50;
        }
        .logo-img { 
            height: 70px;
            width: auto; 
        }
        
        /* Interactive elements styling */
        .plotly-annotation-text {
           white-space: pre-line !important; 
        }
        .list-group-item {
            border-left: 0;
            border-right: 0;
        }
        .list-group-item strong { color: #2C3E50; }
    "))
  ),
  
  # Application Header
  # ============================================================================
  div(class = "title-bar",
      div(class = "logo-left",
          img(src = "logo_uga.png", class = "logo-img", alt = "UGA Logo",
              onerror = "this.src='https://placehold.co/150x35/CCCCCC/333333?text=UGA+Logo+Error&font=inter'")
      ),
      div(class = "title-center",
          h1("University of Georgia Cotton Variety Rx", class = "lab-title")
      ),
      div(class = "logo-right",
          img(src = "logo.png", class = "logo-img", alt = "Bastos Lab Logo",
              onerror = "this.src='https://placehold.co/100x35/CCCCCC/333333?text=Lab+Logo+Error&font=inter'")
      )
  ),
  # Navigation Bar with Multiple Panels
  # ============================================================================
  navset_bar(
    
    # Welcome Page Panel
    # ==========================================================================
    nav_panel(
      title = "Welcome Page",
      icon = icon("house"),
      layout_columns(
        col_widths = c(6, 6),
        # Welcome Information Card
        card(
          height = "600px",
          card_header("Welcome to the UGA Cotton Variety Rx", class = "bg-primary text-white"),
          card_body(
            p("Selecting a cotton variety is one of the most critical decisions a grower has to make."),
            p("This tool was developed to help growers and consultants make that decision based on data collected in their counties."),
            p("The data used behind this tool includes a total of", tags$b("117 trials"), "conducted across", tags$b("5 years (2020-2024)"), "and", tags$b("35 Georgia counties"), "evaluating a total of", tags$b("25 commercial varieties.")),
            h4("Available Tools (as tabs on top menu):", class = "mt-4"),
            # List of application features
            tags$ul(class = "list-group list-group-flush",
                    tags$li(class = "list-group-item", tags$strong("Your County: "), "Get a summary of trials conducted in your county."),
                    tags$li(class = "list-group-item", tags$strong("Variety Rx: "), "Get variety recommendations based on your county and irrigation status."),
                    tags$li(class = "list-group-item", tags$strong("Trait Comparison: "), "Compare the top-yielding varieties for your environment across different fiber quality traits."),
                    tags$li(class = "list-group-item", tags$strong("State-level Performance: "), "Explore how a given variety performed across all trials in the state."),
                    tags$li(class = "list-group-item", tags$strong("Environment Ranking: "), "Learn how your county and irrigation status ranked compared to all trials."),
                    tags$li(class = "list-group-item", tags$strong("List of Trials: "), "Access detailed information about each trial."))
          )
        ),
        # Interactive Map Card
        card(height = "600px",
             card_header("Map of counties and years with trials across Georgia", class = "bg-primary text-white"), 
             leafletOutput("map", height = "550px")
        )
      )
    ),
    
    # Your County Panel
    # ==========================================================================
    nav_panel(
      title = "Your County",
      icon = icon("list-check"),
      layout_sidebar(
        # Sidebar with input controls
        sidebar = sidebar(
          tags$p("On this tab, find out how many trials and varieties have been tested in your county.",
                 class = "text-muted mb-3"),
          tags$p("Below, select your county to view trial details.",
                 class = "text-muted mb-2"),
          # County selection dropdown
          selectInput("county_sel", 
                      "Select County:", 
                      choices = c("Please select a county" = "", unique(data$county)),
                      selected = "") %>%
            tagAppendAttributes(style = "background-color: #FFFF00;"),
        ),
        # Main content area with two cards
        layout_columns( 
          col_widths = c(4, 8),
          # County summary information
          card(
            height = "400px",
            card_header("County Summary"),
            htmlOutput("county_summary")
          ),
          
          # County map
          card(
            height = "350px",
            card_header("County Map"),
            layout_column_wrap( 
              width = "100%",
              #tags$div("Test", class = "mb-0 mt-0 text-muted"),
              leafletOutput("yourcounty_map", height = "300px")
            ) 
          ),
        )
      )
    ),    
    
    # VARIETY Rx Panel
    # ==========================================================================
    nav_panel(
      title = "Variety Rx",
      icon = icon("cotton-bureau"),
      layout_sidebar(
        # Sidebar with input controls
        sidebar = sidebar(
          tags$p("On this tab, explore variety performance rankings for your county, irrigation status, and variable of interest.",
                 class = "text-muted mb-3"),
          tags$p("If you would like to change your County selection, go back to previous tab (Your County).",
                 class = "text-muted mb-2"),
          # County selection  
          selectInput("county_c1", 
                      "Selected County:", 
                      choices = c("Selected county" = ""),
                      selected = ""),
          tags$script(HTML("$('#county_c1').prop('disabled', true);")),
          tags$p("Below, select your irrigation status and variable of interest to view variety performance.",
                 class = "text-muted mb-2"),
          
          # Irrigation selection dropdown
          selectInput(
            inputId = "irrigation_sel", 
            label = "Select Irrigation:", 
            choices = c("Please select irrigation" = ""),
            selected = ""
          ) %>%
            tagAppendAttributes(style = "background-color: #FFFF00;"),
          # Variable selection dropdown
          selectInput("variable_sel", 
                      "Select Variable:", 
                      choices = c("Please select variable" = "", unique(as.character(data$resp_var))),
                      selected = "") %>%
            tagAppendAttributes(style = "background-color: #FFFF00;"),
        ),
        # Main content area with two cards
        layout_columns( 
          col_widths = c(6, 6),
          # Variety Performance Rankings Card
          card(
            height = "350px",
            card_header("Variety average relative ranking (% of maximum)"),
            layout_column_wrap( 
              width = "100%",
              #tags$div("Test", class = "mb-0 mt-0 text-muted"),
              plotlyOutput("plot3_1", height = "300px")
            ) 
          ),
          # Trial Results by Year Card
          card(
            height = "400px",
            card_header("Variety year-specific ranking"),
            layout_column_wrap(
              width = "100%",
              # Year selector for specific trial results
              div(
                style = "margin-bottom: 0px; 
                padding: 6px 10px; 
                background-color: #f8f9fa; 
                border-radius: 5px;",
                selectInput("year_sel_trial", 
                            "Select Year:", 
                            choices = NULL, # Updated dynamically based on other selections
                            selected = NULL,
                            width = "200px") %>%
                  tagAppendAttributes(style = "background-color: #FFFF00;")
              ),
              # Single trial plot output
              div(
                style = "margin-top: 0px;",
                plotlyOutput("single_trial_plot", height = "300px")
              ),
            )
          )
        )
      )
    ),
    
    # TRAIT COMPARISON Panel
    # ==========================================================================
    nav_panel(
      title = "Trait Comparison",
      icon = icon("chart-line"),
      layout_sidebar(
        sidebar = sidebar(
          tags$p("On this tab, explore how the top 5 yielding varieties for your county and irrigation status compare across fiber quality traits.",
                 class = "text-muted mb-3"),
          tags$p("If you would like to change your County and Irrigation selections, go back to previous tabs (Your County and Variety Rx).",
                 class = "text-muted mb-2"),
          # County selection  
          selectInput("county_c2", 
                      "Selected County:", 
                      choices = c("Selected county" = ""),
                      selected = ""),
          tags$script(HTML("$('#county_c2').prop('disabled', true);")),
          # Irrigation selection  
          selectInput(
            inputId = "irrigation_c1", 
            label = "Selected Irrigation:", 
            choices = c("Selected irrigation" = ""),
            selected = ""
          ),
          tags$script(HTML("$('#irrigation_c1').prop('disabled', true);")),
        ),
        # Parallel coordinates plot for top 5 varieties
        card(
          card_header("Top 5 yielding varieties for selected County and Irrigation compared across fiber quality traits"),
          plotlyOutput("parallel_plot")
        )
      )
    ),
    
    # STATE-LEVEL PERFORMANCE Panel
    # ==========================================================================
    nav_panel(
      title = "State-level Performance",
      icon = icon("map"),
      layout_sidebar(
        # Control sidebar
        sidebar = sidebar(
          tags$p("On this tab, explore how a selected variety performed across all counties for a selected year and variable.",
                 class = "text-muted mb-3"),
          tags$p("If you would like to change your Irrigation and Variable selections, go back to previous tab (Variety Rx).",
                 class = "text-muted mb-2"),
                 # Irrigation selection  
          selectInput(
            inputId = "irrigation_c2", 
            label = "Selected Irrigation:", 
            choices = c("Selected irrigation" = ""),
            selected = ""
          ),
          tags$script(HTML("$('#irrigation_c2').prop('disabled', true);")),
          
          selectInput("variable_c1", 
                      "Selected Variable:", 
                      choices = c("Selected variable" = ""),
                      selected = ""),
          tags$script(HTML("$('#variable_c1').prop('disabled', true);")),
          
          tags$p("Below, select your variety of choice and a specific year.",
                 class = "text-muted mb-2"),
          selectInput("variety_sel", 
                      "Select Variety:",
                      choices = c("Please select variety" = "", sort(unique(data$variety))),
                      selected = "") %>%
            tagAppendAttributes(style = "background-color: #FFFF00;"),
          
          selectInput("year_sel", 
                      "Select Year:", 
                      choices = c("Please select year" = "", sort(unique(data$year))),
                      selected = "") %>%
            tagAppendAttributes(style = "background-color: #FFFF00;"),
          
        ),
        # Main content with summary and map
        layout_columns( 
          #col_widths = c(8, 8),
          # Interactive map showing variety performance
          card(
            height = "600px",
            card_header(
              "Top Variety Performance Map",
              tags$span(
                class = "text-muted",
                style = "font-size: 0.8rem; display: block;", 
                "Performance of the selected variety across Georgia counties in selected Year"
              )
            ),
            leafletOutput("map_2", height = "500px")
          )
        )
      )
    ),
    
    # ENVIRONMENTAL CHARACTERIZATION Panel
    # ==========================================================================
    nav_panel(
      title = "Environment Ranking", 
      icon = icon("gauge-simple"),
      layout_sidebar(
        sidebar = sidebar(
          tags$p("On this tab, explore how trials in your county ranked across all trials in the state for the selected variable.",
                 class = "text-muted mb-3"),
          tags$p("If you would like to change your Variable and County selections, go back to previous tabs (Your County and Variety Rx).",
                 class = "text-muted mb-2"),
                 selectInput("variable_c2", 
                      "Selected Variable:", 
                      choices = c("Selected variable" = ""),
                      selected = ""),
          tags$script(HTML("$('#variable_c2').prop('disabled', true);")),
          selectInput("county_c3", 
                      "Selected County:", 
                      choices = c("Selected county" = ""),
                      selected = ""),
          tags$script(HTML("$('#county_c3').prop('disabled', true);")),
        ),
        # Environmental comparison visualization
        card(card_header("Results in different environments across 5 years"), 
             layout_column_wrap(width = "100%", heights_equal = "row", 
                                style = css(grid_template_columns = "repeat(auto-fit, minmax(300px, 1fr))"), 
                                plotlyOutput("plot1_2", height = "2000px") # Large height for many trials
             )
        )
      )
    ),
    
    # EXPERIMENTS DATABASE Panel
    # ==========================================================================
    nav_panel(
      title = "List of Trials", 
      icon = icon("calendar-days"),
      card(
        card_header("List and main condition of trials"), 
        DTOutput("table_supp") # Interactive data table
      )
    ),
    
    # ABOUT Panel
    # ==========================================================================
    nav_panel(
      title = "About", 
      icon = icon("info-circle"),
      card(
        card_header("About the University of Georgia Cotton Variety Rx"),
        card_body(
          h3("About the project", class = "mb-4"), 
          p("In this dashboard, we analyzed cotton variety performance specifically for the state of Georgia. The analysis provides rankings based on trial data, enabling us to assess the performance of each variety across Georgia's diverse agricultural conditions."),
          p("You can find further information visiting the following page: ", 
            tags$a(href="https://www.sciencedirect.com/science/article/pii/S0378429025000875", 
                   "Cotton lint yield and quality variability in Georgia, USA: Understanding genotypic and environmental interactions", 
                   target = "_blank")), 
          
          # Development team information
          h4("Dasboard Development", class = "mt-4"), 
          p("Dr. Gonzalo Scarpin, Post-Doctoral Scientist at UGA"), 
          p("Dr. Leonardo Bastos, Assistant Professor of Integrative Precision Agriculture, UGA"),
          
          h4("On-Farm Cotton Variety Evaluation Program", class = "mt-4"), 
          p("Program Coordinator:"), 
          p("Dr. Camp Hand, Assistant Professor and Cotton Extension Specialist, UGA"),
          
          # Acknowledgments
          h4("County Extension Agents", class = "mt-4"), 
          p("We thank all UGA county extension agents that lead each one of the trials"),
          
          h4("Participating Growers", class = "mt-4"), 
          p("We extend our sincere gratitude to all growers for being part of several trials across the state"),
          
          # Funding information
          h4("Funding and Support", class = "mt-4"), 
          p("This project was supported by:"), 
          tags$ul(
            tags$li("Georgia Cotton Commission"), 
            tags$li("University of Georgia Cooperative Extension Service")
          ),
          
          # Contact information
          h4("Contact Information", class = "mt-4"), 
          p("For questions about this dashboard please do not hesitate to contact Dr. Gonzalo Scarpin:"), 
          p(HTML(paste("Email:", tags$a(href="mailto:gjscarpin@uga.edu", "gjscarpin@uga.edu"))))
        )
      )
    )
  )
)

# Server Logic
# ==============================================================================
server <- function(input, output, session) {
  
  # Input Synchronization Observers
  # ============================================================================
  # These observers ensure that selections in one panel update related inputs in other panels
  # Sync county selection across panels
  observe({
    if (!is.null(input$county_sel) && input$county_sel != "") {
      updateSelectInput(session, "county_c1", choices = c("Selected county" = "", input$county_sel), selected = input$county_sel)
      updateSelectInput(session, "county_c2", choices = c("Selected county" = "", input$county_sel), selected = input$county_sel)
      updateSelectInput(session, "county_c3", choices = c("Selected county" = "", input$county_sel), selected = input$county_sel)
    }
  })
  
  # Sync variable selection across panels
  observeEvent(input$variable_sel, {
    if (input$variable_sel != "") {
      updateSelectInput(session, "variable_c1", choices = c("Selected variable" = "", input$variable_sel), selected = input$variable_sel)
      updateSelectInput(session, "variable_c2", choices = c("Selected variable" = "", input$variable_sel), selected = input$variable_sel)
    }
  })
  
  # Sync irrigation selection across panels
  observe({
    if (!is.null(input$irrigation_sel) && input$irrigation_sel != "") {
      updateSelectInput(session, "irrigation_c1", choices = c("Selected irrigation" = "", input$irrigation_sel), selected = input$irrigation_sel)
      updateSelectInput(session, "irrigation_c2", choices = c("Selected irrigation" = "", input$irrigation_sel), selected = input$irrigation_sel)
      
    }
  })
  
  # Reactive expression: get irrigation choices for selected county
  filtered_irrigation <- reactive({
    req(input$county_sel)
    sort(unique(data$irrigation[data$county == input$county_sel]))
  })
  
  # Reactive expression: get year choices for selected variety
  filtered_year <- reactive({
    req(input$variety_sel)
    sort(unique(data$year[data$variety == input$variety_sel]))
  })
  
  # Observer: update irrigation dropdown based on county
  observeEvent(input$county_sel, {
    updateSelectInput(
      inputId = "irrigation_sel",
      choices = c("Please select irrigation" = "", filtered_irrigation()),
      selected = ""
    )
  })
  
  # Observer: update year dropdown based on variety
  observeEvent(input$variety_sel, {
    updateSelectInput(
      inputId = "year_sel",
      choices = c("Please select year" = "", filtered_year()),
      selected = ""
    )
  })
  
  # Dynamic Year Selection for Trial Results
  # ============================================================================
  # Updates available years based on county, irrigation, and variable selections
  observe({
    req(input$county_sel, input$irrigation_sel, input$variable_sel)
    if (input$county_sel == "" || input$irrigation_sel == "") {
      updateSelectInput(session, "year_sel_trial", choices = NULL, selected = NULL)
      return()
    }
    
    # Get available years for the selected combination
    available_years <- data %>%
      filter(
        county == input$county_sel,
        irrigation == input$irrigation_sel,
        resp_var == input$variable_sel
      ) %>%
      distinct(year) %>%
      arrange(desc(year)) %>% # Sort descending so most recent is first
      pull(year)
    
    if (length(available_years) > 0) {
      # Update choices and select most recent year by default
      updateSelectInput(session, "year_sel_trial", 
                        choices = available_years, 
                        selected = "")
    } else {
      updateSelectInput(session, "year_sel_trial", choices = NULL, selected = NULL)
    }
  })
  
  
  # Main Map Rendering (Home Page)
  # ============================================================================
  output$map <- renderLeaflet({
    # Ensure counties_w is sf object
    counties_w_sf <- if (!inherits(counties_w, "sf")) {
      st_as_sf(counties_w)
    } else {
      counties_w
    }
    
    # Join county information with trial data
    counties_info <- counties %>%
      left_join(
        counties_w_sf %>% 
          st_drop_geometry() %>% 
          select(name, nyear), 
        by = "name"
      ) %>%
      mutate(
        # Create popup text for map interactions
        popup_text = sprintf(
          "<div style='min-width: 150px; font-family: Inter, sans-serif;'><b>County:</b> %s<br><b>Number of Trials:</b> %s</div>",
          name,
          ifelse(is.na(nyear), "No trials", nyear)
        )
      )
    
    # Create base map layer (all counties)
    map1 <- mapview(
      counties_info, 
      alpha = 0.5, 
      col.regions = "grey", 
      popup = lapply(counties_info$popup_text, htmltools::HTML), 
      label = counties_info$name,
      legend = FALSE
    )
    
    # Create overlay layer (counties with experiments)
    map2 <- mapview(
      counties_w_sf, 
      zcol = "nyear", 
      popup = lapply(sprintf( 
        "<div style='min-width: 150px; font-family: Inter, sans-serif;'><b>County:</b> %s<br><b>Number of Trials:</b> %d</div>",
        counties_w_sf$name, 
        counties_w_sf$nyear
      ), htmltools::HTML),
      label = counties_w_sf$name,
      layer.name = "Number of trials"
    )
    
    # Combine layers and return map
    (map1 + map2)@map
  })
  
  # Your County Plot 
  # ============================================================================
  # Prepare your county map data
  prepare_yourcounty_map_data <- reactive({
    req(input$county_sel)
    counties %>% 
      rename(county = name) %>%
      mutate(county = str_remove_all(county, " ")) %>%
      filter(county == input$county_sel)
  })
  
  # Your County map
  output$yourcounty_map <- renderLeaflet({
    #req(prepare_yourcounty_map_data())
    
    # Data
    all_counties <- counties %>%
      rename(county = name) %>%
      mutate(county = str_remove_all(county, " "))
    
      # Make sure this is an sf object
    selected_county <- prepare_yourcounty_map_data()
    
    # Get bounding box of selected county
    centroid <- selected_county %>%
      st_centroid() %>%
      st_coordinates()

    # Render map
    map1 <- mapview(
      all_counties, 
      alpha = 0.5, 
      col.regions = "grey", 
      popup = NULL, 
      layer.name = "GA counties",
      legend = FALSE
    )
    
    # Create overlay layer (counties with experiments)
    map2 <- mapview(
      selected_county, 
      popup = NULL,
      col.regions = "red",
      alpha.regions = 0.5,
      layer.name = "Your county"
    )
    
    # Combine layers and return map
    finalyourcountymap <- (map1 + map2)@map %>%
      setView(centroid[[1]], 
              centroid[[2]],
              zoom = 7) 
    
    finalyourcountymap
  })
  
  
  # Variety Rankings Plot (Relative Performance)
  # ============================================================================
  output$plot3_1 <- renderPlotly({
    # Check if required inputs are selected
    if (is.null(input$county_sel) || input$county_sel == "" || 
        is.null(input$irrigation_sel) || input$irrigation_sel == "" || 
        is.null(input$variable_sel) || input$variable_sel == "") {
      # Return informational message for initial state
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = paste(
                  "<b>Variety Rankings</b><br>",
                  "Select Irrigation and Variable<br>",
                  "to view rankings."
                ),
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, align = "center",
                font = list(size = 12, color = "#333333"),
                bordercolor = "#2C3E50", borderwidth = 1, borderpad = 8,
                bgcolor = "#f0f8ff", opacity = 0.9
              )
            ),
            plot_bgcolor = "#f8f9fa", paper_bgcolor = "#f8f9fa",
            xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
            yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
            margin = list(l=10, r=10, b=10, t=10, pad=4)
          )
      )
    }
    
    # Filter data based on user selections
    filtered_data <- data %>%
      filter(
        county == input$county_sel,
        irrigation == input$irrigation_sel,
        resp_var == input$variable_sel
      )
    
    # Check if filtered data exists
    if(nrow(filtered_data) == 0) {
      no_data_message <- paste0(
        "<b>No Ranking Data Found</b><br>",
        "For: ", htmltools::htmlEscape(input$county_sel), ", ",
        htmltools::htmlEscape(input$irrigation_sel), ", ",
        htmltools::htmlEscape(input$variable_sel), "<br>",
        "<i>Try different selections.</i>"
      )
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = no_data_message,
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, align = "center",
                font = list(size = 12, color = "#495057"),
                bordercolor = "#dc3545", borderwidth = 1, borderpad = 8,
                bgcolor = "#fff0f1", opacity = 0.9
              )
            ),
            plot_bgcolor = "#f8f9fa", paper_bgcolor = "#f8f9fa",
            xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
            yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
            margin = list(l=10, r=10, b=10, t=10, pad=4)
          )
      )
    }
    
    # Calculate relative performance: each variety's value relative to the maximum value in each year
    ranking_df <- filtered_data %>%
      group_by(year) %>%
      mutate(
        max_value_year = max(resp_val, na.rm = TRUE),
        relative_performance = ifelse(max_value_year > 0, (resp_val / max_value_year) * 100, NA_real_)
      ) %>%
      ungroup() %>%
      group_by(variety) %>%
      summarize(
        mean_relative_performance = mean(relative_performance, na.rm = TRUE),
        mean_absolute_value = mean(resp_val, na.rm = TRUE), # Keep absolute value for reference
        n_obs = n(),
        .groups = "drop" 
      ) %>%
      filter(n_obs > 0 & !is.na(mean_relative_performance)) %>% 
      arrange(desc(mean_relative_performance))
    
    # Double-check if ranking data exists after calculations
    if(nrow(ranking_df) == 0) {
      no_data_message <- paste0(
        "<b>No ranking data found</b><br>",
        "For: ", htmltools::htmlEscape(input$county_sel), ", ",
        htmltools::htmlEscape(input$irrigation_sel), ", ",
        htmltools::htmlEscape(input$variable_sel), "<br>",
        "<i>Try different selections.</i>"
      )
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = no_data_message,
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, align = "center",
                font = list(size = 12, color = "#495057"),
                bordercolor = "#dc3545", borderwidth = 1, borderpad = 8,
                bgcolor = "#fff0f1", opacity = 0.9
              )
            ),
            plot_bgcolor = "#f8f9fa", paper_bgcolor = "#f8f9fa",
            xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
            yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
            margin = list(l=10, r=10, b=10, t=10, pad=4)
          )
      )
    }
    
    # Format labels based on variable type
    label_format_relative <- function(x) paste0(round(x, 1), "%")
    label_format_absolute <- if(input$variable_sel == "Lint yield (lbs/ac)") {
      function(x) round(x, 0)
    } else {
      function(x) round(x, 2)
    }
    
    # Create the ranking plot
    p_rank <- ggplot(ranking_df, 
                     aes(x = mean_relative_performance, 
                         y = reorder(variety, mean_relative_performance), 
                         text = paste(
                           "<b>Variety:</b>", variety,
                           "<br><b>Relative Performance:</b>", label_format_relative(mean_relative_performance),
                           "<br><b>Mean", input$variable_sel, ":</b>", label_format_absolute(mean_absolute_value),
                           "<br><b>Observations:</b>", n_obs
                         ))) +
      geom_col(aes(fill = variety), width = 0.8) + 
      geom_text(aes(label = label_format_relative(mean_relative_performance)),
                position = position_stack(vjust = 0.5),
                hjust = -0.2, 
                size = 3.5,
                color = "black", fontface = "bold") +
      scale_fill_manual(values = variety_colors) +
      labs(
        x = paste("Average relative performance (% of yearly max)"),
        y = "Variety",
        title = NULL # Title handled by card header
      ) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        plot.title = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_line(linetype = "dotted", color = "gray80"),
        plot.margin = margin(5, 5, 5, 15)
      ) +
      coord_cartesian(xlim = c(0, max(ranking_df$mean_relative_performance, na.rm = TRUE) * 1.15)) 
    
    # Convert to interactive plot
    ggplotly(p_rank, tooltip = "text") %>% 
      layout(autosize = TRUE, height = NULL, margin = list(t = 5, b = 30, l = 5, r = 5))
  })
  
  # Single Trial Plot for Selected Year
  # ============================================================================
  output$single_trial_plot <- renderPlotly({
    req(input$county_sel, input$irrigation_sel, input$variable_sel, input$year_sel_trial)
    
    if (input$county_sel == "" || input$irrigation_sel == "" || is.null(input$year_sel_trial)) {
      return(div(
        style = "text-align: center; padding: 50px;",
        h4("Select County, Irrigation and Year to view trial results", style = "color: #6c757d;")
      ))
    }
    
    # Generate plot for the selected year
    generate_trial_plot(input$year_sel_trial, input$county_sel, input$irrigation_sel, input$variable_sel)
  })
  
  # Function to Generate Trial Plot for Specific Year
  # ============================================================================
  generate_trial_plot <- function(year_val, county_sel, irrigation_sel, variable_sel) {
    plot_data <- data %>% 
      filter(
        irrigation == irrigation_sel, 
        resp_var == variable_sel, 
        county == county_sel, 
        year == year_val
      ) %>%
      filter(!is.na(resp_val)) 
    
    if (nrow(plot_data) == 0) {
      return(
        plot_ly() %>% 
          layout(
            annotations = list(
              list(
                text = paste0("Data not available for ", year_val), 
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, align = "center",
                font = list(size = 11, color = "#6c757d"), 
                bgcolor = "#f8f9fa", borderpad = 8
              )
            ),
            plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
            xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
            yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
            title = list(text = paste(year_val, "-", variable_sel), font = list(size=10)), 
            margin = list(l=5, r=5, b=5, t=20, pad=2) 
          )
      )
    }
    
    mean_value_overall <- mean(plot_data$resp_val, na.rm = TRUE)
    
    p_1_var <- plot_data %>% 
      mutate(ranking = rank(-resp_val, ties.method = "min")) %>%
      ggplot(aes(x = reorder(as.factor(variety), -resp_val), 
                 y = resp_val, 
                 text = paste(
                   "<b>Variety:</b>", variety,
                   "<b>Value:</b>", round(resp_val, 2),
                   "<br><b>Rank:</b>", ranking
                 ))) +
      geom_col(aes(fill = variety), width = 0.7) + 
      geom_hline(yintercept = mean_value_overall, 
                 color = "firebrick", 
                 linetype = "dashed", size = 0.8) +
      scale_fill_manual(values = variety_colors) +
      theme_minimal(base_size = 9) + 
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7), 
        axis.text.y = element_text(size = 8), 
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        plot.margin = margin(5, 5, 5, 5), 
        legend.position = "none",
        panel.grid.major.x = element_blank()
      ) +
      labs(title = paste(year_val, "-", variable_sel), 
           x = "Variety", 
           y = variable_sel)
    
    ggplotly(p_1_var, tooltip = "text") %>% 
      layout(
        autosize = TRUE, height = NULL, 
        margin = list(l = 50, r = 10, t = 30, b = 60) 
      )
  }
  
  # Top varieties based on Lint Yield
  top_ly_varieties <- reactive({
    req(input$county_sel, input$irrigation_sel)
    if (input$county_sel == "" || input$irrigation_sel == "") return(character(0)) 
    
    filtered_data <- data %>%
      filter(
        county == input$county_sel,
        irrigation == input$irrigation_sel,
        resp_var == "Lint yield (lbs/ac)"
      )
    
    ranking_df <- filtered_data %>%
      group_by(year) %>%
      mutate(
        max_value_year = max(resp_val, na.rm = TRUE),
        relative_performance = ifelse(max_value_year > 0, (resp_val / max_value_year) * 100, NA_real_)
      ) %>%
      ungroup() %>%
      group_by(variety) %>%
      summarize(
        mean_relative_performance = mean(relative_performance, na.rm = TRUE),
        n_obs = n(),
        .groups = "drop"
      ) %>%
      filter(n_obs > 0 & !is.na(mean_relative_performance)) %>% 
      arrange(desc(mean_relative_performance)) %>%
      slice_head(n = 5)
    
    ranking_df$variety
  })
  # Compute fitted values for parallel plot
  computed_fitted_values <- reactive({
    req(length(top_ly_varieties()) > 0, input$county_sel, input$irrigation_sel) 
    
    traits_for_comparison <- c("Lint yield (lbs/ac)", "GTO (%)", "UHML (mm)", "Str (g/tex)", "Mic", "UI (%)")
    top5_varieties <- top_ly_varieties()
    
    map_dfr(traits_for_comparison, function(current_trait) {
      map_dfr(top5_varieties, function(current_variety) {
        value_data <- data %>%
          filter(
            resp_var == current_trait,
            variety == current_variety,
            county == input$county_sel,
            irrigation == input$irrigation_sel
          ) %>%
          summarize(mean_value = mean(resp_val, na.rm = TRUE), .groups = "drop")
        
        tibble(
          variety = current_variety,
          trait = current_trait,
          fitted_value = if(nrow(value_data) > 0) value_data$mean_value else NA_real_
        )
      })
    })
  })
  # Prepare data for parallel plot
  parallel_data_prep <- reactive({
    df_fitted <- computed_fitted_values()
    if (nrow(df_fitted) == 0) return(tibble())
    
    max_per_trait <- df_fitted %>%
      filter(!is.na(fitted_value)) %>% 
      group_by(trait) %>%
      summarize(max_fitted = max(fitted_value, na.rm = TRUE), .groups = "drop")
    
    df_fitted %>%
      left_join(max_per_trait, by = "trait") %>%
      mutate(
        relative_value = ifelse(is.na(fitted_value) | is.na(max_fitted) | max_fitted == 0, 
                                NA_real_, 
                                (fitted_value / max_fitted) * 100),
        actual_value = fitted_value,
        trait = factor(trait, levels =  c("Lint yield (lbs/ac)", "GTO (%)", "UHML (mm)", "Str (g/tex)", "Mic", "UI (%)"))
      ) %>%
      select(variety, trait, actual_value, relative_value)
  })
  # Check for missing traits
  missing_traits_info <- reactive({
    req(length(top_ly_varieties()) > 0) 
    
    df <- parallel_data_prep()
    if (nrow(df) == 0) {
      return(list(general_message = paste(
        "<b>Insufficient data for trait comparison</b><br><br>",
        "Could not retrieve data for top varieties based on selections in 'Variety Rx' tab:<br>",
        "<b>County:</b> ", htmltools::htmlEscape(input$county_sel), "<br>",
        "<b>Irrigation:</b> ", htmltools::htmlEscape(input$irrigation_sel), "<br><br>",
        "<i>Please ensure selections yield data for Lint yield to identify top varieties.</i>"
      )))
    }
    
    top_varieties_list <- top_ly_varieties()
    all_traits_list <- levels(df$trait)
    missing_summary <- list()
    
    for (v in top_varieties_list) {
      variety_data <- df %>% filter(variety == v)
      missing_for_variety <- character(0)
      for (t_level in all_traits_list) {
        trait_entry <- variety_data %>% filter(trait == t_level)
        if (nrow(trait_entry) == 0 || is.na(trait_entry$actual_value)) {
          missing_for_variety <- c(missing_for_variety, as.character(t_level))
        }
      }
      if (length(missing_for_variety) > 0) {
        missing_summary[[v]] <- missing_for_variety
      }
    }
    
    if (length(missing_summary) > 0) {
      return(list(specific_missing = missing_summary))
    } else if (all(!is.na(df$actual_value))) { 
      return(list(all_data_present = TRUE))
    } else { 
      return(list(general_message = paste(
        "<b>Data gaps in trait comparison</b><br><br>",
        "Some trait data might be unavailable for the selected top varieties from:<br>",
        "<b>County:</b> ", htmltools::htmlEscape(input$county_sel), "<br>",
        "<b>Irrigation:</b> ", htmltools::htmlEscape(input$irrigation_sel), "<br><br>",
        "<i>The plot will show available data. Hover over points for details.</i>"
      )))
    }
  })
  # Parallel coordinates plot
  output$parallel_plot <- renderPlotly({
    info <- missing_traits_info()
    
    if (!is.null(info$general_message)) {
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = info$general_message,
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, align = "center",
                font = list(size = 14, color = "#495057"),
                bordercolor = "#ffc107", borderwidth = 1, borderpad = 10,
                bgcolor = "#fff8e1", opacity = 0.9
              )
            ),
            plot_bgcolor = "#f8f9fa", paper_bgcolor = "#f8f9fa",
            xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
            yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE)
          )
      )
    }
    
    p_data <- parallel_data_prep()
    if(nrow(p_data) == 0 || all(is.na(p_data$relative_value))) { 
      initial_selection_msg <- if (is.null(input$county_sel) || input$county_sel == "" || is.null(input$irrigation_sel) || input$irrigation_sel == "") {
        paste( "<b>Trait comparison plot</b><br><br>",
               "Please make selections for County and Irrigation",
               "in the 'Variety Rx' tab first." )
      } else {
        "<b>No data for plot</b><br><i>Unexpected issue: parallel data is empty or all NA after selections.</i>"
      }
      return(
        plot_ly() %>% 
          layout(
            annotations = list(list(
              text = initial_selection_msg,
              x = 0.5, y = 0.5, xref = "paper", yref = "paper", 
              showarrow = FALSE, font = list(size = 14, color = "#dc3545"),
              bordercolor = "#2C3E50", borderwidth = 1, borderpad = 10,
              bgcolor = "#f0f8ff", opacity = 0.9
            )),
            plot_bgcolor = "#f8f9fa", paper_bgcolor = "#f8f9fa"
          )
      )
    }
    
    subtitle_html <- ""
    if (!is.null(info$specific_missing) && length(info$specific_missing) > 0) {
      missing_lines <- sapply(names(info$specific_missing), function(var_name) {
        paste0("<b>", htmltools::htmlEscape(var_name), ":</b> No data for traits (", paste(sapply(info$specific_missing[[var_name]], htmltools::htmlEscape), collapse = ", "), ")")
      })
      subtitle_html <- paste("<i>Data notes (missing for some traits):</i><br>", paste(missing_lines, collapse = "<br>"))
    }
    
    p <- ggplot(
      p_data, 
      aes(
        x = trait, 
        y = relative_value, 
        group = variety, 
        color = variety,
        text = paste(
          "<b>Variety:</b>", variety,
          "<br><b>Trait:</b>", trait,
          "<br><b>Actual Value:</b>", ifelse(is.na(actual_value), "N/A", round(actual_value, 2)),
          "<br><b>Relative Perf.:</b>", ifelse(is.na(relative_value), "N/A", paste0(round(relative_value, 1), "%"))
        )
      )
    ) +
      geom_line(linewidth = 1.2, na.rm = TRUE) + 
      geom_point(size = 2.5, na.rm = TRUE) +      
      scale_y_continuous(
        limits = c(60, 105), 
        breaks = seq(60, 100, by = 10),
        labels = function(x) paste0(round(x, 0), "%"),
        expand = expansion(mult = c(0.05, 0.05))) + 
      scale_color_manual(values = variety_colors) +
      labs(
        title = NULL, 
        x = "Trait", 
        y = "Relative Performance (%)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.y = element_text(size=11),
        axis.title.x = element_text(size=11, margin = margin(t=10)),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.position = "bottom",                # <--- legend at bottom
        legend.title = element_blank(),
        legend.text = element_text(size=9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    plot_obj <- ggplotly(p, tooltip = "text", height = NULL) %>%
      layout(
        legend = list(
          orientation = "h",
          y = -0.25,              
          x = 0.5,
          xanchor = "center",
          traceorder = "normal"
        ),
        margin = list(b = 90, t = 80, l = 60, r = 30), 
        paper_bgcolor = "rgba(255,255,255,0)", 
        plot_bgcolor = "rgba(255,255,255,0)"
      ) %>%
      style(hoverlabel = list(bgcolor = "white", font = list(size=11, color="#333")))
    
    if (subtitle_html != "") {
      plot_obj <- plot_obj %>% add_annotations(
        text = subtitle_html,
        x = 0.5, y = 1.08, 
        yref = "paper", xref = "paper",
        xanchor = "center", yanchor = "bottom",
        showarrow = FALSE, align = "left", 
        font = list(size = 11, color = "firebrick")
      )
      plot_obj <- plot_obj %>% layout(margin = list(t = 120, b = 90)) 
    }
    
    return(plot_obj)
  })
  
  # Prepare county map data
  prepare_county_map_data <- reactive({
    req(input$variable_sel, input$irrigation_sel, input$variety_sel, input$year_sel)
    if (input$variable_sel == "" || input$irrigation_sel == "" || input$variety_sel == "" || input$year_sel == "") return(NULL)
    
    raw_data <- data %>%
      filter(
        year == input$year_sel,
        resp_var == input$variable_sel,
        irrigation == input$irrigation_sel
      ) %>%
      group_by(county) %>%
      mutate(
        variety_rank = rank(-resp_val, ties.method="min"),
        total_varieties = n_distinct(variety),
        percentile_rank = ( (total_varieties - variety_rank + 1) / total_varieties) * 100,
        performance_class = case_when(
          percentile_rank >= 67 ~ "Top Tier (Top 33%)",    
          percentile_rank >= 34 ~ "Mid Tier (Middle 33%)", 
          percentile_rank > 0   ~ "Bottom Tier (Bottom 33%)", 
          TRUE ~ "Data Unavailable" 
        )
      ) %>%
      ungroup()
    
    map_data_join <- raw_data %>%
      filter(variety == input$variety_sel) %>%
      select(county, resp_val_highlight = resp_val, rank_highlight = variety_rank, 
             total_varieties_highlight = total_varieties, class_highlight = performance_class)
    
    top_in_county <- raw_data %>%
      group_by(county) %>%
      slice_max(order_by = resp_val, n = 1, with_ties = FALSE) %>%
      select(county, top_variety_county = variety, top_value_county = resp_val)
    
    counties %>% 
      left_join(map_data_join, by = c("name" = "county")) %>%
      left_join(top_in_county, by = c("name" = "county")) %>%
      mutate(
        
        class_highlight = factor(
          ifelse(is.na(class_highlight), "No Data for Selected Variety", class_highlight),
          levels = c("Top Tier (Top 33%)", 
                     "Mid Tier (Middle 33%)", 
                     "Bottom Tier (Bottom 33%)", 
                     "No Data for Selected Variety"),
          ordered = TRUE
        ),
        popup_html = pmap_chr(list(name, resp_val_highlight, rank_highlight, total_varieties_highlight, class_highlight, top_variety_county, top_value_county), 
                              function(n, rvh, rh, tvh, ch, tvc, tv_c){
                                highlight_text <- if(!is.na(rvh)) {
                                  sprintf("%s: %.1f (Rank %d of %d, %s)", input$variety_sel, rvh, rh, tvh, as.character(ch))
                                } else {
                                  sprintf("%s: Data not available", input$variety_sel)
                                }
                                top_text <- if(!is.na(tvc)){
                                  sprintf("Top in County: %s (%.1f)", tvc, tv_c)
                                } else {
                                  "Top variety data unavailable"
                                }
                                sprintf("
County: %s
%s
%s
", n, highlight_text, top_text)
                              })
      )
  })
  # Render leaflet map
  output$map_2 <- renderLeaflet({
    map_plot_data <- prepare_county_map_data()
    req(map_plot_data) 
    
    
    performance_levels <- c("Top Tier (Top 33%)", 
                            "Mid Tier (Middle 33%)", 
                            "Bottom Tier (Bottom 33%)", 
                            "No Data for Selected Variety")
    
    
    performance_colors_fn <- colorFactor(
      palette = c("#4CAF50", "#FFC107", "#F44336", "#BDBDBD"), 
      domain = performance_levels,
      ordered = TRUE
    )
    
    leaflet(map_plot_data) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Base Map") %>%
      addPolygons(
        fillColor = ~performance_colors_fn(class_highlight),
        color = "#555555", 
        weight = 1.5,
        opacity = 1,
        fillOpacity = ~ifelse(as.character(class_highlight) == "No Data for Highlighted Variety", 0.4, 0.75),
        popup = ~lapply(popup_html, htmltools::HTML),
        label = ~lapply(popup_html, htmltools::HTML), 
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#2C3E50", 
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        group = "Variety Performance"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = performance_colors_fn,
        values = ~class_highlight,
        title = sprintf(
          "%s Performance<br>
%s, %s, %s",
          htmltools::htmlEscape(input$variety_sel),
          htmltools::htmlEscape(input$variable_sel),
          htmltools::htmlEscape(input$irrigation_sel),
          htmltools::htmlEscape(input$year_sel)
        ),
        opacity = 0.8,
        labFormat = labelFormat(prefix = "")
      ) 
  })
  
  # Render county summary
  output$county_summary <- renderText({
    req(input$county_sel)
    if (input$county_sel == "") return("<p>Please select a county.</p>")
    
    county_data_filtered <- data %>% filter(county == input$county_sel)
    
    irrigated_trials_count <- county_data_filtered %>% 
      filter(irrigation == "Irrigated") %>% 
      distinct(year) %>% nrow()
    
    dryland_trials_count <- county_data_filtered %>% 
      filter(irrigation == "Dryland") %>% 
      distinct(year) %>% nrow()
    
    irrigated_varieties_count <- length(unique(county_data_filtered$variety[county_data_filtered$irrigation == "Irrigated"]))
    dryland_varieties_count <- length(unique(county_data_filtered$variety[county_data_filtered$irrigation == "Dryland"]))
    
    HTML(paste0(
      "<div style='padding: 15px; font-size: 0.95rem; line-height: 1.6;'>",
      "<h5 style='color:#2C3E50;'>Summary for ", htmltools::htmlEscape(input$county_sel), "</h5>",
      "<ul class='list-unstyled'>",
      "<li><strong>Irrigated Trials (Years):</strong> ", irrigated_trials_count, "</li>",
      "<li><strong>Dryland Trials (Years):</strong> ", dryland_trials_count, "</li>",
      "<li><strong>Varieties Tested (Irrigated):</strong> ", irrigated_varieties_count, "</li>",
      "<li><strong>Varieties Tested (Dryland):</strong> ", dryland_varieties_count, "</li>",
      "</ul>",
      "</div>"
    ))
  })
  
  # Plot Counties
  output$plot1_2 <- renderPlotly({
    req(input$county_sel, input$variable_sel)
    if (input$county_sel == "" || input$variable_sel == "") return(NULL)
    
    selected_mean_val <- data %>% 
      filter(county == input$county_sel, resp_var == input$variable_sel) %>% 
      summarise(mean_val = mean(resp_val, na.rm = TRUE)) %>% 
      pull(mean_val)
    
    plot_df_env <- data %>% 
      filter(resp_var == input$variable_sel, !is.na(resp_val)) %>% 
      group_by(trial, county, year) %>% 
      summarise(
        mean_resp_val = mean(resp_val, na.rm = TRUE), 
        n_obs = n(), 
        .groups = "drop"
      ) %>% 
      filter(!is.na(mean_resp_val), n_obs > 0) %>% 
      mutate(
        is_selected_county = county == input$county_sel, 
        alpha_val = if_else(is_selected_county, 1, 0.7), 
        color_val = if_else(is_selected_county, "#FF8C00", "#4682B4") 
      )
    
    if (nrow(plot_df_env) == 0) {
      return(plot_ly() %>% layout(annotations = list(text="No data found for the selected variable and county.", x=0.5,y=0.5, xref="paper",yref="paper", showarrow=FALSE, font=list(size=14)), title = "Environmental Characterization"))
    }
    
    p_env <- ggplot(
      plot_df_env, 
      aes(
        x = fct_reorder(as.factor(trial), mean_resp_val, .desc = TRUE), 
        y = mean_resp_val, 
        text = paste(
          "<b>Trial:</b>", trial, 
          sprintf("<br><b>Mean %s:</b> %.1f", input$variable_sel, mean_resp_val),
          "<br><b>County:</b>", county, 
          "<br><b>Year:</b>", year,
          "<br><b>Observations:</b>", n_obs
        )
      )
    ) +
      geom_col(aes(fill = color_val, alpha = alpha_val), show.legend = FALSE, width=0.8) +
      scale_fill_identity() + 
      scale_alpha_identity() + 
      geom_hline(yintercept = selected_mean_val, color = "firebrick", linetype = "dashed", size = 1) +
      coord_flip() +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(size = 9), 
        axis.text.y = element_text(size = 9), 
        plot.title = element_text(size = 13, hjust = 0.5, face="bold"), 
        plot.margin = margin(15, 15, 15, 15), 
        legend.position = "none",
        panel.grid.major.y = element_blank()
      ) +
      labs(x = "Trial (Site-Year)", y = sprintf("Mean %s", input$variable),
           title = paste("Mean", input$variable, "Across Trials (Selected County Highlighted)"))
    
    ggplotly(p_env, tooltip = "text", height = 1500)
    
  })
  
  # Render supplementary information
  output$table_supp <- DT::renderDT(
    datatable(
      supp_data, 
      options = list(
        pageLength = 50, 
        scrollX = TRUE, 
        responsive = TRUE,
        searchHighlight = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = '_all')) 
      ),
      filter = 'top', 
      class = 'table table-striped table-hover table-sm compact', 
      rownames = FALSE
    )
  )
}

shinyApp(ui = ui, server = server)