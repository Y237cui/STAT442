


# in case no such  packages
req_package = c("shiny", "dplyr", "ggplot2", "hexbin", "tibble", 
                "hoopR", "janitor","rlang", "ggtext", "magick", "leaflet", "htmltools", "reactable", "tidyr")
new_package = setdiff(req_package, rownames(installed.packages()))
if (length(new_package)) install.packages(new_package, repos = "https://cloud.r-project.org")

library(shiny)
library(dplyr)
library(ggplot2)
library(hexbin)
library(tibble)
library(hoopR)
library(janitor)
library(rlang)
library(ggtext)
library(magick)

library(leaflet)
library(htmltools)

library(reactable)


library(tidyr)

# the code below are use to draw the court
# for main code can turn to about line 175

court_col = list(court = 'cornsilk',lines = 'slategray',text = 'slategray', 
                 hex_border_size = 0.3, hex_border_color = "slategray"
                 )

# draw the circle 
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(tibble(x = center[1] + radius * cos(angles), y = center[2] + radius * sin(angles)))
}

hoop_center_y =0.5 # shift of original y
width = 50 
height = 94 / 2

key_height = 19 # height of horizontal line

inner_key_width = 12 # inner vertical line
outer_key_width = 16 # outer

backboard_width = 6
backboard_offset = 4

neck_length = 0.5
hoop_radius = 0.75

hoop_center_y = backboard_offset + neck_length + hoop_radius

three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

#
plot_court = function(court_theme = court_col) {
  # draw perimeter line
  court_points = tibble(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  #for outer  
  court_points = bind_rows(court_points , tibble(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  # backcoard, round  + |_
  court_points = bind_rows(court_points , tibble(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  #
  court_points = bind_rows(court_points , tibble(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  # initial
  downcir = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  
  circle_top = filter(downcir, y > key_height) %>%
    mutate(desc = "circle_top")
  
  circle_bottom = filter(downcir, y < key_height) %>%
    mutate(angle = atan((y - key_height) / x) * 180 / pi,
           angle_group = floor((angle - 5.625) / 11.25),
           desc = paste0("circle_bottom_", angle_group)) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = tibble(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  # point for 
  court_points = bind_rows(court_points,
                           circle_top,
                           circle_bottom,
                           hoop,
                           restricted,
                           three_point_line)
  
  
  # draw
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    # the shift for 0.5 here
    coord_fixed(ylim = c(0, 35), xlim = c(-25, 25.5)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}


plot_trim = plot_court(court_col) + 
  coord_fixed(xlim = c(-25, 25.5), 
              ylim = c(0, 35), expand = FALSE) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

bg_court = ggplotGrob(plot_trim)

# data for the map
locs = tribble(~name,                                        ~lat,          ~lon,           ~popup,
                "Birthplace (Akron)  (30, Dec 1984)",      41.078243, -81.532521,       "Akron, OH",
                "High School (SVSM)  (2000-2003)",        41.088578, -81.522299,       "St Vincent–St Mary HS",
                "Cavs Home (2003–2010, 2014–2018)",       41.496798, -81.688651,       "Rocket Mortgage FieldHouse",
                "Heat Home (2010–2014)",                  25.781362, -80.187934,       "Kaseya Center",
                "Lakers Home (2018–present)",              34.044485, -118.265702,      "Crypto.com Arena"
               )




# year ans season here
years = 2003:(as.numeric(format(Sys.Date(),"%Y"))-1)
seasons = sprintf("%d-%02d", years, (years+1)%%100)

# add icon
champ_seasons = c("2011-12", "2012-13", "2015-16", "2020-21")
mvp_seasons = c("2008-09", "2009-10", "2011-12", "2012-13")

season_label = seasons

season_label[seasons %in% mvp_seasons] = paste0(season_label[seasons %in% mvp_seasons], " 👑MVP ")
season_label[seasons %in% champ_seasons] = paste0(season_label[seasons %in% champ_seasons], "  🏆")


ui = fluidPage(
  titlePanel("LeBron James's Basketball Career"),
  sidebarLayout(
    sidebarPanel(
      # map without the iamge
      conditionalPanel(
        condition = "input.mainTabs != 'Caareer Map'",
        uiOutput("season_image"),
        selectInput("season", "Season", choices =setNames(seasons, season_label), selected = tail(seasons, 1))
      ),

    
      
      # for shot
      conditionalPanel(
        condition = " input.mainTabs == 'Shot Location'",
        radioButtons("game_type", "Game Type",
                     c("All","Regular Season", "Playoffs"), "All"),
        checkboxGroupInput("shot_type","Shot Type", 
                           choices = NULL),
        radioButtons("shot_result","Shot Result",
                     c("All","Made","Missed"), "All"),
        sliderInput("shot_dist","Shot Distance (ft)", 0, 40, value = c(0, 40)),
        selectInput("period","Period", c("All",1,2,3,4,"Overtime"), "All")
      ),
      
      # map here
      conditionalPanel(
        condition = "input.mainTabs == 'Career Map'",
        numericInput("map_lng",  "Longitude (E+/W‑)", -96),
        numericInput("map_lat",  "Latitude (N+/S‑)",  36),
        sliderInput("map_zoom",  "Zoom Level", 1, 19, value = 4, step = 1),
        selectInput("map_tiles", "Choose your tileset:",
                    choices = names(providers),
                    selected = "OpenStreetMap")
      ),
      
      # game log data
      conditionalPanel(
        condition = "input.mainTabs == 'Game Log'",
        dateRangeInput("gamelog_dates", "Date range",
                       start = NA, end = NA,
                       min   = as.Date("2003-10-01"),
                       max   = Sys.Date())
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "mainTabs",
                  
                  tabPanel("Career Map",
                           leafletOutput("career_map", height = 550),
                           htmlOutput("map_desc")),

                  tabPanel("Shot Location",
                           tagList(
                             plotOutput("chart", height = 500, width = "100%"),
                             htmlOutput("shot_desc", inline = FALSE, 
                                        container = tags$p,
                                        style = "text-align:center; margin-top:200px;")
                           )),
                  
                  tabPanel("NBA Game Log",
                           reactableOutput("game_log"),
                           htmlOutput("game_log_caption")),
                  tabPanel("Text Art",
                           htmlOutput("text_desc"),
                           plotOutput("text_portrait", width="auto", height="auto"),
                           htmlOutput("text_desc2"),
                  )
      ))
  )
)


server <- function(input, output, session) {
  addResourcePath("img", "www")
  
  # choose data here
  shots_all = reactive({
    req(input$season,input$game_type)
    
    # option for game kind
    # if "ALl" bind all
    if (input$game_type == "All") {
      df1 = hoopR::nba_shotchartdetail(league_id = "00",
                                       player_id= "2544",
                                       season = input$season,
                                       
                                       season_type = "Regular Season",
                                       context_measure = "FGA")$Shot_Chart_Detail
      
      df2 <- hoopR::nba_shotchartdetail(league_id = "00",
                                        player_id = "2544",
                                        season = input$season,
                                        season_type = "Playoffs",
                                        context_measure = "FGA")$Shot_Chart_Detail
      
      raw = bind_rows(df1, df2)
    } else {
      raw = hoopR::nba_shotchartdetail(league_id = "00",
                                       player_id = "2544",
                                       season = input$season,
                                       season_type = input$game_type,
                                       context_measure = "FGA")$Shot_Chart_Detail
    }
    
    
    # case avoid no data pull
    # i add this because in coding 
    # this helps detect error  
    if (is.null(raw) || nrow(raw) == 0) {
      return(tibble(period = character(),
                    x = numeric(),
                    y = numeric(),
                    shot_distance = numeric(),
                    shot_type = character(),
                    made= numeric()
      ))
    }
    #
    
    # data clean, for the coord, period, make ,dist
    raw %>%
      janitor::clean_names() %>%
      mutate(period = if_else(as.numeric(period) >= 5, "Overtime", as.character(period)), # over time tie together
             x = -as.numeric(loc_x) / 10,
             y = as.numeric(loc_y) / 10 + hoop_center_y,      # note here add the shift and in my prev code, x / 10 to map to the bg_court 
             shot_distance = as.numeric(shot_distance),
             made = as.numeric(shot_made_flag)) %>%
      # what i need
      select(period, x, y, shot_distance, shot_type, made)
  })
  
  
  
  # for the picture to text one
  text_art = reactive({
    year_suf = substr(input$season, 3, 4) 
    img  = image_read(file.path("www", paste0("lebron_", year_suf, ".png"))) %>%
      image_resize("800") %>%
      image_convert(colorspace = "gray")
    
    
    info = image_info(img)
    w = info$width; h = info$height
    # how gray they are
    hex_mat = image_data(img, channels = "gray")[1,,]
    num_mat = matrix(strtoi(as.vector(hex_mat), 16L),
                     nrow = h, ncol = w, byrow = TRUE)
    
    chars  = unlist(strsplit("LEBRON JAMES", split = ""))
    
    n_char = length(chars)
    
    step = max(1, floor(w / 200))
    x = seq(1, w, by = step)
    y = seq(1, h, by = step)
    
    # data frame
    expand_grid(x = x, y = y) %>%
      mutate(gray = num_mat[cbind(y, x)] / 255,
             idx = floor(gray * (n_char - 1)) + 1,
             char = chars[pmin(pmax(idx, 1), n_char)],
             y = h - y
      )
  })
  
  
  output$text_portrait <- renderPlot({

    ggplot(text_art(), aes(x, y, label = char, colour = gray)) +
      geom_text(family = "mono", size = 4, lineheight = 0.8) +
      coord_equal(expand = FALSE) +
      scale_colour_gradient(low = "black", high = "#eeeeee", guide = "none") +
      theme_void() +
      theme(plot.background = element_rect(fill = "#eeeeee", color = NA))},
    width  = 800,
    height = function(){
      info = image_info(image_read(file.path("www", paste0("lebron_", substr(input$season,3,4), ".png"))))
      round(info$height / info$width * 800)},
    res = 220
  )
  
  
  
  # for image in bar
  output$season_image <- renderUI({
    season_selected = input$season
    # year_prefix = substr(season_selected, 1, 4)   # 2003
    year_suffix = substr(season_selected, 3, 4)       # 03
    img_file = paste0("lebron_", year_suffix, ".png")
    
    web_path = file.path("img", img_file)
    
    tags$img(src = web_path,
             width = "100%",
             style = "margin-bottom: 10px; border-radius: 10px;"
    )
  })
  
  # update shot type
  observe({
    types = unique(shots_all()$shot_type)
    updateCheckboxGroupInput(session,"shot_type",choices=types,selected=types)
  })
  
  
  filtered = reactive({
    shots_all() %>% filter(shot_distance >= input$shot_dist[1],
                           shot_distance <= input$shot_dist[2],
                           shot_type %in% input$shot_type,
                           (input$shot_result=="All") | (input$shot_result=="Made"&made==1) | (input$shot_result=="Missed"&made==0),
                           (input$period=="All"|period==input$period)
    )
  })
  
  
  output$chart <- renderPlot({
    
    df = filtered()
    
    # for case there is no selected case
    if (nrow(df) == 0) {
      grid::grid.newpage()
      grid::grid.text(
        "There is No Such Data for the Selected Filter",
        x = 0.5,
        y = 0.5,
        gp   = grid::gpar(fontsize = 18, fontface = "bold", color = "royalblue4")
      )
      return()
    }
    
    req(nrow(df) > 0)
    
    
    
    df_shot = df %>%
      # we combine 
      mutate(x = round(x, 1),
             y = round(y, 1), #   as x = loc_x / 10, put all 1 inch together,
             made = factor(made)    #   i compare this with 0.1 inch, this is preferred                                 
      ) %>%                     #  other wise some data under the "basket" not clear
      group_by(x, y, made) %>%
      summarise(freq = n(), .groups = "drop")%>%
      arrange(freq)
    
    max_freq = max(df_shot$freq)
    
    # for dot size
    size_max = if (max_freq <= 3) {
      4
    } else if (max_freq <= 20) {
      4 + sqrt(max_freq) + 5
    } else {
      15
    }
    
    # draw dots here
    p = ggplot() +
      annotation_custom(bg_court, xmin = -25, xmax = 25.5, ymin = 0, ymax = 35)
    
    # All case
    if (input$shot_result == "All") {
      p = p +
        # aes x,y to dots
        geom_point(data = filter(df_shot, made == 1),
                   aes(x = x, y = y, size = freq , fill = freq),
                   shape = 21, alpha = 0.75, stroke = 0.3) +
        geom_point(data = filter(df_shot, made == 0),
                   aes(x = x, y = y, size = freq + 1, fill = freq),
                   shape = 21, alpha = 0.75, stroke = 0.3) +
        scale_fill_viridis_c( 
          option = "H", direction = 1, trans = "sqrt", name = "Frequency",
          alpha =0.8,
          guide = guide_colorbar(barheight = unit(0.4, "cm"),
                                 barwidth = unit(7, "cm"),
                                 title.position = "left",
                                 label.position = "bottom",
                                 title.hjust = 0.5)) +
        scale_size_continuous(range = c(3, size_max),
                              trans = "sqrt",
                              name = "Frequency") +
        guides(
          size = guide_legend(order = 2, override.aes = list(fill = "grey", shape = 21)))
    }
    
    
    if (input$shot_result == "Made") {
      p = p +
        geom_point(data = filter(df_shot, made == 1),
                   aes(x = x, y = y, size = freq+1),
                   shape = 21, fill = "seagreen", color = "seagreen", alpha = 0.75, stroke = 0.3) +
        scale_size_continuous(range = c(3, size_max),
                              trans = "sqrt",
                              name = "Frequency"
        ) +
        guides(size = guide_legend(override.aes = list(fill = "seagreen")))
    }
    
    
    if (input$shot_result == "Missed") {
      p = p +
        geom_point(data = filter(df_shot, made == 0),
                   aes(x = x, y = y, size = freq + 1),
                   shape = 21, fill = "red", color = "#8b0000", alpha = 0.75, stroke = 0.3) +
        scale_size_continuous(range = c(3, size_max),
                              trans = "sqrt",
                              name = "Frequency") +
        guides(size = guide_legend(override.aes = list(fill = "red")))
    }
    
    
    p +
      coord_fixed(xlim = c(-25, 25.5), ylim = c(0, 35), expand = FALSE) +
      labs(
        title = paste(input$season,  input$game_type, "LeBron James's Shot Location: Made & Missed")) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.box = "vertical",    
      )
  },
  width = 1000, height = 700
  )
  
  # MAP
  output$career_map <- renderLeaflet({
    leaflet(locs) %>%
      addProviderTiles(input$map_tiles) %>%
      setView(lng = input$map_lng, lat = input$map_lat, zoom = input$map_zoom) %>%
      # if near combine
      addMarkers(
        lng = ~lon, lat = ~lat, label = ~lapply(name, htmltools::HTML),  
        clusterOptions = markerClusterOptions() 
      )
  })
  
  # observe
  observeEvent({ input$map_tiles ; input$map_lng ; input$map_lat ; input$map_zoom }, {
    leafletProxy("career_map") %>%
      clearTiles() %>%
      addProviderTiles(input$map_tiles) %>%
      setView(lng = input$map_lng, lat = input$map_lat, zoom = input$map_zoom) |>
      clearMarkers() %>%
      addMarkers(
        data = locs,
        lng = ~lon, lat = ~lat, label = ~htmlEscape(name),
        clusterOptions = markerClusterOptions()
      )
  })
  
  
  
  
  # game log
  output$game_log <- renderReactable({
    req(input$season, input$game_type)
    
    # type season 
    if (input$game_type == "All") {
      df_1 <- nba_playergamelog(player_id= "2544",
                                season = input$season,
                                season_type_all_star = "Regular Season",
                                per_mode_simple = "Totals"
      )$PlayerGameLog %>%
        mutate(game_type = "Regular Season")
      
      df_2 <- nba_playergamelog(player_id = "2544",
                                season = input$season,
                                season_type_all_star = "Playoffs",
                                per_mode_simpl = "Totals")$PlayerGameLog %>%
        mutate(game_type = "Playoffs")
      
      raw <- bind_rows(df_1, df_2)
    } else {
      raw = nba_playergamelog(player_id = "2544", 
                              season = input$season,
                              season_type_all_star = input$game_type,
                              per_mode_simple      = "Totals"
      )$PlayerGameLog %>%
        mutate(game_type = input$game_type)
    }
    
    
    df = raw %>%
      janitor::clean_names() %>%
      mutate(date = lubridate::mdy(game_date),
             fgm = as.numeric(fgm),
             fga = as.numeric(fga),
             reb = as.numeric(reb),
             ast = as.numeric(ast),
             plus_minus = as.numeric(plus_minus),
             fg_pct = fgm / fga,
             win_flag = if_else(toupper(wl) == "W", "✅", "❌")
      ) %>%
      filter(
        is.na(input$gamelog_dates[1]) | date >= input$gamelog_dates[1],
        is.na(input$gamelog_dates[2]) | date <= input$gamelog_dates[2]
      ) %>%
      select(
        date, 
        opponent   = matchup, 
        `Game Type` = game_type,
        pts, reb, ast, 
        fg_pct, 
        plus_minus,
        win_flag
      )
    
    
    if (nrow(df) == 0) {
      return(reactable(data.frame(Message = "No games for chosen filters"),
                       pagination = FALSE))
    }
    
    # for col for +/-
    max_plus = max(abs(df$plus_minus), 1)
    
    # tow cal for bar , 2 condition here
    bar_style = function(val, maxv, pos, neg = pos) {
      width = paste0(abs(val) / maxv * 100, "%")
      color = ifelse(val >= 0, pos, neg)
      list(background = color, width = width, height = "100%")
    }
    
    bar_style2 = function(val, maxv, pos = "springgreen", neg = "#e31a1c") {
      color = if (val >= 0) pos else neg
      width = paste0(abs(val) / maxv * 100, "%") 
      list(background = color, width = width, height = "100%")
    }
    
    reactable(
      df,
      searchable = TRUE, pagination = TRUE, striped = TRUE,
      defaultPageSize = 15, highlight = TRUE,
      columns = list(
        date  = colDef("Date", width = 120),
        
        opponent = colDef("VS",align ="center", width = 140),
        
        `Game Type` = colDef(width=120),
        
        pts  = colDef("PTS",  align = "right",width = 70),
        
        reb  = colDef("REB",  align = "right", width = 70),
        
        ast  = colDef("AST",  align = "right",width = 70),
        
        fg_pct = colDef("FG%",
                        width = 180,
                        cell = function(v) {
                          percent = sprintf("%.1f%%", v * 100)
                          bar_color = if (v < 0.4) "#de2d26" else "#2ca25f"
                          
                          # the background of the 2 col
                          div(style = list(position = "relative",
                                           height = "18px",
                                           background = "#f0f0f0",
                                           borderRadius = "4px",
                                           overflow = "hidden"
                          ),
                          div(style = list(position = "absolute",
                                           top = 0,
                                           bottom = 0,
                                           left = 0,
                                           background = bar_color,
                                           width = paste0(v * 100, "%")
                          )
                          ),
                          div(style = list(position = "absolute", 
                                           top = "50%",
                                           left = "50%",
                                           transform = "translate(-50%, -50%)",fontWeight = "bold"
                          ),
                          percent
                          )
                          )
                        }
        ),
        plus_minus = colDef("+/-", align = "center",
                            width = 180,
                            cell = function(val) {
                              value_str = if (val >= 0) paste0("+", val) else as.character(val)
                              
                              div(style = list(position = "relative",
                                               height = "18px",
                                               background = "#f0f0f0",
                                               borderRadius = "4px",
                                               overflow = "hidden"
                              ),
                              # col of the +/-
                              if (val != 0) div(style = list(position = "absolute",
                                                             top = 0,
                                                             bottom = 0,
                                                             background = if (val >= 0) "#2ca25f" else "#de2d26",
                                                             width = paste0(abs(val) / max_plus * 50, "%"),
                                                             left = if (val >= 0) "50%" else NULL,
                                                             right = if (val < 0) "50%" else NULL
                              )
                              ),
                              # label
                              div(
                                style = list(position = "absolute",
                                             top = "50%",
                                             left = "50%",
                                             transform = "translate(-50%, -50%)",
                                             fontWeight = "bold"
                                ),
                                value_str
                              )
                              )
                            }
        ),
        win_flag = colDef("Win?", align = "center", width = 60)
      )
    )
  })
  
  
  # observe season
  observeEvent(input$season, {
    
    # set year and mutate date also
    start_year = as.integer(substr(input$season, 1, 4))
    end_year   = start_year + 1
    
    season_start = as.Date(sprintf("%d-10-01", start_year))  
    season_end   = as.Date(sprintf("%d-06-30", end_year)) 
    
    
    updateDateRangeInput(
      session, "gamelog_dates",
      start = season_start,
      end   = season_end,
      min   = season_start,
      max   = season_end
    )
  })
  
  
  
  
  
  
  
  output$game_log_caption <- renderUI({
    HTML(
    '<p style="text-align:left; font-size:14px; margin-top:10px;">
    <strong>Figure 3 – LeBron James’s NBA Game .</strong>  
    <br/>  
    The table shows LeBron’s NBA-game stats including points, assists, rebounds, percentage (FG%), and plus/minus value.
    In "VS", vs. means Home Game, @ means Away Game 
    
    <p style="font-size:18px;text-align:justify;margin-top:6px;">
    
    <br/>
   Over two decades in the NBA, LeBron James has redefined greatness — not just with his scoring, but through his vision, leadership, and impact on both ends of the court.
  <br/> 
  This table captures every heartbeat of LeBron James on the hardwood: his scoring speaks of relentless will, rebounds and assists showcase his all‑around dominance, FG% and plus‑minus reflect his command of the game’s flow, and the win/loss icons embody his unwavering competitive spirit.
    
    
  </p>')
  })
  
  
  output$shot_desc <- renderUI({
    HTML(
    '<p style="text-align:left;font-size:14px;">
    <strong>Figure 2 – Shot Location Chart.</strong>
    <br/>
    Dot size and colour encode the volume of LeBron’s field‑goal attempts
    in the chosen season with some filters.
    <br/>
    
    <p style="font-size:18px;text-align:justify;margin-top:6px;">
    <br/>
     As the NBA’s all‑time leading scorer, LeBron James has turned every inch of the hardwood into his personal canvas.
    <br/>Each dot on this chart captures the precision and power of a man who defies gravity on a nightly basis—whether splashing threes from the wing or bulldozing to the rim.
    <br/>Here, you witness not just statistics but the heartbeat of a champion: relentless ambition, unwavering focus, and the artistry of a player who writes history one shot at a time.
    
    </p>'
    )
  })
  
  output$map_desc <- renderUI({
    HTML(
    '<p style="text-align:left;font-size:14px;">
    <strong>Figure 1 - Career Map.</strong>
    <br/>Markers highlight LeBron James’s birthplace, high‑school gym and NBA
    home arenas. 
    <p style="font-size:18px;text-align:justify;margin-top:6px;">
    <br/>
    
    LeBron James was born on 30 December 1984 in <strong>Akron,
    Ohio</strong>. 
    <br/>A generational talent from the moment he stepped onto the court at<strong> St. Vincent–St. Mary High School</strong>, 
    LeBron skipped college and entered the 2003 NBA Draft and chosen first overall by his hometown <strong>Cleveland Cavaliers</strong>.
    <br/>Over two decades he has starred for <strong>Cleveland</strong>, <strong>Miami Heat</strong> and
    <strong>Los Angeles Lakers</strong>, winning four NBA championships, four MVP titles, and etching his name in history as the league’s all-time leading scorer.
    </p>'
    )
  })
  
  output$text_desc = renderUI({
    HTML(
      '<p style="text-align:left; font-size:18px; margin-bottom:10px;">

       <strong>My hought.</strong><br/>
       <br/>
      When I first watched NBA around 2014, at just eleven years old,
      I was completely spellbound by LeBron James.
      <br/>Despite the doubters, in my eyes, he shouldered all of Cleveland’s hopes and shattered a 50‑year title drought, bringing the city its first championship ever. 
      <br/>His story of rising from a child in a tough neighborhood to one of the game’s immortals has always been encouraging me to trust myself and aspire for great goals. 
      <br/>Beyond his charms in basketball, his contribution out off the court—building the “I PROMISE” School, lifting up his community, and giving hope to so many. 
      <br/>LeBron’s resilience,  generosity, unbreakable spirit—it all reminds me that greatness comes from lifting others as you climb.
      <br/>
      <br/>
     </p>'
    )
  })
  output$text_desc2 = renderUI({
    HTML(
      '<p style="text-align:left; font-size:14px; margin-bottom:5px;">
      
       <strong>Figure 4 – Text Portrait.</strong>
       <br/>
       This typographic portrait is built solely from the characters “LEBRON JAMES”,<br/>
       mapping pixel brightness to character choice to recreate his iconic silhouette.
     </p>'
    )
  })

  
  
}
shinyApp(ui,server)




