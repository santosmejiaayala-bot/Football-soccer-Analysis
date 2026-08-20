library(tidyverse)
library(shiny)
library(plotly)
worldcups <- read.csv("WorldCups.csv", stringsAsFactors = FALSE)
matches   <- read.csv("WorldCupMatches.csv", stringsAsFactors = FALSE)
players   <- read.csv("WorldCupPlayers.csv", stringsAsFactors = FALSE)
# Clean World Cup summary data
worldcups$Attendance <- as.numeric(gsub(",", "", worldcups$Attendance))

# Clean Matches data: remove duplicates and NA rows
matches <- matches %>%
  filter(!is.na(Year), !is.na(MatchID)) %>%
  distinct(MatchID, .keep_all = TRUE) %>%
  mutate(
    Year = as.numeric(Year),
    MatchLabel = paste0(Year, " ", Stage, ": ", Home.Team.Name, " (", Home.Team.Goals, ") vs ", Away.Team.Name, " (", Away.Team.Goals, ")")
  )

players$Player.Name[grepl("^M.LLER$", players$Player.Name)] <- "Thomas MULLER"
players$Player.Name[grepl("^PEL", players$Player.Name)]      <- "PELE (Edson Arantes do Nascimento)"

players <- players %>%
  mutate(Goals = replace_na(str_count(Event, "\\bG\\d+"), 0) + replace_na(str_count(Event, "\\bP\\d+"), 0))

match_year <- matches %>% select(MatchID, Year, Stage, Home.Team.Name, Away.Team.Name)
player_goals <- players %>% left_join(match_year, by = "MatchID")

per_tournament <- player_goals %>%
  group_by(Player.Name, Year) %>%
  summarize(Goals = sum(Goals, na.rm = TRUE), .groups = "drop") %>%
  filter(Goals > 0)

multi_tournament_players <- per_tournament %>%
  group_by(Player.Name) %>%
  summarize(Tournaments = n_distinct(Year), CareerGoals = sum(Goals), .groups = "drop") %>%
  filter(Tournaments >= 2) %>%
  arrange(desc(CareerGoals)) %>%
  slice_head(n = 8) %>%
  pull(Player.Name)

career_arc_data <- per_tournament %>%
  filter(Player.Name %in% multi_tournament_players) %>%
  mutate(Year = as.numeric(Year))

team_choices <- sort(unique(c(matches$Home.Team.Name, matches$Away.Team.Name)))
match_choices <- setNames(matches$MatchID, matches$MatchLabel)

home_games <- matches %>%
  transmute(Year, Team = Home.Team.Name, GoalsFor = Home.Team.Goals, GoalsAgainst = Away.Team.Goals)
away_games <- matches %>%
  transmute(Year, Team = Away.Team.Name, GoalsFor = Away.Team.Goals, GoalsAgainst = Home.Team.Goals)

team_games <- bind_rows(home_games, away_games) %>%
  mutate(
    Team = recode(Team, "Germany FR" = "Germany"),
    Result = case_when(
      GoalsFor > GoalsAgainst ~ "Win",
      GoalsFor == GoalsAgainst ~ "Draw",
      TRUE ~ "Loss"
    )
  )

top_teams <- team_games %>%
  group_by(Team) %>%
  summarize(Tournaments = n_distinct(Year), .groups = "drop") %>%
  arrange(desc(Tournaments)) %>%
  slice_head(n = 16) %>%
  pull(Team)

all_years <- sort(unique(team_games$Year))

team_year_summary <- team_games %>%
  filter(Team %in% top_teams) %>%
  group_by(Team, Year) %>%
  summarize(
    GoalsScored = sum(GoalsFor),
    GoalDiff = sum(GoalsFor) - sum(GoalsAgainst),
    Wins = sum(Result == "Win"),
    .groups = "drop"
  ) %>%
  complete(Team = top_teams, Year = all_years) %>% 
  mutate(Team = factor(Team, levels = rev(top_teams)))

metric_labels <- c("GoalsScored" = "Goals Scored", "GoalDiff" = "Goal Differential", "Wins" = "Wins")
# UI (Tabs)
ui <- navbarPage(
  title = "FIFA World Cup Explorer",
  theme = shinythemes::shinytheme("flatly"),
  
  # TAB 1: Tournament Level - Team Performance (Line Chart + Interactive Heatmap)
  tabPanel(
    "1. Team Performance",
    tabsetPanel(
      tabPanel(
        "Historical Heatmap Matrix",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h4("Heatmap Controls"),
            selectInput(
              "metric",
              "Color tiles by metric:",
              choices = c("Goals Scored" = "GoalsScored", "Goal Differential" = "GoalDiff", "Wins" = "Wins"),
              selected = "GoalsScored"
            ),
            p("Hover over tiles to see exact performance statistics or non-qualification status for each tournament year.")
          ),
          mainPanel(
            width = 9,
            plotlyOutput("heatmap_plot", height = "600px")
          )
        )
      ),
      tabPanel(
        "Filtered Line Chart Comparisons",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h4("Filter Team Metrics"),
            selectInput(
              "stat_choice",
              "Select Statistic:",
              choices = c("Goals Scored", "Wins"),
              selected = "Goals Scored"
            ),
            selectizeInput(
              "selected_teams",
              "Select Teams to Compare:",
              choices = team_choices,
              multiple = TRUE,
              selected = c("Brazil", "Germany", "Argentina", "Italy")
            ),
            sliderInput(
              "year_range",
              "World Cup Year Range:",
              min = min(worldcups$Year),
              max = max(worldcups$Year),
              value = c(min(worldcups$Year), max(worldcups$Year)),
              step = 4,
              sep = ""
            ),
            p(em("Note: Blank/missing entries in line charts indicate non-qualification or absence from the tournament."))
          ),
          mainPanel(
            width = 9,
            plotOutput("teamPlot", height = "500px")
          )
        )
      )
    )
  ),
  
  # TAB 2: Player Level - Career Arcs & Event Counts
  tabPanel(
    "2. Player Career Arcs & Events",
    tabsetPanel(
      tabPanel(
        "Career Scoring Arcs",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            checkboxGroupInput(
              "selected_arc_players",
              "Select Top Scorers:",
              choices = multi_tournament_players,
              selected = multi_tournament_players
            ),
            p("Drag a box across points on the plot to view precise tournament details below.")
          ),
          mainPanel(
            width = 9,
            plotOutput("career_plot", brush = "plot_brush", height = "450px"),
            br(),
            h4("Selected Point Summary:"),
            tableOutput("brush_info")
          )
        )
      ),
      tabPanel(
        "Individual Event Breakdown",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            checkboxGroupInput(
              "event_types",
              "Select Event Types:",
              choices = c(
                "Goal" = "G", "Penalty" = "P", "Yellow Card" = "Y", 
                "Red Card" = "R", "Second Yellow" = "SY", "Own Goal" = "OG", 
                "Missed Penalty" = "MP", "Sub In" = "I", "Sub Out" = "O"
              ),
              selected = c("G", "P", "Y", "R")
            ),
            selectizeInput(
              "selected_event_players",
              "Filter Players:",
              choices = NULL,
              multiple = TRUE
            )
          ),
          mainPanel(
            width = 9,
            plotOutput("playerEventPlot", height = "500px")
          )
        )
      )
    )
  ),
  
  # TAB 3: Match Level - Roster & Event Footprint
  tabPanel(
    "3. Match Roster & Event Footprint",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Match Context Selector"),
        selectizeInput(
          "selected_match_id",
          "Search & Select Match (Year Stage: Home vs Away):",
          choices = match_choices,
          selected = 300186501 # Default: 2014 Final
        ),
        checkboxInput("starting_only", "Show Starting Line-Up Only (S)", value = FALSE),
        hr(),
        p(strong("Visual Encoding Guide:")),
        p("• Gold Highlighted Points: Goal Scorers (Labeled with Player Name)"),
        p("• Muted Points: Non-Scorers"),
        p("• Color Scale: Positions / Roles (GK, C, DF, MF, FW, Sub)")
      ),
      mainPanel(
        width = 8,
        plotOutput("roster_plot", height = "650px")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # TAB 1A: Plotly Heatmap
  output$heatmap_plot <- renderPlotly({
    df <- team_year_summary
    df$Value <- df[[input$metric]]
    label <- metric_labels[[input$metric]]
    
    df <- df %>%
      mutate(TooltipText = ifelse(
        is.na(Value),
        paste0(Team, " — ", Year, "<br>Did not qualify / Participate"),
        paste0(Team, " — ", Year, "<br>", label, ": ", Value)
      ))
    
    p <- ggplot(df, aes(x = factor(Year), y = Team, fill = Value, text = TooltipText)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(na.value = "grey90", option = "C") +
      labs(
        title = "Team Performance Across World Cups",
        subtitle = paste("Tiles colored by", label),
        x = "World Cup Year",
        y = "National Team",
        fill = label
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # TAB 1B: Team Performance Line
  team_data <- reactive({
    req(input$selected_teams, input$year_range)
    
    valid_years <- worldcups$Year[worldcups$Year >= input$year_range[1] & worldcups$Year <= input$year_range[2]]
    matches_filtered <- matches %>% filter(Year %in% valid_years)
    
    if (input$stat_choice == "Goals Scored") {
      home <- matches_filtered %>%
        group_by(Year, Team = Home.Team.Name) %>%
        summarize(Value = sum(Home.Team.Goals, na.rm = TRUE), .groups = "drop")
      away <- matches_filtered %>%
        group_by(Year, Team = Away.Team.Name) %>%
        summarize(Value = sum(Away.Team.Goals, na.rm = TRUE), .groups = "drop")
    } else {
      home <- matches_filtered %>%
        mutate(Win = Home.Team.Goals > Away.Team.Goals) %>%
        group_by(Year, Team = Home.Team.Name) %>%
        summarize(Value = sum(Win, na.rm = TRUE), .groups = "drop")
      away <- matches_filtered %>%
        mutate(Win = Away.Team.Goals > Home.Team.Goals) %>%
        group_by(Year, Team = Away.Team.Name) %>%
        summarize(Value = sum(Win, na.rm = TRUE), .groups = "drop")
    }
    
    bind_rows(home, away) %>%
      group_by(Year, Team) %>%
      summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      filter(Team %in% input$selected_teams)
  })
  
  output$teamPlot <- renderPlot({
    df <- team_data()
    req(nrow(df) > 0)
    
    y_title <- if (input$stat_choice == "Goals Scored") "Total Goals Scored" else "Number of Match Wins"
    
    ggplot(df, aes(x = Year, y = Value, color = Team, group = Team)) +
      geom_line(linewidth = 1.2, alpha = 0.85) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), by = 4)) +
      labs(
        title = paste("Sustained Team Performance:", input$stat_choice, "Across World Cups"),
        subtitle = "Comparing selected national teams across historical tournament years",
        x = "World Cup Tournament Year",
        y = y_title,
        color = "National Team"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # TAB 2A: Career Scoring Arcs
  filtered_arc_data <- reactive({
    req(input$selected_arc_players)
    career_arc_data %>% filter(Player.Name %in% input$selected_arc_players)
  })
  
  selected_points <- reactiveVal(NULL)
  
  observeEvent(input$plot_brush, {
    selected_points(brushedPoints(filtered_arc_data(), input$plot_brush, xvar = "Year", yvar = "Goals"))
  })
  
  output$career_plot <- renderPlot({
    df <- filtered_arc_data()
    df$Selected <- "no"
    
    sel <- selected_points()
    if (!is.null(sel) && nrow(sel) > 0) {
      df$Selected[paste(df$Player.Name, df$Year) %in% paste(sel$Player.Name, sel$Year)] <- "yes"
    }
    
    ggplot(df, aes(x = Year, y = Goals, group = Player.Name, color = Player.Name)) +
      geom_line(linewidth = 1.1) +
      geom_point(aes(alpha = Selected, size = Selected)) +
      scale_alpha_manual(values = c("no" = 0.35, "yes" = 1), guide = "none") +
      scale_size_manual(values = c("no" = 3.5, "yes" = 6), guide = "none") +
      scale_x_continuous(breaks = seq(1950, 2014, by = 4)) +
      labs(
        title = "Career Scoring Output Arcs for Top Multi-Tournament Scorers",
        subtitle = "Drag a selection box across points to inspect precise tournament goals",
        x = "World Cup Tournament Year",
        y = "Goals Scored in Tournament",
        color = "Player"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      )
  })
  
  output$brush_info <- renderTable({
    req(selected_points())
    selected_points() %>%
      select(Player = Player.Name, Year, Goals) %>%
      arrange(desc(Goals))
  })
  
  # TAB 2B: Player Event Breakdown Bar Chart
  observe({
    req(input$event_types)
    
    avail_players <- players %>%
      filter(!is.na(Event)) %>%
      filter(stringr::str_detect(Event, paste(input$event_types, collapse = "|"))) %>%
      distinct(Player.Name) %>%
      arrange(Player.Name) %>%
      pull(Player.Name)
    
    current_sel <- intersect(input$selected_event_players, avail_players)
    if (length(current_sel) == 0 && length(avail_players) > 0) {
      current_sel <- head(avail_players, 8)
    }
    
    updateSelectizeInput(
      session,
      "selected_event_players",
      choices = avail_players,
      selected = current_sel
    )
  })
  
  player_event_data <- reactive({
    req(input$event_types, input$selected_event_players)
    
    event_names <- c(
      G = "Goal", OG = "Own Goal", Y = "Yellow Card", R = "Red Card",
      SY = "Second Yellow", P = "Penalty Scored", MP = "Missed Penalty",
      I = "Substitution In", O = "Substitution Out"
    )
    
    bind_rows(lapply(input$event_types, function(ev) {
      players %>%
        filter(Player.Name %in% input$selected_event_players) %>%
        mutate(Count = stringr::str_count(Event, ev)) %>%
        filter(Count > 0) %>%
        group_by(Player.Name) %>%
        summarize(Count = sum(Count), .groups = "drop") %>%
        mutate(EventType = event_names[ev])
    }))
  })
  
  output$playerEventPlot <- renderPlot({
    df <- player_event_data()
    req(nrow(df) > 0)
    
    plot_data <- df %>%
      group_by(Player.Name) %>%
      mutate(Total = sum(Count)) %>%
      ungroup()
    
    ggplot(plot_data, aes(x = reorder(Player.Name, Total), y = Count, fill = EventType)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Selected Player Events Breakdown",
        subtitle = "Aggregated count of individual match events across World Cup appearances",
        x = "Player",
        y = "Number of Occurrences",
        fill = "Event Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      )
  })
  
  # TAB 3: Match Roster & Event Footprint Plot
  filtered_roster_data <- reactive({
    req(input$selected_match_id)
    
    m_id <- as.numeric(input$selected_match_id)
    df <- players %>%
      filter(MatchID == m_id) %>%
      distinct(Player.Name, .keep_all = TRUE)
    
    if (input$starting_only) {
      df <- df %>% filter(Line.up == "S")
    }
    
    df <- df %>%
      mutate(
        GoalCount = replace_na(str_count(Event, "\\bG\\d+"), 0) + replace_na(str_count(Event, "\\bP\\d+"), 0),
        IsGoal = ifelse(GoalCount > 0, "Goal Scorer", "No Goal"),
        
        Role = case_when(
          Position == "C" ~ "Captain",
          Position == "GK" ~ "Goalkeeper",
          Position == "DF" ~ "Defender",
          Position == "MF" ~ "Midfielder",
          Position == "FW" ~ "Forward",
          Line.up == "S" ~ "Starter (Field)",
          TRUE ~ "Substitute"
        )
      )
    
    list(df = df, match_info = matches %>% filter(MatchID == m_id) %>% slice(1))
  })
  
  output$roster_plot <- renderPlot({
    res <- filtered_roster_data()
    df <- res$df
    info <- res$match_info
    
    req(nrow(df) > 0)
    
    match_title <- paste0("Match Roster Footprint: ", info$Home.Team.Name, " vs. ", info$Away.Team.Name)
    match_sub   <- paste0(info$Year, " World Cup — ", info$Stage, " Stage (Score: ", info$Home.Team.Goals, "-", info$Away.Team.Goals, ")")
    
    df_scorers <- df %>% filter(IsGoal == "Goal Scorer")
    
    ggplot(df, aes(x = Team.Initials, y = reorder(Player.Name, Team.Initials))) +
      geom_point(data = filter(df, IsGoal == "No Goal"), aes(color = Role), size = 2.5, alpha = 0.5) +
      geom_point(data = df_scorers, aes(color = Role), size = 6, shape = 21, stroke = 1.5, fill = "#FFD700") +
      geom_text(
        data = df_scorers,
        aes(label = paste0(Player.Name, " (", GoalCount, " G)")),
        hjust = -0.15,
        vjust = 0.4,
        size = 4,
        fontface = "bold",
        color = "#B8860B"
      ) +
      scale_color_brewer(palette = "Dark2") +
      labs(
        title = match_title,
        subtitle = match_sub,
        x = "National Team Initials",
        y = "Player Name",
        color = "Player Role / Position"
      ) +
      theme_bw(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 13, face = "italic"),
        axis.text.y = element_text(size = 9),
        legend.position = "right"
      )
  })
}

shinyApp(ui = ui, server = server)