library(bslib)
ui <- page_sidebar(
  theme = bs_theme(
    bg = "white",
    fg = "steelblue"
  ),
  title = "Asset Relative Value Calculator",
  
  sidebar = sidebar(
    radioButtons(
      "view_mode",
      "View Mode:",
      choices = c(
        "Single Asset" = "single",
        "Dual Asset" = "double",
        "Dual Denominator" = "dual_denom"
      ),
      selected = "single"
    ),
    conditionalPanel(
      condition = "input.view_mode == 'single'",
      selectInput("single_numerator", "Price Asset:", choices = select_choice, selected = "Apple")
    ),
    conditionalPanel(
      condition = "input.view_mode == 'double'",
      selectInput("num1", "First Asset:", choices = select_choice, selected = "Apple"),
      selectInput("num2", "Second Asset:", choices = select_choice, selected = "Microsoft")
    ),
    conditionalPanel(
      condition = "input.view_mode == 'dual_denom'",
      selectInput("dual_numerator", "Price Asset:", choices = select_choice, selected = "Apple"),
      selectInput("denom1", "First Denominator:", choices = select_choice, selected = "USD"),
      selectInput("denom2", "Second Denominator:", choices = select_choice, selected = "Euro")
    ),
    conditionalPanel(
      condition = "input.view_mode == 'single' || input.view_mode == 'double'",
      selectInput("denominator", "Denominated In:", choices = select_choice, selected = "USD")
    ),
    actionButton("flip", "Shuffle", class = "btn-primary"),
    dateRangeInput(
      "date_range",
      "Date Range:",
      start = start_date,
      end = Sys.Date(),
      min = start_date,
      max = Sys.Date()
    )
  ),
  plotlyOutput("price_plot", height = "600px")
)