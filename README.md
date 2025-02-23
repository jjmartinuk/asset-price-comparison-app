# Asset Relative Value Calculator

A Shiny application for visualizing and comparing the relative values of different assets, including stocks, cryptocurrencies, commodities, and economic indicators.

## Features

- Three viewing modes:
  - Single Asset: View one asset priced in terms of another
  - Dual Asset: Compare two assets priced in the same denominator
  - Dual Denominator: View one asset priced in two different denominators
- Interactive date range selection
- Shuffle button to quickly flip between assets
- Support for multiple asset types:
  - Stocks
  - International Indices
  - Cryptocurrencies and Precious Metals
  - Energy and Industrial Commodities
  - Agricultural Commodities
  - Currencies
  - Economic Indices

## Setup

1. Ensure you have R installed
2. Install required packages:
```R
install.packages(c("shiny", "plotly", "tidyverse", "quantmod", "fredr", "bslib"))
```
3. Set your FRED API key in `R/global.R`
4. Run the application:
```R
shiny::runApp()
```

## Project Structure

- `app.R`: Main entry point
- `R/global.R`: Data loading and preprocessing
- `R/ui.R`: User interface definition
- `R/server.R`: Server logic
- `data/`: Directory for future data files