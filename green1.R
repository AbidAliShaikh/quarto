library(plotly)
library(dplyr)

# Sample Data: Replace this with your actual data
set.seed(123)
universities <- paste("University", 1:8) # Only 8 universities
data <- data.frame(
  University = rep(universities, each = 6),
  Indicator = rep(1:6, times = 8),
  Value = sample(50:500, 48, replace = TRUE)
)

# Select universities to plot (all 8 in this case)
selected_universities <- unique(data$University)

# Generate the plot
fig <- plot_ly(
  data = data,
  x = ~Indicator,
  y = ~Value,
  color = ~University,
  type = 'scatter',
  mode = 'lines+markers',
  hoverinfo = 'text',
  text = ~paste("University:", University, "<br>Indicator:", Indicator, "<br>Value:", Value)
) %>%
  layout(
    title = "University Trends Across Indicators",
    xaxis = list(title = "Indicators"),
    yaxis = list(title = "Values"),
    hovermode = "closest",
    colorway = rainbow(length(selected_universities)) # Assign rainbow colors
  )

fig
