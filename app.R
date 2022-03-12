library(dash)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv(here::here('data', 'processed_data.csv'))

fig <- plot_geo(df)
fig <- fig %>% add_markers(x = ~longitude, y = ~latitude, size = ~all, color= ~all, text =~city, hoverinfo = 'text')
fig <- fig %>% colorbar(title = "Total monthly cost ($)")
fig <- fig %>% layout(
  title = "Total cost comparison with cities on World map"
)

app$layout(
  htmlDiv(
    list(
      dccDropdown(
        id='cost-select',
        options = df %>% select(-city, -country, -region, -latitude, -longitude) %>% colnames %>% purrr::map(function(col) list(label = col, value = col)),
        value = 'all'
      ),
      dccGraph(id= 'bar-plot'),
      dccGraph(figure = fig) 
    )
  )
)

app$callback(
  output('bar-plot', 'figure'),
  list(input('cost-select', 'value')),
  function(cost) {
    df_bar <- df %>%
      select(country, !!sym(cost)) %>%
      group_by(country) %>%
      summarise(total = sum(!!sym(cost)))

    p <- ggplot(df_bar) +
      aes(x = country,
          y = total,
          fill = country) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90))+
      labs(title= "Bar plot of countries comparing different expense costs (see dropdown above)",
           y = "Expense cost ($)",
           x = "Countires",
           fill = "Countires")

    ggplotly(p) %>% layout(dragmode = 'select')
  }
)

app$run_server(host = '0.0.0.0')
