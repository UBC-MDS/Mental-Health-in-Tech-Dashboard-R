library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyverse)
library(readr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

data <- read_csv("data/processed/mental_health_clean.csv")

countries = c("United States of America",
              "United Kingdom",
              "Canada",
              "Germany")

app$layout(dbcContainer(list(
  dccGraph(id = 'formal_discuss_donutplot'),
  dccRadioItems(
    id='formal_discuss_radio',
    options=list(
      list("label" = "Yes", "value" = "Yes"),
      list("label" = "No", "value" = "No"),
      list("label" = "I don't know", "value" = "I don't know")
    ),
    value = "Yes",
    labelStyle = list("display" = "block")
  ),
  dccGraph(id = 'mental_health_benefits_employer_donutplot'),
  dccRadioItems(
    id='mental_health_benefits_employer_radio',
    options=list(
      list("label" = "Yes", "value" = "Yes"),
      list("label" = "No", "value" = "No"),
      list("label" = "I am not sure", "value" = "I am not sure")
    ),
    value = "Yes",
    labelStyle = list("display" = "block")
  ),
  dccGraph(id = 'mental_health_leave_donutplot'),
  dccRadioItems(
    id='mental_health_leave_radio',
    options=list(
      list("label" = "Very easy", "value" = "Very easy"),
      list("label" = "Somewhat easy", "value" = "Somewhat easy"),
      list("label" = "Neither easy nor difficult", "value" = "Neither easy nor difficult"),
      list("label" = "Somewhat difficult", "value" = "Somewhat difficult"),
      list("label" = "Very difficult", "value" = "Very difficult")
    ),
    value = "Very easy",
    labelStyle = list("display" = "block")
  )
)))




app$callback(
  output('formal_discuss_donutplot', 'figure'),
  list(input('formal_discuss_radio', 'value')),
  function(radiovalue) {
    build_graph(formal_discuss, radiovalue)
  }
)


app$callback(
  output('mental_health_benefits_employer_donutplot', 'figure'),
  list(input('mental_health_benefits_employer_radio', 'value')),
  function(radiovalue) {
    build_graph(mental_health_benefits_employer, radiovalue)
  }
)


app$callback(
  output('mental_health_leave_donutplot', 'figure'),
  list(input('mental_health_leave_radio', 'value')),
  function(radiovalue="Very easy") {
    build_graph(mental_health_leave, radiovalue)
  }
)


#' Donut chart using plotly
#'
#' Creates a donut chart gpraph using plotly library 
#'
#' @param column_name quoted column name to be used for the plot
#' @param column_input value of input coming from the callback function
#'
#' @return plotly layout
#'
#' @examples
#' build_graph(formal_discuss, "Yes")
#'
#' @export
build_graph <- function(column_name, column_input) {
  column_name <- enquo(column_name)
  
  subset_data <-
    data %>%
    select(!!column_name, country) %>%
    mutate(clean_country = ifelse(country %in% countries, country, 'Other'))  %>%
    select(-country) %>%
    mutate_all(funs(na_if(., ""))) %>%
    remove_empty("cols") %>%
    drop_na()
  
  normalize_counts <-
    subset_data %>% group_by(clean_country) %>% count(!!column_name) %>%
    transform(n = (n / sum(n) * 100)) %>%
    spread(key = !!column_name, value = n)
  
  labs <- normalize_counts$clean_country
  vals <- normalize_counts[[column_input]]
  
  plot_ly(normalize_counts,
          labels = ~ labs,
          values = ~ vals) %>%
    add_pie(hole = 0.44) %>%
    layout()
  
}

app$run_server(debug = T)