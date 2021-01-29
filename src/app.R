library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(plotly)
library(devtools)
# install_github('facultyai/dash-bootstrap-components@r-release')

#read in data files
data <- read_csv("data/processed/mental_health_clean_reformat.csv")
feature_list = read_csv("data/processed/features_list.csv")

# Change columns into factors
data_cols <- c(1:16)

data[, data_cols] <- lapply(data[, data_cols], factor)

data$num_employees <- factor(data$num_employees, c(
  "6-25", "26-100",
  "100-500", "500-1000"
))
data$mental_health_leave <- factor(
  data$mental_health_leave,
  c(
    "Very easy", "Somewhat easy",
    "Neither easy nor difficult",
    "Somewhat difficult", "Very difficult",
    "I don't know"
  )
)
data$mental_health_benefits_healthcare <- factor(
  data$mental_health_benefits_healthcare,
  c(
    "I know some", "Somewhat easy",
    "Neither easy nor difficult",
    "Somewhat difficult",
    "Very difficult", "I don't know"
  )
)
data$online_resources <- factor(data$online_resources, c(
  "Very easy",
  "Somewhat easy",
  "Neither easy nor difficult",
  "Somewhat difficult",
  "Very difficult",
  "I don't know"
))
ymn_cols <- c(
  "self_employed", "tech_org", "mental_health_resources",
  "mental_disorder_discuss", "health_disorder_discuss",
  "discuss_coworker", "discuss_supervisor",
  "have_mental_helth_disorder", "treatment"
)
data[ymn_cols] <- lapply(data[ymn_cols], factor, levels = c(
  "No",
  "Maybe", "Yes"
))


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)


app$layout(
  dbcContainer(
    list(
    dbcRow(list(
      dbcCol(htmlH1("Mental Health in Tech Dashboard"))
    )),
    htmlHr(),
    dbcRow(list(
      dbcCol(htmlH2("Overview:"))
    )),
    
    dbcRow(
      list(
        dbcCol(
          list(
            dccDropdown(
              id = "q_selection",
              # options = purrr::map(feature_list$variables3, function(col) list(label = col, value = col)
              options = list(
                list(label = "mental_health_leave", value = "mental_health_leave"),
                list(label = "# employees", value = "num_employees")
              ),
              value = "num_employees",
            )
          ), md=3
        ),
        dbcCol(
          list(
            dccGraph(id = "plot-area")
          )
        )
      )
    ),
    dbcRow(list(dbcCol(htmlH2("HR Questions:"))))
  )
))


app$callback(
  output("plot-area", "figure"),
  list(input("q_selection", "value")),
  function(q_selection) {
    gp <- ggplot(data, aes(
      x = ..prop.., group = 1,
      y = !!sym(q_selection)
    )) +
      geom_bar() +
      facet_wrap(~gender) +
      labs(x = "\n\n\nProportion of Responses") +
      scale_x_continuous(labels = function(x) paste0(x * 100, "%")) +
      theme_bw()
    ggplotly(gp)
  }
)
app$run_server(debug = T)
