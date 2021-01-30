library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(plotly)
library(devtools)
library(janitor)
# install_github('facultyai/dash-bootstrap-components@r-release')

# read in data files
data <- read_csv("data/processed/mental_health_clean_reformat.csv")
feature_list <- read_csv("data/processed/features_list.csv")

countries <- c(
  "United States of America",
  "United Kingdom",
  "Canada",
  "Germany"
)

# Change columns into factors
data_cols <- c(1:16)

# data[, data_cols] <- lapply(data[, data_cols], factor)
data[, data_cols] <- lapply(data[, data_cols], fct_explicit_na, na_level = "No response")

data$num_employees <- ordered(data$num_employees, levels = c(
  "1-5",
  "6-25", "26-100", "100-500", "500-1000", "More than 1000", "No response"
))

data$mental_health_leave <- factor(
  data$mental_health_leave,
  levels =
    c(
      "Very easy", "Somewhat easy",
      "Neither easy nor difficult",
      "Somewhat difficult", "Very difficult",
      "I don't know", "No response"
    )
)

data$online_resources <- factor(data$online_resources, levels = c(
  "Yes, I know several",
  "I know some",
  "No, I don't know any", "No response"
))
ymn_cols <- c(
  "self_employed", "tech_org", "mental_health_resources",
  "mental_disorder_discuss", "health_disorder_discuss",
  "discuss_coworker", "discuss_supervisor",
  "have_mental_helth_disorder", "treatment",
  "mental_health_benefits_healthcare", "productivity"
)
data[ymn_cols] <- lapply(data[ymn_cols], fct_explicit_na, na_level = "No response")
data[ymn_cols] <- lapply(data[ymn_cols], factor, levels = c(
  "Yes", "Maybe", "I don't know",
  "Unsure", "No", "Not eligible for coverage",
  "Not applicable to me", "No response"
))


# App layout

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
                options = map2(feature_list$variables3[data_cols][-c(15)], feature_list$variables[data_cols][-c(15)], function(col, col2) list(label = col, value = col2)),
                value = "num_employees",
              )
            ),
            md = 3
          ),
          dbcCol(
            list(
              dccGraph(id = "plot-area")
            )
          )
        )
      ),
      htmlHr(),
      dbcRow(list(dbcCol(htmlH2("HR Questions:")))),
      dbcRow(
        list(
          dbcCol(
            list(
              dbcCol(htmlH4("Gender:")),
              dccRadioItems(
                id = "gender_selection",
                options = list(
                  list(label = "All", value = "all"),
                  list(label = "Male", value = "Male"),
                  list(label = "Female", value = "Female"),
                  list(label = "Others", value = "Other")
                ),
                value = "all"
              ),
              dbcCol(htmlH4("Age of Respondents:")),
              dccRangeSlider(
                id = "age_slider",
                min = 15,
                max = 65,
                allowCross = FALSE,
                marks = list(
                  "15" = "15",
                  "20" = "20",
                  "25" = "25",
                  "30" = "30",
                  "35" = "35",
                  "40" = "40",
                  "45" = "45",
                  "50" = "50",
                  "55" = "55",
                  "60" = "60",
                  "65" = "65"
                ),
                value = list(15, 65)
              )
            ),
            md = 3
          ),
          dbcCol(
            list(
              dccGraph(id = "work_interfere_bars"),
              dccGraph(id = "remote_barplot")
            )
          )
        )
      ),
      htmlHr(),
      dbcRow(
        list(
          dbcCol(
            list(
              htmlP("Has your employer ever formally discussed mental health (for example, as part of a wellness campaign or other official communication)?"),
              dccRadioItems(
                id = "formal_discuss_radio",
                options = list(
                  list(label = "Yes", value = "Yes"),
                  list(label = "No", value = "No"),
                  list(label = "I don't know", value = "I don't know")
                ),
                value = "Yes",
                labelStyle = list("display" = "block"),
                inputStyle = list("marginLeft" = "20px", "marginRight" = "5px")
              )
            )
          ),
          dbcCol(
            list(
              htmlP("Do you know the options for mental health care available under your employer-provided coverage?"),
              dccRadioItems(
                id = "mental_health_benefits_employer_radio",
                options = list(
                  list(label = "Yes", value = "Yes"),
                  list(label = "No", value = "No"),
                  list(label = "I am not sure", value = "I am not sure")
                ),
                value = "Yes",
                labelStyle = list("display" = "block"),
                inputStyle = list("marginLeft" = "20px", "marginRight" = "5px")
              )
            )
          ),
          dbcCol(
            list(
              htmlP("If a mental health issue prompted you to request a medical leave from work, asking for that leave would be:"),
              dccRadioItems(
                id = "mental_health_leave_radio",
                options = list(
                  list(label = "Very easy", value = "Very easy"),
                  list(label = "Somewhat easy", value = "Somewhat easy"),
                  list(label = "Neither easy nor difficult", value = "Neither easy nor difficult"),
                  list(label = "Somewhat difficult", value = "Somewhat difficult"),
                  list(label = "Very difficult", value = "Very difficult")
                ),
                value = "Very easy",
                labelStyle = list("display" = "block"),
                inputStyle = list("marginLeft" = "20px", "marginRight" = "5px")
              )
            )
          ),
          dbcRow(
            list(
              dbcCol(
                list(
                  dccGraph(id = "formal_discuss_donutplot")
                )
              ),
              dbcCol(
                list(
                  dccGraph(id = "mental_health_benefits_employer_donutplot")
                )
              ),
              dbcCol(
                list(
                  dccGraph(id = "mental_health_leave_donutplot")
                )
              )
            )
          )
        )
      )
    )
  )
)

# Overview plot (by gender):

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
      labs(x = "Proportion of Responses") +
      scale_x_continuous(labels = function(x) paste0(x * 100, "%")) +
      theme(
        strip.text.x = element_text(size = 14),
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 18, hjust = -0.5)
      ) +
      labs(title = "Survey Responses by Gender", x = "")
    ggplotly(gp)
  }
)

# First HR Plot:
app$callback(
  output("work_interfere_bars", "figure"),
  list(input("age_slider", "value"), input("gender_selection", "value")),
  function(age_slider = c(15, 65), gender_select = "all") {
    plot_data <- data %>%
      subset(work_interfere_treated != "Not applicable to me" &
        work_interfere_not_treated != "Not applicable to me" &
        age >= age_slider[1] &
        age <= age_slider[2])

    if (gender_select != "all") {
      plot_data <- plot_data %>% filter(gender == gender_select)
    }

    work_interfere_bars_treated <- ggplot(data = plot_data) +
      geom_bar(aes(
        x = factor(work_interfere_treated, levels = c("Never", "Rarely", "Sometimes", "Often")),
        fill = work_interfere_treated
      ),
      stat = "count"
      ) +
      ylab("Number of Responses") +
      xlab("When Treated") +
      guides(fill = FALSE) +
      theme(
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(
          fill = "white", size = 1.5, linetype = "solid"
        )
      )

    work_interfere_bars_untreated <- ggplot(data = plot_data) +
      geom_bar(aes(
        x = factor(work_interfere_not_treated, levels = c("Never", "Rarely", "Sometimes", "Often")),
        fill = work_interfere_not_treated
      ),
      stat = "count"
      ) +
      ylab("Number of Responses") +
      xlab("When Untreated") +
      guides(fill = FALSE) +
      theme(
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(
          fill = "white", size = 1.5, linetype = "solid"
        )
      )
    subplot(ggplotly(work_interfere_bars_treated), ggplotly(work_interfere_bars_untreated), margin = 0.05, titleY = TRUE, titleX = TRUE) %>%
      layout(title = "Does your mental health issue interfere with your work?", showlegend = FALSE)
  }
)


app$callback(
  output("remote_barplot", "figure"),
  list(input("age_slider", "value"), input("gender_selection", "value")),
  function(age_slider = c(15, 65), gender_select = "all") {
    data3 <- data.frame(data) %>%
      drop_na(gender) %>%
      filter(age >= age_slider[1] & age <= age_slider[2])
    if (gender_select != "all") {
      data3 <- data3 %>% filter(gender == gender_select)
    }

    plot3 <- ggplot(data3) +
      aes(x = is_remote, fill = is_remote) +
      geom_bar(stat = "count", position = "dodge") +
      facet_wrap(~have_mental_helth_disorder, strip.position = "bottom") +
      ggtitle("Do employees that work remotely report fewer mental health issues?") +
      labs(x = "Mental Health Response", y = "Number of responses", fill = "Remote work") +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(
          fill = "white", size = 1.5, linetype = "solid"
        )
      )

    ggplotly(plot3)
  }
)



app$callback(
  output("formal_discuss_donutplot", "figure"),
  list(input("formal_discuss_radio", "value")),
  function(radiovalue) {
    build_graph(formal_discuss, radiovalue)
  }
)


app$callback(
  output("mental_health_benefits_employer_donutplot", "figure"),
  list(input("mental_health_benefits_employer_radio", "value")),
  function(radiovalue) {
    build_graph(mental_health_benefits_employer, radiovalue)
  }
)


app$callback(
  output("mental_health_leave_donutplot", "figure"),
  list(input("mental_health_leave_radio", "value")),
  function(radiovalue = "Very easy") {
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
#' @export
build_graph <- function(column_name, column_input) {
  column_name <- enquo(column_name)

  subset_data <-
    data %>%
    select(!!column_name, country) %>%
    mutate(clean_country = ifelse(country %in% countries, country, "Other")) %>%
    select(-country) %>%
    mutate_all(funs(na_if(., ""))) %>%
    remove_empty("cols") %>%
    drop_na()

  normalize_counts <-
    subset_data %>%
    group_by(clean_country) %>%
    count(!!column_name) %>%
    transform(n = (n / sum(n) * 100)) %>%
    spread(key = !!column_name, value = n)

  labs <- normalize_counts$clean_country
  vals <- normalize_counts[[column_input]]

  plot_ly(normalize_counts,
    labels = ~labs,
    values = ~vals,
    width = 330,
    height = 330
  ) %>%
    add_pie(hole = 0.44) %>%
    layout(
      legend = list(x = 0.01, y = 0.99, yanchor = "bottom", xanchor = "left"),
      margin = list(r = 20, l = 0, b = 0, t = 0),
      autosize = FALSE
    )
}

app$run_server(debug = T)
