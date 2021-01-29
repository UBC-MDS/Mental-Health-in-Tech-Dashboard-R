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
                min=15,
                max=65,
                allowCross=FALSE,
                marks=list(
                  "15"= "15",
                  "20"= "20",
                  "25"= "25",
                  "30"= "30",
                  "35"= "35",
                  "40"= "40",
                  "45"= "45",
                  "50"= "50",
                  "55"= "55",
                  "60"= "60",
                  "65"= "65"),
                value=list(15, 65)
              )
            ), md=3
          ),
          dbcCol(
            list(
              dccGraph(id = "work_interfere_bars"),
              dccGraph(id = "remote_barplot")
            )
          )
        ))
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

# First HR Plot:
app$callback(
  output("work_interfere_bars", "figure"),
  list(input("age_slider", "value"), input("gender_selection", "value")),
  function(age_slider=c(15, 65), gender_select="all") {
    plot_data <- data %>% 
      subset(work_interfere_treated != "Not applicable to me" & 
               work_interfere_not_treated != "Not applicable to me" &
               age >= age_slider[1] &
               age <= age_slider[2])
    
    if (gender_select!="all"){
      plot_data <- plot_data %>% filter(gender==gender_select)
    }

    work_interfere_bars_treated <- ggplot(data = plot_data) +
      geom_bar(aes(x = factor(work_interfere_treated, levels = c('Never', 'Rarely', 'Sometimes', 'Often')), 
                   fill = work_interfere_treated), 
               stat = 'count') +
      ylab("Number of Responses") +
      xlab("When Treated")+
      guides(fill=FALSE) +
      theme(panel.grid.major.x = element_blank(),
            strip.background = element_rect(
              fill="white", size=1.5, linetype="solid"
            ))
    
    work_interfere_bars_untreated <- ggplot(data = plot_data) +
      geom_bar(aes(x = factor(work_interfere_not_treated, levels = c('Never', 'Rarely', 'Sometimes', 'Often')), 
                   fill = work_interfere_not_treated), 
               stat = 'count') +
      ylab("Number of Responses") +
      xlab("When Untreated")+
      guides(fill=FALSE) +
      theme(panel.grid.major.x = element_blank(),
            strip.background = element_rect(
              fill="white", size=1.5, linetype="solid"
            ))
    subplot(ggplotly(work_interfere_bars_treated), ggplotly(work_interfere_bars_untreated), margin = 0.05, titleY=TRUE, titleX=TRUE )%>%
      layout(title="Does your mental health issue interfere with your work?", showlegend=FALSE)
  }
)


app$callback(
  output("remote_barplot", "figure"),
  list(input("age_slider", "value"), input("gender_selection", "value")),
  function(age_slider=c(15, 65), gender_select="all") {
    data3 <- data.frame(data) %>%  drop_na(gender) %>% 
      filter(age>=age_slider[1] & age<=age_slider[2])
    if (gender_select!="all"){
      data3 <- data3 %>% filter(gender==gender_select)
    }
    
    plot3 <- ggplot(data3)+
      aes(x=is_remote, fill=is_remote) +
      geom_bar(stat = 'count', position = "dodge")+
      facet_wrap(~have_mental_helth_disorder, strip.position = "bottom")+
      ggtitle("Do employees that work remotely report fewer mental health issues?")+
      labs(x = 'Mental Health Response', y = 'Number of responses', fill="Remote work")+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_rect(
              fill="white", size=1.5, linetype="solid"
            ))
    
    ggplotly(plot3)
  })

app$run_server(debug = T)
