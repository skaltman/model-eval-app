# `are` eval results Shiny app

library(shiny)
library(bslib)
library(gt)
library(purrr)

# Source helper functions (plotting and data helpers)
source("R/helpers.R")

# Load Data ------------------------------------------------------------------

# Load pre-processed eval data
app_data <- readr::read_rds("data/app_data.rds")

are_eval_full <- app_data$eval_data
are_costs <- app_data$cost_data
model_info <- app_data$model_info

# Get available models
available_models <- get_available_models(are_eval_full)

# Add provider info and sort by provider, then by release date (most recent first)
available_models_with_provider <- available_models |>
  left_join(
    model_info |> select(model_join, provider, release_date),
    by = "model_join"
  ) |>
  arrange(provider, desc(release_date))

# Models to select at startup
default_selected <- c(
  "opus_4_5",
  "haiku_4_5_thinking",
  "sonnet_4_5_thinking",
  "gemini_3",
  "gpt_5_1",
  "gpt_5"
)

# Build checkbox UI with provider headers
checkbox_ui <- available_models_with_provider |>
  dplyr::group_split(provider) |>
  purrr::map(\(provider_df) {
    tagList(
      h6(
        unique(provider_df$provider),
        style = "margin-top: 10px; margin-bottom: 5px; color: #2c3e50; font-weight: 600;"
      ),
      div(
        style = "margin-top: -10px;",
        purrr::map2(
          provider_df$model_join,
          provider_df$model_display,
          \(join, display) div(
            style = "margin-bottom: -10px;",
            checkboxInput(
              inputId = paste0("model_", join),
              label = display,
              value = join %in% default_selected
            )
          )
        )
      )
    )
  }) |>
  tagList()

# UI -------------------------------------------------------------------------

ui <- page_navbar(
  title = "How well do LLMs generate R code?",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  id = "main_nav",

  nav_panel(
    "Results",
    page_sidebar(
      sidebar = sidebar(
        title = "Select models",
        width = 300,

        checkbox_ui,

        hr(),

        actionButton(
          "select_all",
          "Select All",
          class = "btn-sm btn-outline-primary",
          width = "48%"
        ),
        actionButton(
          "clear_all",
          "Clear All",
          class = "btn-sm btn-outline-secondary",
          width = "48%"
        ),

        hr(),

        p(
          class = "text-muted small",
          "This app displays evaluation results from the vitals package, ",
          "comparing LLM performance on R code generation tasks."
        )
      ),

      navset_card_tab(
        nav_panel(
          "Performance",
          card(
            card_header("Model Performance on R Code Generation"),
            card_body(
              plotOutput("performance_plot", height = "600px")
            )
          )
        ),

        nav_panel(
          "Cost vs Performance",
          card(
            card_header("Model Performance vs. Cost"),
            card_body(
              plotOutput("cost_plot", height = "600px")
            )
          )
        ),

        nav_panel(
          "Pricing Details",
          card(
            card_header("Model Pricing and Token Usage"),
            card_body(
              class = "p-0",
              gt_output("pricing_table")
            )
          )
        )
      )
    )
  ),

  nav_panel(
    "About",
    layout_columns(
      col_widths = c(2, 8, 2),
      NULL,
      div(
        style = "padding-top: 20px;",
        h2("About This Evaluation"),
        includeMarkdown("about.md")
      ),
      NULL
    )
  )
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  # Reactive: Collect selected models from individual checkboxes
  selected_models <- reactive({
    available_models_with_provider$model_join[
      sapply(available_models_with_provider$model_join, function(model) {
        input[[paste0("model_", model)]]
      })
    ]
  })

  # Reactive: Filtered evaluation data
  filtered_eval <- reactive({
    req(length(selected_models()) > 0)

    are_eval_full |>
      filter(model_join %in% selected_models())
  })

  # Reactive: Summary statistics
  eval_summary <- reactive({
    req(length(selected_models()) > 0)

    compute_summary_stats(
      are_eval_full,
      are_costs,
      selected_models()
    )
  })

  # Select/Clear all buttons
  observeEvent(input$select_all, {
    purrr::walk(
      available_models_with_provider$model_join,
      \(model) updateCheckboxInput(session, paste0("model_", model), value = TRUE)
    )
  })

  observeEvent(input$clear_all, {
    purrr::walk(
      available_models_with_provider$model_join,
      \(model) updateCheckboxInput(session, paste0("model_", model), value = FALSE)
    )
  })

  # Outputs ---------------------------------------------------------------------

  # Performance plot (stacked bar chart)
  output$performance_plot <- renderPlot({
    req(nrow(filtered_eval()) > 0)

    plot_performance(filtered_eval())
  })

  # Cost vs Performance scatter plot
  output$cost_plot <- renderPlot({
    req(nrow(eval_summary()) > 0)

    plot_cost_vs_performance(eval_summary())
  })

  # Pricing table
  output$pricing_table <- render_gt({
    req(nrow(eval_summary()) > 0)

    eval_summary() |>
      left_join(model_info, by = "model_join") |>
      arrange(desc(percent_correct)) |>
      select(
        Model = model_display,
        `Input (per 1M tokens)` = Input,
        `Output (per 1M tokens)` = Output,
        `Input Tokens Used` = input,
        `Output Tokens Used` = output,
        `Total Cost` = price,
        `% Correct` = percent_correct
      ) |>
      gt() |>
      fmt_currency(
        columns = c(
          `Input (per 1M tokens)`,
          `Output (per 1M tokens)`,
          `Total Cost`
        ),
        currency = "USD",
        decimals = 2
      ) |>
      fmt_number(
        columns = c(`Input Tokens Used`, `Output Tokens Used`),
        decimals = 0,
        use_seps = TRUE
      ) |>
      fmt_percent(
        columns = `% Correct`,
        decimals = 1
      ) |>
      cols_align(
        align = "left",
        columns = everything()
      ) |>
      tab_header(
        title = "Model Pricing and Performance Details",
        subtitle = "Sorted by percent correct (descending)"
      ) |>
      data_color(
        columns = `% Correct`,
        palette = c("#ef8a62", "#f6e8c3", "#6caea7"),
        domain = NULL
      ) |>
      data_color(
        columns = `Total Cost`,
        palette = c("#e8f4f8", "#a8d5e2", "#6baed6", "#3182bd", "#08519c"),
        domain = NULL
      ) |>
      tab_options(
        table.font.size = px(14),
        heading.title.font.size = px(18),
        heading.subtitle.font.size = px(14),
        column_labels.font.weight = "bold",
        ihtml.use_pagination = FALSE,
        ihtml.use_page_size_select = FALSE,
        table.width = pct(100)
      )
  })
}


shinyApp(ui = ui, server = server)
