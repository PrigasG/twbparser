library(shiny)
library(twbparser)

# Allow large .twbx uploads (Shiny defaults to 5 MB). Override with the
# TWBPARSER_MAX_UPLOAD_MB environment variable when deploying.
.twbp_max_upload_mb <- suppressWarnings(
  as.numeric(Sys.getenv("TWBPARSER_MAX_UPLOAD_MB", "100"))
)
if (is.na(.twbp_max_upload_mb) || .twbp_max_upload_mb <= 0) .twbp_max_upload_mb <- 100
options(shiny.maxRequestSize = .twbp_max_upload_mb * 1024^2)

`%||%` <- function(a, b) if (!is.null(a)) a else b

empty_tbl <- function(...) tibble::tibble(...)

uploaded_workbook_path <- function(upload) {
  ext <- tolower(tools::file_ext(upload$name))
  if (!ext %in% c("twb", "twbx")) {
    stop("Please upload a .twb or .twbx file.", call. = FALSE)
  }

  dest <- tempfile(pattern = "twbparser_upload_", fileext = paste0(".", ext))
  if (!file.copy(upload$datapath, dest, overwrite = TRUE)) {
    stop("Could not prepare uploaded workbook for parsing.", call. = FALSE)
  }
  dest
}

section_table <- function(id) {
  div(class = "table-wrap", tableOutput(id))
}

metric_card <- function(label, value) {
  div(
    class = "metric",
    div(class = "metric-value", value),
    div(class = "metric-label", label)
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("twbparser workbook inspector"),
    tags$style(HTML("
      :root {
        --bg: #f7f8fa;
        --panel: #ffffff;
        --line: #d9dee7;
        --text: #20242c;
        --muted: #667085;
        --accent: #0f766e;
        --accent-soft: #d9f2ee;
        --danger: #b42318;
      }

      html, body {
        background: var(--bg);
        color: var(--text);
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }

      .container-fluid {
        padding: 0;
      }

      .app-shell {
        min-height: 100vh;
        display: grid;
        grid-template-columns: 320px minmax(0, 1fr);
      }

      .sidebar {
        background: #ffffff;
        border-right: 1px solid var(--line);
        padding: 22px;
      }

      .brand {
        margin-bottom: 22px;
      }

      .brand h1 {
        margin: 0;
        font-size: 24px;
        line-height: 1.1;
        font-weight: 700;
      }

      .brand p {
        margin: 8px 0 0;
        color: var(--muted);
        font-size: 13px;
        line-height: 1.45;
      }

      .main {
        min-width: 0;
        padding: 22px 26px 36px;
      }

      .panel {
        background: var(--panel);
        border: 1px solid var(--line);
        border-radius: 8px;
        padding: 16px;
        margin-bottom: 16px;
      }

      .panel-title {
        margin: 0 0 12px;
        font-size: 14px;
        font-weight: 700;
      }

      .status {
        color: var(--muted);
        font-size: 13px;
        line-height: 1.45;
      }

      .status strong {
        color: var(--text);
      }

      .btn-default, .btn-primary {
        border-radius: 6px;
      }

      .btn-primary {
        background: var(--accent);
        border-color: var(--accent);
      }

      .metrics {
        display: grid;
        grid-template-columns: repeat(6, minmax(120px, 1fr));
        gap: 12px;
        margin-bottom: 16px;
      }

      .metric {
        background: var(--panel);
        border: 1px solid var(--line);
        border-radius: 8px;
        padding: 14px;
        min-height: 84px;
      }

      .metric-value {
        font-size: 28px;
        line-height: 1;
        font-weight: 700;
      }

      .metric-label {
        margin-top: 8px;
        color: var(--muted);
        font-size: 12px;
      }

      .tabs-card {
        background: var(--panel);
        border: 1px solid var(--line);
        border-radius: 8px;
        padding: 14px 16px 18px;
      }

      .nav-tabs {
        border-bottom-color: var(--line);
      }

      .nav-tabs > li > a {
        color: var(--muted);
        border-radius: 6px 6px 0 0;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: var(--text);
        font-weight: 600;
      }

      .tab-pane {
        padding-top: 16px;
      }

      .table-wrap {
        overflow: auto;
        border: 1px solid var(--line);
        border-radius: 8px;
      }

      .table {
        margin-bottom: 0;
        font-size: 13px;
        white-space: nowrap;
      }

      .table > thead > tr > th {
        background: #f3f5f8;
        border-bottom: 1px solid var(--line);
      }

      .empty-state {
        padding: 28px;
        color: var(--muted);
        text-align: center;
        border: 1px dashed var(--line);
        border-radius: 8px;
        background: #fbfcfd;
      }

      .error-box {
        padding: 12px;
        color: var(--danger);
        border: 1px solid #f3b7b0;
        background: #fff4f2;
        border-radius: 8px;
        font-size: 13px;
      }

      #twbp-busy {
        position: fixed;
        inset: 0;
        z-index: 9999;
        display: none;
        align-items: center;
        justify-content: center;
        background: rgba(247, 248, 250, 0.78);
        backdrop-filter: blur(3px);
      }

      .busy-card {
        width: min(420px, calc(100vw - 32px));
        background: #ffffff;
        border: 1px solid var(--line);
        border-radius: 8px;
        box-shadow: 0 18px 60px rgba(20, 24, 32, 0.18);
        padding: 22px;
      }

      .busy-title {
        margin: 0;
        font-size: 18px;
        font-weight: 700;
      }

      .busy-message {
        margin: 8px 0 16px;
        color: var(--muted);
        font-size: 13px;
      }

      .busy-bar {
        height: 7px;
        overflow: hidden;
        border-radius: 999px;
        background: var(--accent-soft);
      }

      .busy-bar span {
        display: block;
        width: 42%;
        height: 100%;
        background: var(--accent);
        border-radius: 999px;
        animation: twbp-slide 1.1s ease-in-out infinite;
      }

      @keyframes twbp-slide {
        0% { transform: translateX(-110%); }
        50% { transform: translateX(95%); }
        100% { transform: translateX(250%); }
      }

      @media (max-width: 900px) {
        .app-shell {
          grid-template-columns: 1fr;
        }

        .sidebar {
          border-right: 0;
          border-bottom: 1px solid var(--line);
        }

        .metrics {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }
    ")),
    tags$script(HTML("
      function twbpShowBusy(message) {
        var overlay = document.getElementById('twbp-busy');
        var messageNode = document.getElementById('twbp-busy-message');
        if (messageNode && message) messageNode.textContent = message;
        if (overlay) overlay.style.display = 'flex';
      }

      function twbpHideBusy() {
        var overlay = document.getElementById('twbp-busy');
        if (overlay) overlay.style.display = 'none';
      }

      Shiny.addCustomMessageHandler('twbp-busy', function(payload) {
        if (payload.show) {
          twbpShowBusy(payload.message || 'Working on the workbook...');
        } else {
          twbpHideBusy();
        }
      });

      $(document).on('change', '#workbook', function() {
        twbpShowBusy('Uploading and parsing workbook...');
      });

      $(document).on('click', '#load_demo', function() {
        twbpShowBusy('Loading the bundled demo workbook...');
      });
    "))
  ),
  div(
    id = "twbp-busy",
    div(
      class = "busy-card",
      h2(class = "busy-title", "Working on workbook"),
      div(id = "twbp-busy-message", class = "busy-message", "Parsing workbook and building report tables..."),
      div(class = "busy-bar", span())
    )
  ),
  div(
    class = "app-shell",
    tags$aside(
      class = "sidebar",
      div(
        class = "brand",
        h1("twbparser"),
        p("Inspect Tableau workbook structure, fields, filters, calculations, SQL, and packaged assets.")
      ),
      div(
        class = "panel",
        div(class = "panel-title", "Workbook"),
        fileInput(
          "workbook",
          NULL,
          accept = c(".twb", ".twbx"),
          buttonLabel = "Choose file",
          placeholder = "No workbook selected"
        ),
        actionButton("load_demo", "Load demo workbook", class = "btn-primary"),
        tags$hr(),
        uiOutput("file_status")
      ),
      div(
        class = "panel",
        div(class = "panel-title", "Table Display"),
        numericInput("max_rows", "Rows per section", value = 100, min = 10, max = 1000, step = 10),
        checkboxInput("show_empty", "Show empty sections", value = TRUE)
      ),
      div(
        class = "panel",
        div(class = "panel-title", "Export"),
        downloadButton("download_brief", "Replication brief (.md)", class = "btn-primary"),
        tags$div(style = "height: 10px;"),
        selectInput("export_table", "Table", choices = NULL),
        downloadButton("download_csv", "Download CSV")
      )
    ),
    tags$main(
      class = "main",
      uiOutput("parse_error"),
      uiOutput("metrics"),
      div(
        class = "tabs-card",
        tabsetPanel(
          id = "section",
          tabPanel("Overview", verbatimTextOutput("summary_text")),
          tabPanel("Replication Brief", verbatimTextOutput("brief_text")),
          tabPanel("Pages", section_table("pages_table")),
          tabPanel("Filters", section_table("filters_table")),
          tabPanel("Shelves", section_table("shelves_table")),
          tabPanel("Fields", section_table("fields_table")),
          tabPanel("Datasources", section_table("datasources_table")),
          tabPanel("Calculations", section_table("calcs_table")),
          tabPanel("SQL", section_table("sql_table")),
          tabPanel("TWBX Assets", section_table("twbx_table")),
          tabPanel("Validation", section_table("validation_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  parsed <- reactiveVal(NULL)
  parse_error <- reactiveVal(NULL)

  # Track temp files/dirs created from uploads so they can be cleaned up when
  # the session ends (important for long-running public deployments).
  temp_paths <- character()
  register_temp <- function(p) {
    if (!is.null(p) && length(p) && nzchar(p)) {
      temp_paths <<- unique(c(temp_paths, p))
    }
  }
  session$onSessionEnded(function() {
    for (p in temp_paths) {
      if (dir.exists(p)) {
        unlink(p, recursive = TRUE, force = TRUE)
      } else if (file.exists(p)) {
        unlink(p, force = TRUE)
      }
    }
  })

  show_busy <- function(message) {
    session$sendCustomMessage("twbp-busy", list(show = TRUE, message = message))
  }

  hide_busy <- function() {
    session$sendCustomMessage("twbp-busy", list(show = FALSE))
  }

  parse_workbook <- function(path, display_name) {
    parse_error(NULL)
    show_busy("Parsing workbook and building report tables...")
    on.exit(hide_busy(), add = TRUE)

    tryCatch({
      parser <- TwbParser$new(path)
      register_temp(parser$twbx_dir)
      report <- parser$get_workbook_report()
      parsed(list(parser = parser, report = report, name = display_name, path = path))
    }, error = function(e) {
      parsed(NULL)
      parse_error(conditionMessage(e))
    })
  }

  observeEvent(input$workbook, {
    req(input$workbook)
    tryCatch({
      dest <- uploaded_workbook_path(input$workbook)
      register_temp(dest)
      parse_workbook(dest, input$workbook$name)
    }, error = function(e) {
      parsed(NULL)
      parse_error(conditionMessage(e))
      hide_busy()
    })
  })

  observeEvent(input$load_demo, {
    demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
    parse_workbook(demo, basename(demo))
  })

  data <- reactive(parsed())

  report <- reactive({
    req(data())
    data()$report
  })

  # Named list of every tibble that can be exported as CSV.
  export_tables <- reactive({
    req(data())
    p <- data()$parser
    r <- report()
    tbls <- list(
      overview          = r$overview,
      pages             = r$pages_summary,
      worksheet_filters = r$sheet_filters,
      dashboard_filters = r$dashboard_filters,
      shelves           = r$sheet_shelves,
      sorts             = r$sheet_sorts,
      fields            = p$get_fields(),
      datasources       = r$datasources,
      parameters        = r$parameters,
      calculated_fields = r$calculated_fields,
      calc_complexity   = p$get_calc_complexity(),
      field_usage       = p$get_field_usage(),
      custom_sql        = r$custom_sql,
      initial_sql       = r$initial_sql,
      relationships     = p$get_relationships(),
      joins             = p$get_joins(),
      dashboard_actions = r$dashboard_actions,
      twbx_manifest     = p$get_twbx_manifest()
    )
    tbls[!vapply(tbls, function(x) is.null(x) || !NROW(x), logical(1))]
  })

  observe({
    tbls <- export_tables()
    updateSelectInput(session, "export_table", choices = names(tbls))
  })

  limited <- function(x) {
    if (is.null(x) || !NROW(x)) return(x)
    utils::head(x, input$max_rows %||% 100)
  }

  empty_section <- function(message) {
    div(class = "empty-state", message)
  }

  render_section_table <- function(expr, empty_message) {
    renderTable({
      x <- expr()
      validate(need(input$show_empty || NROW(x), empty_message))
      limited(x)
    }, striped = TRUE, bordered = FALSE, spacing = "s", width = "100%")
  }

  output$file_status <- renderUI({
    if (is.null(data())) {
      div(class = "status", "Upload a `.twb` or `.twbx` file, or load the bundled demo workbook.")
    } else {
      div(
        class = "status",
        tags$strong(data()$name),
        tags$br(),
        "Parsed and ready"
      )
    }
  })

  output$parse_error <- renderUI({
    if (is.null(parse_error())) return(NULL)
    div(class = "error-box", parse_error())
  })

  output$metrics <- renderUI({
    if (is.null(data())) {
      return(div(class = "empty-state", "No workbook loaded yet."))
    }

    ov <- report()$overview
    div(
      class = "metrics",
      metric_card("Datasources", ov$datasources[1]),
      metric_card("Worksheets", sum(report()$pages$page_type == "worksheet", na.rm = TRUE)),
      metric_card("Dashboards", ov$dashboards[1]),
      metric_card("Fields", ov$raw_fields[1]),
      metric_card("Calculations", ov$calculated_fields[1]),
      metric_card("Filters", NROW(report()$sheet_filters) + NROW(report()$dashboard_filters))
    )
  })

  output$summary_text <- renderPrint({
    if (is.null(data())) {
      cat("No workbook loaded.")
      return(invisible(NULL))
    }
    print(report())
  })

  brief_text <- reactive({
    req(data())
    txt <- tryCatch(
      data()$parser$get_replication_brief(format = "text"),
      error = function(e) paste("Could not build replication brief:", conditionMessage(e))
    )
    if (!is.character(txt) || !length(txt) || !nzchar(txt[1])) {
      return("Replication brief is empty for this workbook.")
    }
    paste(txt, collapse = "\n")
  })

  output$brief_text <- renderPrint({
    if (is.null(data())) {
      cat("No workbook loaded.")
      return(invisible(NULL))
    }
    cat(brief_text())
  })

  output$download_brief <- downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(data()$name %||% "workbook")
      paste0(base, "_replication_brief.md")
    },
    content = function(file) {
      txt <- if (is.null(data())) "No workbook loaded." else brief_text()
      writeLines(txt, file, useBytes = TRUE)
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(data()$name %||% "workbook")
      sel <- input$export_table %||% "table"
      paste0(base, "_", sel, ".csv")
    },
    content = function(file) {
      tbls <- if (is.null(data())) list() else export_tables()
      sel <- input$export_table
      df <- if (!is.null(sel) && sel %in% names(tbls)) tbls[[sel]] else data.frame()
      utils::write.csv(df, file, row.names = FALSE, na = "")
    }
  )

  output$pages_table <- render_section_table(
    function() report()$pages_summary,
    "No pages found."
  )

  output$filters_table <- render_section_table(
    function() {
      sheet <- report()$sheet_filters
      dash <- report()$dashboard_filters

      sheet_out <- if (NROW(sheet)) {
        dplyr::mutate(
          sheet,
          scope = "worksheet",
          container = .data$sheet,
          field = .data$field_clean
        ) |>
          dplyr::select(scope, container, field, filter_class, include_mode, members, range_min, range_max)
      } else {
        empty_tbl(scope = character(), container = character(), field = character(),
                  filter_class = character(), include_mode = character(),
                  members = character(), range_min = character(), range_max = character())
      }

      dash_out <- if (NROW(dash)) {
        dplyr::mutate(
          dash,
          scope = "dashboard",
          container = .data$dashboard,
          filter_class = .data$presentation,
          include_mode = NA_character_,
          members = NA_character_,
          range_min = NA_character_,
          range_max = NA_character_
        ) |>
          dplyr::select(scope, container, field, filter_class, include_mode, members, range_min, range_max)
      } else {
        empty_tbl(scope = character(), container = character(), field = character(),
                  filter_class = character(), include_mode = character(),
                  members = character(), range_min = character(), range_max = character())
      }

      dplyr::bind_rows(sheet_out, dash_out)
    },
    "No filters found."
  )

  output$shelves_table <- render_section_table(
    function() report()$sheet_shelves,
    "No shelf assignments found."
  )

  output$fields_table <- render_section_table(
    function() report()$parser_fields %||% data()$parser$get_fields(),
    "No fields found."
  )

  output$datasources_table <- render_section_table(
    function() report()$datasources,
    "No datasource details found."
  )

  output$calcs_table <- render_section_table(
    function() report()$calculated_fields,
    "No calculated fields found."
  )

  output$sql_table <- render_section_table(
    function() {
      dplyr::bind_rows(
        dplyr::mutate(report()$custom_sql, sql_type = "custom", sql = .data$custom_sql) |>
          dplyr::select(sql_type, relation_name, relation_type, sql, is_custom_sql),
        dplyr::mutate(report()$initial_sql, sql_type = "initial", relation_name = .data$connection_id,
                      relation_type = NA_character_, sql = .data$initial_sql,
                      is_custom_sql = NA) |>
          dplyr::select(sql_type, relation_name, relation_type, sql, is_custom_sql)
      )
    },
    "No SQL found."
  )

  output$twbx_table <- render_section_table(
    function() data()$parser$get_twbx_manifest(),
    "This workbook was not loaded from a TWBX package."
  )

  output$validation_table <- render_section_table(
    function() {
      v <- report()$validation
      if (is.list(v) && NROW(v$issues)) return(v$issues)
      empty_tbl(status = if (isTRUE(v$ok)) "OK" else "Not run", detail = "No validation issues found.")
    },
    "No validation issues found."
  )
}

shinyApp(ui, server)
