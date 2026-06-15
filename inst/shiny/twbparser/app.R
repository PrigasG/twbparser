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

# Map a single Tableau mark type to a suggested ggplot2 geom.
twbp_mark_to_geom <- function(mark) {
  m <- tolower(trimws(mark %||% ""))
  if (!nzchar(m)) return("geom_blank()")
  switch(
    m,
    bar        = "geom_col()",
    line       = "geom_line()",
    area       = "geom_area()",
    circle     = "geom_point()",
    point      = "geom_point()",
    square     = "geom_tile()",
    shape      = "geom_point(aes(shape = ...))",
    text       = "geom_text(aes(label = ...))",
    gantt      = "geom_segment()",
    polygon    = "geom_polygon()",
    map        = "geom_sf()",
    pie        = "geom_col() + coord_polar(theta = 'y')",
    automatic  = "geom_point()  # mark: automatic",
    multiple   = "# multiple marks: build one layer per pane",
    paste0("# mark: ", m, " (choose a geom)")
  )
}

# Map a comma-separated mark_types string to combined geom suggestions.
twbp_marks_to_geoms <- function(mark_types) {
  if (is.null(mark_types) || is.na(mark_types) || !nzchar(mark_types)) return(NA_character_)
  marks <- trimws(strsplit(mark_types, ",", fixed = TRUE)[[1]])
  marks <- marks[nzchar(marks)]
  if (!length(marks)) return(NA_character_)
  paste(unique(vapply(marks, twbp_mark_to_geom, character(1))), collapse = " + ")
}

# Static colours per dashboard component type (light-themed canvas).
twbp_comp_palette <- function(ct) {
  switch(
    ct %||% "",
    worksheet         = c(bg = "#e0f2fe", bd = "#0284c7", fg = "#075985", icon = "▣"),
    filter            = c(bg = "#fef3c7", bd = "#d97706", fg = "#92400e", icon = "▼"),
    parameter_control = c(bg = "#fae8ff", bd = "#a21caf", fg = "#86198f", icon = "◈"),
    legend            = c(bg = "#dcfce7", bd = "#16a34a", fg = "#166534", icon = "☰"),
    text              = c(bg = "#f1f5f9", bd = "#94a3b8", fg = "#475569", icon = "T"),
    image             = c(bg = "#ecfeff", bd = "#0891b2", fg = "#155e75", icon = "▦"),
    c(bg = "#f8fafc", bd = "#cbd5e1", fg = "#64748b", icon = "□")
  )
}

# Build an HTML wireframe (to scale) for one dashboard's layout tibble.
twbp_layout_html <- function(ld) {
  if (is.null(ld) || !NROW(ld)) {
    return(div(class = "empty-state", "No layout zones found for this dashboard."))
  }

  # Render only leaf zones (drop container zones whose id is a parent).
  parents <- unique(ld$parent_zone_id[!is.na(ld$parent_zone_id)])
  leaves <- ld[!(ld$zone_id %in% parents), , drop = FALSE]
  leaves <- leaves[
    !is.na(leaves$x) & !is.na(leaves$y) & !is.na(leaves$w) & !is.na(leaves$h),
    , drop = FALSE
  ]
  if (!NROW(leaves)) {
    return(div(class = "empty-state",
               "This dashboard's zones do not carry pixel coordinates, so it can't be drawn to scale. See the table below."))
  }

  minx <- min(leaves$x); miny <- min(leaves$y)
  spanw <- max(leaves$x + leaves$w) - minx
  spanh <- max(leaves$y + leaves$h) - miny
  if (!is.finite(spanw) || spanw <= 0 || !is.finite(spanh) || spanh <= 0) {
    return(div(class = "empty-state", "Could not determine a drawable canvas for this dashboard."))
  }

  boxes <- lapply(seq_len(nrow(leaves)), function(i) {
    z <- leaves[i, ]
    pal <- twbp_comp_palette(z$component_type)
    left <- (z$x - minx) / spanw * 100
    top  <- (z$y - miny) / spanh * 100
    wpc  <- z$w / spanw * 100
    hpc  <- z$h / spanh * 100
    label <- if (!is.na(z$target) && nzchar(z$target)) z$target else z$component_type
    dims <- sprintf("%s,%s · %s×%s", z$x, z$y, z$w, z$h)
    float_badge <- if (identical(z$layout_type, "floating")) " · floating" else ""
    div(
      style = sprintf(
        paste0("position:absolute; left:%.3f%%; top:%.3f%%; width:%.3f%%; height:%.3f%%;",
               "background:%s; border:1px solid %s; border-radius:6px; padding:5px 7px;",
               "overflow:hidden; box-sizing:border-box;"),
        left, top, wpc, hpc, pal[["bg"]], pal[["bd"]]
      ),
      div(style = sprintf("font-size:11px; font-weight:600; color:%s; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;", pal[["fg"]]),
          paste0(pal[["icon"]], " ", label)),
      div(style = sprintf("font-size:10px; color:%s; opacity:0.8;", pal[["fg"]]),
          paste0(z$component_type, float_badge)),
      div(style = sprintf("position:absolute; bottom:3px; right:6px; font-size:9px; color:%s; opacity:0.7;", pal[["fg"]]),
          dims)
    )
  })

  div(
    style = sprintf(
      "position:relative; width:100%%; aspect-ratio:%s / %s; max-height:620px; border:1px solid var(--line); border-radius:8px; background:#fff;",
      spanw, spanh
    ),
    boxes
  )
}

# Generate a starter R script from a parsed workbook.
twbp_r_scaffold <- function(parser) {
  L <- character()
  add <- function(...) L[[length(L) + 1L]] <<- paste0(...)
  slugify <- function(x) {
    s <- gsub("[^A-Za-z0-9]+", "_", x %||% "")
    s <- gsub("^_|_$", "", s)
    if (!nzchar(s)) "sheet" else s
  }
  bt <- function(x) if (is.null(x) || !length(x)) NULL else paste0("`", x[1], "`")

  ch <- tryCatch(parser$get_charts(), error = function(e) tibble::tibble())
  sh <- tryCatch(parser$get_sheet_shelves(), error = function(e) tibble::tibble())
  layout <- tryCatch(parser$get_dashboard_layout(), error = function(e) tibble::tibble())
  field_for <- function(sheet, shelf) {
    if (!NROW(sh)) return(NULL)
    v <- sh$field_clean[sh$sheet == sheet & sh$shelf == shelf]
    v <- unique(v[!is.na(v) & nzchar(v)])
    if (length(v)) v else NULL
  }

  add("# -----------------------------------------------------------------")
  add("# R starter scaffold generated by twbparser")
  add("# Source workbook: ", basename(parser$path %||% "workbook"))
  add("# Generated:       ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  add("# Review before use: wire up `data`, adjust field names and aesthetics.")
  add("# -----------------------------------------------------------------")
  add("")
  add("library(ggplot2)")
  add("library(dplyr)")
  add("# library(bslib)   # for the dashboard layout")
  add("# library(shiny)")
  add("")
  add("# Load your data (Tableau extract or SQL export) into `data`.")
  add("# data <- readr::read_csv('your_data.csv')")
  add("")
  add("# ============================ Worksheets ============================")
  if (NROW(ch)) {
    for (i in seq_len(nrow(ch))) {
      ws    <- ch$worksheet[i]
      marks <- ch$mark_types[i]
      cols  <- field_for(ws, "cols")
      rows  <- field_for(ws, "rows")
      color <- field_for(ws, "color")
      size  <- field_for(ws, "size")
      slug  <- slugify(ws)
      add("")
      add("# ---- ", ws, "  (mark: ", marks %||% "NA", ") ----")
      aes_parts <- character()
      if (!is.null(cols))  aes_parts <- c(aes_parts, paste0("x = ", bt(cols)))
      if (!is.null(rows))  aes_parts <- c(aes_parts, paste0("y = ", bt(rows)))
      if (!is.null(color)) aes_parts <- c(aes_parts, paste0("colour = ", bt(color)))
      if (!is.null(size))  aes_parts <- c(aes_parts, paste0("size = ", bt(size)))
      aes_str <- if (length(aes_parts)) paste(aes_parts, collapse = ", ") else "x = ..., y = ..."
      geom <- twbp_marks_to_geoms(marks)
      if (is.null(geom) || is.na(geom)) geom <- "geom_blank()  # no mark type recorded"
      add("p_", slug, " <- ggplot(data, aes(", aes_str, ")) +")
      add("  ", geom, " +")
      add("  labs(title = \"", ws, "\")")
    }
  } else {
    add("# (no worksheets found)")
  }
  add("")
  add("# ============================ Dashboards ============================")
  if (NROW(layout)) {
    for (dn in unique(layout$dashboard)) {
      ld <- layout[layout$dashboard == dn, , drop = FALSE]
      ws_zones <- ld[!is.na(ld$target) & nzchar(ld$target), , drop = FALSE]
      ext_w <- suppressWarnings(max(ld$x + ld$w, na.rm = TRUE))
      ext_h <- suppressWarnings(max(ld$y + ld$h, na.rm = TRUE))
      add("")
      add("# ---- Dashboard: ", dn, " ----")
      if (is.finite(ext_w) && is.finite(ext_h)) {
        add("# Inferred canvas extent (workbook units): ", ext_w, " x ", ext_h)
      }
      if (nrow(ws_zones)) {
        add("# Worksheet zones (target @ x,y wxh):")
        for (j in seq_len(nrow(ws_zones))) {
          add("#   ", ws_zones$target[j], " @ ", ws_zones$x[j], ",", ws_zones$y[j],
              " ", ws_zones$w[j], "x", ws_zones$h[j])
        }
        add("# ui_", slugify(dn), " <- bslib::page_fillable(")
        for (j in seq_len(nrow(ws_zones))) {
          add("#   bslib::card(bslib::card_header('", ws_zones$target[j],
              "'), shiny::plotOutput('", slugify(ws_zones$target[j]), "')),")
        }
        add("# )")
      } else {
        add("# (no worksheet zones found)")
      }
    }
  } else {
    add("# (no dashboards found)")
  }

  paste(unlist(L), collapse = "\n")
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
        tags$div(style = "height: 8px;"),
        downloadButton("download_rscript", "R starter script (.R)"),
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
          tabPanel(
            "Dashboard Layout",
            div(
              style = "display:flex; align-items:flex-end; gap:14px; flex-wrap:wrap; margin-bottom:12px;",
              div(style = "min-width:240px;", selectInput("layout_dashboard", "Dashboard", choices = NULL, width = "100%")),
              uiOutput("layout_meta")
            ),
            uiOutput("layout_legend"),
            uiOutput("layout_canvas"),
            tags$div(style = "height:14px;"),
            div(class = "panel-title", "Objects on this dashboard"),
            section_table("layout_table")
          ),
          tabPanel("Charts", section_table("charts_table")),
          tabPanel("Formatting", section_table("formatting_table")),
          tabPanel("Tooltips", section_table("tooltips_table")),
          tabPanel("Pages", section_table("pages_table")),
          tabPanel("Filters", section_table("filters_table")),
          tabPanel("Shelves", section_table("shelves_table")),
          tabPanel("Fields", section_table("fields_table")),
          tabPanel("Datasources", section_table("datasources_table")),
          tabPanel("Parameters", section_table("parameters_table")),
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
      dashboard_size    = p$get_dashboard_size(),
      formatting        = p$get_formatting(),
      tooltips          = p$get_tooltips(),
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

  # ---- Dashboard Layout ----
  layout_all <- reactive({
    req(data())
    data()$parser$get_dashboard_layout()
  })

  observe({
    la <- layout_all()
    dn <- if (NROW(la)) unique(la$dashboard) else character()
    updateSelectInput(session, "layout_dashboard", choices = dn)
  })

  layout_selected <- reactive({
    la <- layout_all()
    if (!NROW(la)) return(la)
    sel <- input$layout_dashboard
    if (is.null(sel) || !nzchar(sel)) return(la[la$dashboard == la$dashboard[1], , drop = FALSE])
    la[la$dashboard == sel, , drop = FALSE]
  })

  output$layout_meta <- renderUI({
    if (is.null(data())) return(NULL)
    ld <- layout_selected()
    if (!NROW(ld)) return(div(class = "status", "No dashboards in this workbook."))
    ext_w <- suppressWarnings(max(ld$x + ld$w, na.rm = TRUE))
    ext_h <- suppressWarnings(max(ld$y + ld$h, na.rm = TRUE))
    n_ws  <- sum(!is.na(ld$target) & nzchar(ld$target))

    # Prefer the declared <size>; fall back to extent inferred from zones.
    sz <- data()$parser$get_dashboard_size(input$layout_dashboard)
    size_txt <- if (NROW(sz) && !is.na(sz$max_width[1]) && !is.na(sz$max_height[1])) {
      sprintf("%d × %d px (%s)", sz$max_width[1], sz$max_height[1],
              sz$sizing_mode[1] %||% "fixed")
    } else if (NROW(sz) && !is.na(sz$sizing_mode[1])) {
      sprintf("%s sizing · %g × %g (inferred from zones)",
              sz$sizing_mode[1], ext_w, ext_h)
    } else if (is.finite(ext_w) && is.finite(ext_h)) {
      sprintf("%g × %g (workbook units, inferred from zones)", ext_w, ext_h)
    } else {
      "unknown"
    }
    div(
      class = "status",
      tags$strong("Page size: "), size_txt, tags$br(),
      sprintf("%d zones · %d worksheet placements", NROW(ld), n_ws)
    )
  })

  output$layout_legend <- renderUI({
    if (is.null(data()) || !NROW(layout_selected())) return(NULL)
    item <- function(ct, label) {
      pal <- twbp_comp_palette(ct)
      span(
        style = "display:inline-flex; align-items:center; gap:5px; margin-right:14px; font-size:12px; color:var(--muted);",
        span(style = sprintf("width:12px; height:12px; border-radius:3px; background:%s; border:1px solid %s; display:inline-block;",
                             pal[["bg"]], pal[["bd"]])),
        label
      )
    }
    div(
      style = "margin-bottom:10px;",
      item("worksheet", "worksheet"), item("filter", "filter"),
      item("parameter_control", "parameter"), item("legend", "legend"),
      item("text", "text"), item("image", "image")
    )
  })

  output$layout_canvas <- renderUI({
    if (is.null(data())) return(div(class = "empty-state", "No workbook loaded yet."))
    ld <- layout_selected()
    if (!NROW(ld)) return(div(class = "empty-state", "This workbook has no dashboards to draw."))
    twbp_layout_html(ld)
  })

  output$layout_table <- render_section_table(
    function() {
      ld <- layout_selected()
      if (!NROW(ld)) return(ld)
      dplyr::select(ld, zone_id, parent_zone_id, component_type, target, layout_type, x, y, w, h)
    },
    "No layout zones found."
  )

  # ---- Charts + ggplot2 hints ----
  charts_data <- reactive({
    req(data())
    p <- data()$parser
    ch <- p$get_charts()
    if (!NROW(ch)) return(ch)
    sh <- p$get_sheet_shelves()
    pick <- function(sheet_name, shelf_name) {
      if (!NROW(sh)) return(NA_character_)
      v <- sh$field_clean[sh$sheet == sheet_name & sh$shelf == shelf_name]
      v <- unique(v[!is.na(v) & nzchar(v)])
      if (length(v)) paste(v, collapse = ", ") else NA_character_
    }
    ch |>
      dplyr::mutate(
        suggested_ggplot = vapply(.data$mark_types, twbp_marks_to_geoms, character(1)),
        cols  = vapply(.data$worksheet, pick, character(1), shelf_name = "cols"),
        rows  = vapply(.data$worksheet, pick, character(1), shelf_name = "rows"),
        color = vapply(.data$worksheet, pick, character(1), shelf_name = "color"),
        size  = vapply(.data$worksheet, pick, character(1), shelf_name = "size")
      ) |>
      dplyr::select(worksheet, mark_types, suggested_ggplot, cols, rows, color, size)
  })

  output$charts_table <- render_section_table(
    function() charts_data(),
    "No worksheets with mark types found."
  )

  output$formatting_table <- render_section_table(
    function() data()$parser$get_formatting(),
    "No explicit formatting rules found."
  )

  output$tooltips_table <- render_section_table(
    function() {
      tt <- data()$parser$get_tooltips()
      if (NROW(tt)) tt[tt$is_customized, , drop = FALSE] else tt
    },
    "No customized tooltips found."
  )

  output$download_rscript <- downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(data()$name %||% "workbook")
      paste0(base, "_scaffold.R")
    },
    content = function(file) {
      txt <- if (is.null(data())) {
        "# No workbook loaded."
      } else {
        tryCatch(
          twbp_r_scaffold(data()$parser),
          error = function(e) paste("# Could not generate scaffold:", conditionMessage(e))
        )
      }
      writeLines(txt, file, useBytes = TRUE)
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

  output$parameters_table <- render_section_table(
    function() {
      pr <- report()$parameters
      if (!NROW(pr)) return(pr)
      keep <- intersect(
        c("name", "datatype", "role", "parameter_type", "allowable_type", "current_value"),
        names(pr)
      )
      if (length(keep)) dplyr::select(pr, dplyr::all_of(keep)) else pr
    },
    "No parameters found."
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
