# =====================================================================
#  Skewness & Kurtosis Dashboard for UG students  (R Shiny)
#  install.packages(c("shiny", "shinydashboard", "plotly"))
#  Run: shiny::runApp("app.R")
# =====================================================================

library(shiny)
library(shinydashboard)
library(plotly)

# ---------------------------------------------------------------------
# Helper functions (curve shapes)
# ---------------------------------------------------------------------

# Skewed curve in standard units (mean 0, SD 1) using a shifted Gamma shape.
# Shape is capped at |skew| = 1.8 for drawing so the picture stays a clear hump.
skew_density <- function(s, z) {
  s <- max(min(s, 1.8), -1.8)
  if (abs(s) < 0.01) return(dnorm(z))
  k <- 4 / s^2
  d <- dgamma(k + sign(s) * z * sqrt(k), shape = k) * sqrt(k)
  d[!is.finite(d)] <- NA
  d
}

# Theoretical mean / median / mode of that curve (standard units)
skew_stats <- function(s) {
  s <- max(min(s, 1.8), -1.8)
  if (abs(s) < 0.01) return(c(mean = 0, median = 0, mode = 0))
  k <- 4 / s^2
  c(mean = 0,
    median = sign(s) * (qgamma(0.5, shape = k) - k) / sqrt(k),
    mode   = sign(s) * (-1 / sqrt(k)))
}

# Symmetric curve (mean 0, SD 1) with a given EXCESS kurtosis
#   > 0 : Student-t shape (sharp peak, heavy tails)
#   < 0 : flat-topped shape (light tails), drawn down to -1.15
kurt_density <- function(k, z) {
  if (abs(k) < 1e-6) return(dnorm(z))
  if (k > 0) {
    nu <- 6 / k + 4; sc <- sqrt((nu - 2) / nu)
    return(dt(z / sc, df = nu) / sc)
  }
  k <- max(k, -1.15)
  f <- function(b) exp(lgamma(5 / b) + lgamma(1 / b) - 2 * lgamma(3 / b)) - 3 - k
  b <- uniroot(f, c(2, 1000))$root
  a <- sqrt(exp(lgamma(1 / b) - lgamma(3 / b)))
  b / (2 * a * gamma(1 / b)) * exp(-(abs(z) / a)^b)
}

val_or_na <- function(v, na) if (isTRUE(na) || is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
f2 <- function(x) ifelse(is.na(x), "N/A", formatC(x, digits = 2, format = "f"))

# Numeric box + "N/A" tick box
na_input <- function(id, label, value) {
  div(class = "na-input",
      conditionalPanel(sprintf("!input.%s_na", id),
                       numericInput(id, label, value = value)),
      checkboxInput(paste0(id, "_na"), paste(label, "N/A"), FALSE))
}

COL <- list(right = "#dd6b20", left = "#3182ce", sym = "#38a169",
            lepto = "#e53e3e", platy = "#805ad5", normal = "#718096",
            mean = "#e53e3e", median = "#2c7a7b", mode = "#6b46c1", range = "#a0aec0")

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Skewness & Kurtosis", titleWidth = 280),

  dashboardSidebar(
    width = 280,
    div(style = "padding: 8px 15px;",
        h4("Your data summary", style = "color:#fff;"),
        na_input("min",    "Minimum", 20),
        na_input("max",    "Maximum", 110),
        na_input("mean",   "Mean",    52),
        na_input("median", "Median",  50),
        na_input("mode",   "Mode",    47),
        hr(style = "border-color:#555;"),
        h4("Coefficients", style = "color:#fff;"),
        numericInput("skew", "Skewness coefficient", value = 2.82, step = 0.1),
        numericInput("kurt", "Kurtosis coefficient", value = 1.5, step = 0.1),
        radioButtons("ktype", NULL,
                     choices = c("Excess kurtosis (normal = 0)" = "excess",
                                 "Pearson kurtosis (normal = 3)" = "pearson")),
        actionButton("submit", "Submit", icon = icon("chart-area"),
                     class = "btn-primary btn-lg",
                     style = "width:100%; margin:10px 0 20px 0; color:#fff;")
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background:#f4f6f8; }
      .interp { font-size:16px; line-height:1.7; color:#1a202c; margin:4px 0; }
      .interp .arrow { color:#718096; }
      .na-input .checkbox { margin-top:-8px; font-size:12px; }
      .note { color:#718096; font-size:12px; margin-top:6px; }
      .warn { color:#b7791f; font-size:13px; }
    "))),

    fluidRow(
      box(width = 12, title = "Interpretation", status = "primary", solidHeader = TRUE,
          uiOutput("interpretation"))
    ),

    fluidRow(
      box(width = 6, title = "Skewness graph", status = "warning", solidHeader = TRUE,
          plotlyOutput("skewPlot", height = "380px"), uiOutput("skewNote")),
      box(width = 6, title = "Kurtosis graph", status = "danger", solidHeader = TRUE,
          plotlyOutput("kurtPlot", height = "380px"), uiOutput("kurtNote"))
    ),

    fluidRow(
      box(width = 12, title = "Rules for decision", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          column(6,
                 h4("Skewness"),
                 tags$ul(
                   tags$li(HTML("<b>Positive skewness (&gt; 0) &rarr; Right-skewed:</b> long tail to the right (high end). Mean &gt; Median &gt; Mode.")),
                   tags$li(HTML("<b>Negative skewness (&lt; 0) &rarr; Left-skewed:</b> long tail to the left (low end). Mean &lt; Median &lt; Mode.")),
                   tags$li(HTML("<b>Skewness = 0 &rarr; Symmetric:</b> Mean = Median = Mode."))
                 )),
          column(6,
                 h4("Kurtosis (tailedness compared with a normal curve)"),
                 tags$ul(
                   tags$li(HTML("<b>Kurtosis &gt; 0 &rarr; Leptokurtic:</b> sharp peak, heavy tails, more extreme values (outliers).")),
                   tags$li(HTML("<b>Kurtosis &lt; 0 &rarr; Platykurtic:</b> flatter peak, lighter tails, fewer extreme values; data more evenly spread.")),
                   tags$li(HTML("<b>Kurtosis = 0 &rarr; Mesokurtic:</b> same tailedness as a normal curve."))
                 ),
                 p(class = "note", "Pearson kurtosis is converted to excess kurtosis by subtracting 3.")))
    )
  )
)

# ---------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  res <- eventReactive(input$submit, {
    validate(need(!is.na(input$skew), "Enter the skewness coefficient, then click Submit."),
             need(!is.na(input$kurt), "Enter the kurtosis coefficient, then click Submit."))

    mn  <- val_or_na(input$min,    input$min_na)
    mx  <- val_or_na(input$max,    input$max_na)
    avg <- val_or_na(input$mean,   input$mean_na)
    md  <- val_or_na(input$median, input$median_na)
    mo  <- val_or_na(input$mode,   input$mode_na)
    s   <- input$skew
    k_in <- input$kurt
    k   <- if (input$ktype == "pearson") k_in - 3 else k_in

    validate(need(is.na(mn) || is.na(mx) || mn < mx, "Minimum must be smaller than Maximum."),
             need(k >= -2, "Excess kurtosis cannot be below -2. Check the value or the kurtosis type."))

    st <- skew_stats(s)

    # Spread (SD) for drawing: Range/6 if min & max given, else from Mean-Median, else 1
    sd_est <- NA; sd_note <- ""
    if (!is.na(mn) && !is.na(mx)) {
      sd_est <- (mx - mn) / 6; sd_note <- "Spread of the curve estimated as SD ≈ (Max − Min) / 6."
    } else if (!is.na(avg) && !is.na(md) && abs(st["median"]) > 1e-6) {
      v <- (avg - md) / (-st["median"])
      if (is.finite(v) && v > 0) { sd_est <- unname(v); sd_note <- "Spread of the curve estimated from Mean and Median." }
    }
    if (is.na(sd_est)) { sd_est <- 1; sd_note <- "Not enough values to estimate spread, so standard units are shown." }

    # Centre: put the curve's mean at your Mean (or Median / Mode / middle of range)
    loc <- if (!is.na(avg)) avg else if (!is.na(md)) md - sd_est * st["median"] else
           if (!is.na(mo)) mo - sd_est * st["mode"] else if (!is.na(mn) && !is.na(mx)) (mn + mx) / 2 else 0
    loc <- unname(loc)

    # x-axis range
    xr <- if (!is.na(mn) && !is.na(mx)) { pad <- 0.05 * (mx - mn); c(mn - pad, mx + pad) } else
          loc + sd_est * c(-4, 4)

    list(mn = mn, mx = mx, avg = avg, md = md, mo = mo, s = s, k = k, k_in = k_in,
         st = st, sd = sd_est, sd_note = sd_note, loc = loc, xr = xr)
  })

  # ---------------- interpretation lines ----------------
  output$interpretation <- renderUI({
    r <- res()

    skew_line <- if (r$s > 0) {
      sprintf("<b>Skewness coefficient = %s (&gt; 0)</b> <span class='arrow'>&rarr;</span> The distribution is <b>positively skewed (right-skewed)</b>: the long tail is on the right and Mean &gt; Median &gt; Mode.", f2(r$s))
    } else if (r$s < 0) {
      sprintf("<b>Skewness coefficient = %s (&lt; 0)</b> <span class='arrow'>&rarr;</span> The distribution is <b>negatively skewed (left-skewed)</b>: the long tail is on the left and Mean &lt; Median &lt; Mode.", f2(r$s))
    } else {
      "<b>Skewness coefficient = 0</b> <span class='arrow'>&rarr;</span> The distribution is <b>symmetric</b>: Mean = Median = Mode."
    }

    klab <- if (input$ktype == "pearson") sprintf("Kurtosis coefficient = %s (excess = %s)", f2(r$k_in), f2(r$k)) else
            sprintf("Kurtosis coefficient = %s", f2(r$k))
    kurt_line <- if (r$k > 0) {
      sprintf("<b>%s (&gt; 0)</b> <span class='arrow'>&rarr;</span> The distribution is <b>leptokurtic</b>: sharp peak and heavy tails, so more extreme values (outliers).", klab)
    } else if (r$k < 0) {
      sprintf("<b>%s (&lt; 0)</b> <span class='arrow'>&rarr;</span> The distribution is <b>platykurtic</b>: flatter peak and lighter tails, so fewer extreme values.", klab)
    } else {
      sprintf("<b>%s (= 0)</b> <span class='arrow'>&rarr;</span> The distribution is <b>mesokurtic</b>: tails like a normal curve.", klab)
    }

    # Mean / Median / Mode order check
    order_line <- NULL
    vals <- c(Mean = r$avg, Median = r$md, Mode = r$mo)
    have <- vals[!is.na(vals)]
    if (length(have) >= 2) {
      sym <- function(a, b) if (a > b) "&gt;" else if (a < b) "&lt;" else "="
      txt <- paste0(names(have)[1], " (", f2(have[1]), ")")
      for (i in 2:length(have))
        txt <- paste0(txt, " ", sym(have[i - 1], have[i]), " ", names(have)[i], " (", f2(have[i]), ")")
      ok <- all(sign(round(diff(-have), 10)) == sign(r$s))
      order_line <- sprintf("<b>Order check:</b> %s %s", txt,
        if (ok) "<span style='color:#2f855a'>&#10004; agrees with the skewness sign.</span>"
        else    "<span style='color:#c53030'>&#10008; does not agree with the skewness sign; recheck your values.</span>")
    }

    range_line <- if (!is.na(r$mn) && !is.na(r$mx))
      sprintf("<b>Range</b> = Max &minus; Min = %s &minus; %s = %s", f2(r$mx), f2(r$mn), f2(r$mx - r$mn)) else NULL

    # Values outside min/max?
    warn <- NULL
    if (!is.na(r$mn) && !is.na(r$mx)) {
      out <- names(have)[have < r$mn | have > r$mx]
      if (length(out)) warn <- sprintf("<span class='warn'>Check: %s lies outside the Min–Max range.</span>",
                                       paste(out, collapse = ", "))
    }

    tagList(lapply(Filter(Negate(is.null), list(skew_line, kurt_line, order_line, range_line, warn)),
                   function(x) div(class = "interp", HTML(x))))
  })

  # ---------------- skewness graph ----------------
  output$skewPlot <- renderPlotly({
    r <- res()
    xs <- seq(r$xr[1], r$xr[2], length.out = 600)
    y  <- skew_density(r$s, (xs - r$loc) / r$sd) / r$sd
    ymax <- max(y, na.rm = TRUE) * 1.15
    col <- if (r$s > 0) COL$right else if (r$s < 0) COL$left else COL$sym
    title <- if (r$s > 0) "Right-skewed (positive)" else if (r$s < 0) "Left-skewed (negative)" else "Symmetric"

    p <- plot_ly() %>%
      add_lines(x = xs, y = y, name = "Your distribution", fill = "tozeroy",
                fillcolor = paste0(col, "40"), line = list(color = col, width = 3),
                hoverinfo = "skip")

    # Mean / Median / Mode lines: your value, or estimated if N/A
    fitted <- r$loc + r$sd * r$st
    for (nm in c("mean", "median", "mode")) {
      user <- switch(nm, mean = r$avg, median = r$md, mode = r$mo)
      v <- if (!is.na(user)) user else fitted[[nm]]
      lab <- paste0(tools::toTitleCase(nm), " = ", f2(v), if (is.na(user)) " (estimated)" else "")
      p <- p %>% add_segments(x = v, xend = v, y = 0, yend = ymax, name = lab,
                              text = lab, hoverinfo = "text",
                              line = list(color = COL[[nm]], width = 2.5,
                                          dash = if (is.na(user)) "dot" else "dash"))
    }
    # Min / Max lines
    for (nm in c("mn", "mx")) {
      v <- r[[nm]]
      if (!is.na(v)) {
        lab <- paste0(if (nm == "mn") "Min" else "Max", " = ", f2(v))
        p <- p %>% add_segments(x = v, xend = v, y = 0, yend = ymax, name = lab,
                                text = lab, hoverinfo = "text",
                                line = list(color = COL$range, width = 2))
      }
    }

    ann <- if (r$s != 0) list(list(
      x = if (r$s > 0) r$xr[1] + 0.8 * diff(r$xr) else r$xr[1] + 0.2 * diff(r$xr),
      y = ymax * 0.55, showarrow = FALSE, font = list(size = 14, color = col),
      text = if (r$s > 0) "Long tail →" else "← Long tail")) else list()

    p %>% layout(title = list(text = paste0("<b>", title, "</b>"), x = 0.02),
                 xaxis = list(title = "Value", range = r$xr, zeroline = FALSE),
                 yaxis = list(title = "Frequency (density)", range = c(0, ymax), showticklabels = FALSE),
                 legend = list(orientation = "h", y = -0.2),
                 annotations = ann, margin = list(t = 50)) %>%
      config(displaylogo = FALSE)
  })

  output$skewNote <- renderUI({
    r <- res()
    extra <- if (abs(r$s) > 1.8) " Very high skewness is drawn at ±1.8 so the shape stays readable." else ""
    p(class = "note", paste0(r$sd_note, " Dashed lines are your values; dotted lines are estimated for N/A values.", extra))
  })

  # ---------------- kurtosis graph ----------------
  output$kurtPlot <- renderPlotly({
    r <- res()
    xs <- seq(r$xr[1], r$xr[2], length.out = 700)
    z  <- (xs - r$loc) / r$sd
    col <- if (r$k > 0) COL$lepto else if (r$k < 0) COL$platy else COL$sym
    title <- if (r$k > 0) "Leptokurtic (sharp peak, heavy tails)" else
             if (r$k < 0) "Platykurtic (flat peak, light tails)" else "Mesokurtic (normal)"
    yu <- kurt_density(r$k, z) / r$sd
    yn <- dnorm(z) / r$sd
    ymax <- max(c(yu, yn)) * 1.15

    plot_ly() %>%
      add_lines(x = xs, y = yn, name = "Normal curve (kurtosis = 0)",
                line = list(color = COL$normal, width = 2, dash = "dash"), hoverinfo = "skip") %>%
      add_lines(x = xs, y = yu, name = sprintf("Your distribution (kurtosis = %s)", f2(r$k)),
                fill = "tozeroy", fillcolor = paste0(col, "33"),
                line = list(color = col, width = 3), hoverinfo = "skip") %>%
      layout(title = list(text = paste0("<b>", title, "</b>"), x = 0.02),
             xaxis = list(title = "Value", range = r$xr, zeroline = FALSE),
             yaxis = list(title = "Frequency (density)", range = c(0, ymax), showticklabels = FALSE),
             legend = list(orientation = "h", y = -0.2),
             annotations = list(list(
               x = r$loc, y = max(yu), ax = 70, ay = -25, arrowhead = 2,
               text = if (r$k > 0) "Taller, sharper peak" else if (r$k < 0) "Flatter peak" else "Same as normal",
               font = list(color = col))),
             margin = list(t = 50)) %>%
      config(displaylogo = FALSE)
  })

  output$kurtNote <- renderUI({
    r <- res()
    extra <- if (r$k < -1.15) " Kurtosis below −1.15 is drawn at −1.15 (the flattest smooth shape)." else ""
    p(class = "note", paste0("Both curves have the same centre and spread, so only the peak and tails differ.", extra))
  })
}

shinyApp(ui, server)
