explain_pca_css <- "
@import url('https://fonts.googleapis.com/css2?family=Noto+Sans+KR:wght@400;500;700&display=swap');

.explain-pca h3 {
  font-family: 'Noto Sans KR', 'Malgun Gothic', 'Apple SD Gothic Neo', 'Segoe UI', Arial, sans-serif;
  font-size: 30px;
  font-weight: 700;
  margin-top: 8px;
  margin-bottom: 14px;
}
.explain-pca h4 {
  font-family: 'Noto Sans KR', 'Malgun Gothic', 'Apple SD Gothic Neo', 'Segoe UI', Arial, sans-serif;
  font-size: 20px;
  font-weight: 700;
  margin-top: 26px;
  margin-bottom: 10px;
}
.explain-pca h5 {
  font-family: 'Noto Sans KR', 'Malgun Gothic', 'Apple SD Gothic Neo', 'Segoe UI', Arial, sans-serif;
  font-size: 15px;
  font-weight: 700;
}
"

join_and <- function(x) {
  if (length(x) <= 1) return(x)
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}

first_reach <- function(cum, p) {
  i <- which(cum >= p)
  if (length(i)) i[1] else NA_integer_
}

vec_to_img <- function(v, side = 64L) {
  matrix(as.numeric(v), nrow = side, ncol = side, byrow = TRUE)
}

photo_label <- function(info) {
  if (!is.null(info$id)) sprintf("ID %04d (%s)", info$id, info$source) else info$source
}

category_colors <- function(model) {
  cats <- sort(unique(model$meta$category))
  pal  <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7",
            "#D55E00", "#56B4E9", "#F0E442")
  stats::setNames(pal[seq_along(cats)], cats)
}

rebuild_vec <- function(scores, model, K) {
  v <- model$pca$center
  if (K >= 1) {
    v <- v + as.vector(model$pca$rotation[, seq_len(K), drop = FALSE] %*% scores[seq_len(K)])
  }
  pmin(pmax(v, model$pixel_range[1]), model$pixel_range[2])
}

plot_gray <- function(vec, model, hover = "Brightness") {
  side <- model$pixel_size[1]
  rng  <- model$pixel_range
  p <- plotly::plot_ly(
    x = seq_len(side), y = seq_len(side), z = vec_to_img(vec, side),
    type = "heatmap",
    colorscale = list(c(0, "black"), c(1, "white")),
    zmin = rng[1], zmax = rng[2],
    hovertemplate = paste0("Column %{x}, Row %{y}<br>", hover, ": %{z:.1f}<extra></extra>"),
    colorbar = list(title = "Brightness")
  )
  p <- plotly::layout(
    p,
    xaxis = list(visible = FALSE, constrain = "domain"),
    yaxis = list(visible = FALSE, autorange = "reversed", scaleanchor = "x")
  )
  plotly::config(p, displayModeBar = FALSE)
}

plot_diverging <- function(vec, model, title = "Difference", hover = "Difference") {
  side <- model$pixel_size[1]
  m <- max(abs(vec))
  if (!is.finite(m) || m == 0) m <- 1
  p <- plotly::plot_ly(
    x = seq_len(side), y = seq_len(side), z = vec_to_img(vec, side),
    type = "heatmap",
    colorscale = list(c(0, "blue"), c(0.5, "white"), c(1, "red")),
    zmin = -m, zmax = m,
    hovertemplate = paste0("Column %{x}, Row %{y}<br>", hover, ": %{z:.3f}<extra></extra>"),
    colorbar = list(title = title)
  )
  p <- plotly::layout(
    p,
    xaxis = list(visible = FALSE, constrain = "domain"),
    yaxis = list(visible = FALSE, autorange = "reversed", scaleanchor = "x")
  )
  plotly::config(p, displayModeBar = FALSE)
}

plot_diff <- function(vec, model) {
  plot_diverging(vec, model, "Difference", "Difference")
}

plot_score_map <- function(pts, model, me = NULL, point_opacity = 0.6, point_size = 6) {
  cols <- category_colors(model)
  v    <- model$pca$sdev[1:2]^2 / model$total_variance * 100
  
  p <- plotly::plot_ly(
    data = pts, x = ~PC1, y = ~PC2, color = ~category, colors = cols,
    type = "scatter", mode = "markers",
    marker = list(size = point_size, opacity = point_opacity),
    text = ~sprintf("ID %04d<br>%s<br>PC1 = %.1f<br>PC2 = %.1f", id, category, PC1, PC2),
    hoverinfo = "text"
  )
  
  if (!is.null(me)) {
    p <- plotly::add_markers(
      p, x = me[1], y = me[2], inherit = FALSE, name = "Your photo",
      marker = list(symbol = "star", size = 18, color = "black", opacity = 1,
                    line = list(color = "white", width = 1.5)),
      text = sprintf("Your photo<br>PC1 = %.1f<br>PC2 = %.1f", me[1], me[2]),
      hoverinfo = "text"
    )
  }
  
  p <- plotly::layout(
    p,
    xaxis  = list(title = sprintf("PC1 (%.1f%% of variance)", v[1])),
    yaxis  = list(title = sprintf("PC2 (%.1f%% of variance)", v[2]), scaleanchor = "x"),
    legend = list(title = list(text = "Category"))
  )
  plotly::config(p, displaylogo = FALSE)
}

plot_biplot <- function(pts, model, me, arrows) {
  p <- plot_score_map(pts, model, me, point_opacity = 0.15, point_size = 4)
  
  # 카테고리별 타원(분포 범위의 약 2/3), 연한 칠, 중심점
  cols <- category_colors(model)
  th   <- seq(0, 2 * pi, length.out = 100)
  r    <- sqrt(stats::qchisq(0.68, df = 2))
  for (cat in names(cols)) {
    d <- pts[pts$category == cat, c("PC1", "PC2")]
    if (nrow(d) < 3) next
    m  <- colMeans(d)
    e  <- m + r * (t(chol(stats::cov(d))) %*% rbind(cos(th), sin(th)))   # 2 x 100
    cc <- grDevices::col2rgb(cols[[cat]])[, 1]
    tint <- function(a) sprintf("rgba(%d,%d,%d,%.2f)", cc[1], cc[2], cc[3], a)
    
    p <- plotly::add_trace(
      p, x = e[1, ], y = e[2, ], type = "scatter", mode = "lines",
      inherit = FALSE, showlegend = FALSE, hoverinfo = "skip",
      fill = "toself", fillcolor = tint(0.07),
      line = list(color = tint(0.5), width = 1.5)
    )
    p <- plotly::add_markers(
      p, x = m[1], y = m[2], inherit = FALSE, showlegend = FALSE,
      marker = list(symbol = "diamond", size = 12, color = cols[[cat]],
                    line = list(color = "white", width = 1.5)),
      text = sprintf("%s center<br>PC1 = %.1f<br>PC2 = %.1f", cat, m[1], m[2]),
      hoverinfo = "text"
    )
  }
  
  p <- plotly::add_markers(
    p, x = arrows$x, y = arrows$y, inherit = FALSE, showlegend = FALSE,
    marker = list(size = 10, color = "rgba(0,0,0,0)"),
    text = sprintf("Region %s<br>Average loading on PC1: %.4f<br>Average loading on PC2: %.4f",
                   arrows$label, arrows$l1, arrows$l2),
    hoverinfo = "text"
  )
  
  heads <- lapply(seq_len(nrow(arrows)), function(i) list(
    x = arrows$x[i], y = arrows$y[i], ax = 0, ay = 0,
    xref = "x", yref = "y", axref = "x", ayref = "y",
    showarrow = TRUE, arrowhead = 2, arrowsize = 1, arrowwidth = 1.5,
    arrowcolor = "#333333", text = ""
  ))
  tags <- lapply(seq_len(nrow(arrows)), function(i) list(
    x = arrows$x[i], y = arrows$y[i], xref = "x", yref = "y",
    text = arrows$label[i], showarrow = FALSE,
    xanchor = if (arrows$x[i] >= 0) "left" else "right",
    yanchor = if (arrows$y[i] >= 0) "bottom" else "top",
    font = list(size = 12, color = "#333333")
  ))
  
  plotly::layout(p, annotations = c(heads, tags))
}

photo_picker_ui <- function(id, show_id = TRUE, show_k = TRUE, extra = NULL) {
  ns <- NS(id)
  div(
    class = "photo-picker",
    style = "display:flex; align-items:center; flex-wrap:wrap; gap:10px; margin:6px 0 16px 0; font-size:90%;",
    tags$style(HTML(".photo-picker .form-group { margin-bottom: 0; }")),
    
    if (show_id) tagList(
      span(tags$b("Catalogue ID")),
      numericInput(ns("pick_id"), NULL, value = 1, min = 1, max = 2000,
                   step = 1, width = "90px"),
      actionButton(ns("pick_random"), "Random", class = "btn-sm")
    ),
    
    if (show_k) tagList(
      span(style = if (show_id) "margin-left:14px;" else "", tags$b("K")),
      sliderInput(ns("pick_K"), NULL, min = 1, max = 150, value = 40,
                  step = 1, ticks = FALSE, width = "220px")
    ),
    
    extra,
    
    if (show_id) uiOutput(ns("pick_info"), inline = TRUE)
  )
}

photo_picker_server <- function(id, safe_info, model, root_session) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      updateNumericInput(session, "pick_id",
                         min = min(model$meta$id), max = max(model$meta$id))
      updateSliderInput(session, "pick_K",
                        min = 1, max = ncol(model$pca$rotation))
    })
    
    # ID
    observeEvent(input$pick_id, {
      id_value <- suppressWarnings(as.integer(input$pick_id))
      req(!is.na(id_value), id_value %in% model$meta$id)
      info <- safe_info()
      already <- !is.null(info) &&
        identical(info$source, "Catalogue") &&
        identical(as.integer(info$id), id_value)
      if (!already) {
        updateRadioButtons(root_session, "src", selected = "Catalogue")
        updateNumericInput(root_session, "cat_id", value = id_value)
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$pick_random, {
      updateNumericInput(session, "pick_id", value = sample(model$meta$id, 1))
    })
    
    k_settled <- debounce(reactive(input$pick_K), 400)
    observeEvent(k_settled(), {
      k <- suppressWarnings(as.integer(k_settled()))
      req(!is.na(k), k >= 1)
      info <- safe_info()
      if (is.null(info) || !identical(as.integer(info$K), k)) {
        updateSliderInput(root_session, "K", value = k)
      }
    }, ignoreInit = TRUE)
    
    observeEvent(safe_info(), {
      info <- safe_info()
      if (is.null(info)) return()
      
      if (!is.null(input$pick_id) &&
          identical(info$source, "Catalogue") && !is.null(info$id) &&
          !identical(as.integer(input$pick_id), as.integer(info$id))) {
        updateNumericInput(session, "pick_id", value = info$id)
      }
      if (!is.null(input$pick_K) &&
          !identical(as.integer(input$pick_K), as.integer(info$K))) {
        updateSliderInput(session, "pick_K", value = info$K)
      }
    })
    
    output$pick_info <- renderUI({
      info <- safe_info()
      if (is.null(info)) {
        return(span(style = "color:#666;", "No photo selected"))
      }
      lab <- if (!is.null(info$id)) sprintf("ID %04d", info$id) else "No ID"
      tc  <- info$true_category
      cat_label <- if (is.null(tc) || is.na(tc) || !nzchar(tc)) "category unknown" else tc
      span(style = "color:#666;",
           paste(lab, info$source, cat_label, sep = " \u00b7 "))
    })
  })
}

# ============================================================
# UI
# ============================================================
explain_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(explain_pca_css)),
    
    div(
      class = "explain-pca",
      withMathJax(
        navlistPanel(
          id = ns("steps"),
          widths = c(2, 10),
          well = FALSE,
          
          # ---------------- Intro ----------------
          tabPanel("Intro", value = "intro",
                   uiOutput(ns("intro"))
          ),
          
          # ---------------- Step 1 ----------------
          tabPanel("Step 1. Average shoe", value = "step1",
                   h3("Step 1. Average shoe"),
                   h4("Formula"),
                   p("$$\\bar{x} = \\frac{1}{n}\\sum_{i=1}^{n} x_i$$"),
                   uiOutput(ns("step1_text")),
                   h4("The average shoe"),
                   p(em("Hover over the picture to see the average brightness at each pixel.")),
                   plotly::plotlyOutput(ns("avg_plot"), height = "420px"),
                   h4("What do we see?"),
                   p("The average shoe has a bright background with a gray shape in the middle. It does not look like any single category of shoe. The entire part is blurry because sandals, boots, and other types are mixed together. This imaginary shoe is the reference point for every comparison that follows."),
                   h4("Details"),
                   tags$ul(
                     tags$li(
                       tags$b("Why do we need to center? "),
                       "PCA looks at how the photos spread around the average. If we do not subtract the average first, the first PC tends to describe the average shoe itself, not the differences between shoes."
                     )
                   )
          ),
          
          # ---------------- Step 2 ----------------
          tabPanel("Step 2. Photo \u2212 average", value = "step2",
                   h3("Step 2. Photo \u2212 average"),
                   uiOutput(ns("step2_header")),
                   h4("Formula"),
                   p("$$x - \\bar{x}$$"),
                   uiOutput(ns("step2_formula_text")),
                   h4("Your photo, the average shoe, and the difference"),
                   p(em("Three pictures side by side. Hover over a picture to see the value at each pixel.")),
                   
                   photo_picker_ui(ns("pick2"), show_k = FALSE),   # ID만
                   
                   fluidRow(
                     column(4,
                            h5("Your photo (x)"),
                            plotly::plotlyOutput(ns("x_plot"), height = "300px")
                     ),
                     column(4,
                            h5("Average shoe (x\u0304)"),
                            plotly::plotlyOutput(ns("xbar_plot"), height = "300px")
                     ),
                     column(4,
                            h5("Difference (x \u2212 x\u0304)"),
                            plotly::plotlyOutput(ns("diff_plot"), height = "300px"),
                            p(style = "text-align:center; color:#666;",
                              "Darker than average \u2190 0 \u2192 Brighter than average")
                     )
                   ),
                   h4("What do we see?"),
                   uiOutput(ns("step2_obs")),
                   h4("Details: What does this step do?"),
                   tags$ul(
                     tags$li(tags$b("What we remove: "),
                             "the part that all shoes share, which is the average shoe."),
                     tags$li(tags$b("What stays: "),
                             "what makes this photo different from the others. PCA works with these differences, not with the raw image data itself."),
                     tags$li(tags$b("Why it matters: "),
                             "In Step 5, we compare this difference image with each eigenshoe to get the scores.")
                   )
          ),
          
          # ---------------- Step 3 ----------------
          tabPanel("Step 3. Score map", value = "step3",
                   h3("Step 3. Score map"),
                   uiOutput(ns("step3_header")),
                   h4("Catalogue photos on PC1 and PC2"),
                   p(em("Each dot is one catalogue photo, colored by category. The star is your photo. Hover over a dot to see its id, category, and scores. Click a category in the legend to hide or show it.")),
                   
                   photo_picker_ui(ns("pick3"), show_k = FALSE),   # ID만
                   
                   plotly::plotlyOutput(ns("score_map"), height = "520px"),
                   h4("What do we see?"),
                   uiOutput(ns("step3_obs")),
                   uiOutput(ns("step3_remark")),
                   h4("Details"),
                   tags$ul(
                     tags$li(tags$b("What is a score map? "),
                             "Each photo is turned into a few numbers, called scores. Here we draw the first two scores as a dot."),
                     tags$li(tags$b("Why only two PCs? "),
                             "Two numbers are easy to draw on a plot. The real match uses more PCs (K), which is why photos that look close on this map may not be the final closest match.")
                   )
          ),
          
          # ---------------- Step 4 ----------------
          tabPanel("Step 4. Eigenshoes", value = "step4",
                   h3("Step 4. Eigenshoes"),
                   uiOutput(ns("step4_text")),
                   
                   h4("The first 12 eigenshoes"),
                   p(em("Each picture is one PC, with the share of variance it explains in the title (for example, \"PC1 (30.1%)\"). Red pixels have positive weights and blue pixels have negative weights. If loading is near 0, the color is white which means that the following pixel does not contribute to specific PC. Each picture has its own color scale, so the shades cannot be compared between different PCs. The sign of a PC is arbitrary, so red and blue may swap if we run PCA again. Note not to focus on what red means — look at the contrast between the two colors and the color intensity of *each PC* instead.")),
                   plotOutput(ns("eigen_grid"), height = "480px"),
                   
                   h4("How much variance does each PC explain?"),
                   uiOutput(ns("step4_var_caption")),
                   
                   photo_picker_ui(ns("pick4"), show_id = FALSE),   # K만
                   
                   plotly::plotlyOutput(ns("var_plot"), height = "380px"),
                   
                   h4("Which parts of the photo move it on the map?"),
                   p(em("This is the score map from Step 3, with arrows added (biplot). The dots are lighter than in Step 3, and each ellipse covers about two thirds of the photos of one category, with a diamond marking its center. We cut the 64 \u00d7 64 picture into 16 regions (the picture on the right shows where they are). Each arrow shows the average weight (loading) of the pixels in one region on PC1 (left or right) and on PC2 (up or down). Hover over the tip of an arrow to see its values.")),
                   tags$ul(
                     tags$li("An arrow that points right means: photos that are brighter than average in this region tend to land further right (higher PC1). Photos that are darker there tend to land further left. The same holds for up and down (PC2)."),
                     tags$li("A long arrow means the region matters a lot for PC1 and PC2. A short arrow means it matters little."),
                     tags$li("Arrow lengths are scaled to fit the map, so only the direction and the relative length matter.")
                   ),
                   
                   photo_picker_ui(ns("pick4b"), show_k = FALSE),   # ID만
                   
                   fluidRow(
                     column(8, plotly::plotlyOutput(ns("biplot"), height = "560px")),
                     column(4,
                            h5("The 16 regions"),
                            plotOutput(ns("region_ref"), height = "260px"),
                            p(style = "color:#666;",
                              "R1C1 is the top left region and R4C4 is the bottom right region.")
                     )
                   ),
                   
                   h4("What do we see?"),
                   uiOutput(ns("step4_obs")),
                   
                   h4("Connection to class"),
                   p("Remind the cancer study in the midterm exam. PC1 had a larger variance than PC2, but PC2 was better at separating the subtypes. Why does this happen even though PC1 explains more variance? This shows that a large variance does not mean that the PC carries information about *everything*. It may mainly carry information about *specific variables*."),
                   p("Biplot above shows the same idea. The arrow of R3C2 is almost flat, so its loading on PC2 is close to zero. This means the PC2 score hardly depends on the brightness of R3C2. Two photos that differ a lot in R3C2 can still have the same PC2 score. This is the reason why Shoe Search uses K PCs, not just two."),
                   
                   h4("Details: Why are the PCs eigenshoes? (a quick reminder from class)"),
                   p("$$\\max_{v}\\ \\mathrm{Var}(v^T x)\\ \\rightarrow\\ v \\text{ is an eigenvector of } \\mathrm{Var}(x)\\ \\rightarrow\\ \\mathrm{Var}(x) = \\tfrac{1}{n-1}X^T X\\ \\rightarrow\\ X^T X \\text{ has an SVD}$$"),
                   p("$$\\rightarrow\\ \\text{the columns of } V \\text{ are its eigenvectors}\\ \\rightarrow\\ \\text{the columns of the rotation matrix } V \\text{ are the PCs}$$"),
                   p("So each PC is a column of the rotation matrix. It has one weight per pixel, and when we draw it as a 64 \u00d7 64 picture, we get an eigenshoe.")
          ),
          
          # ---------------- Step 5 ----------------
          tabPanel("Step 5. Scores", value = "step5",
                   h3("Step 5. Scores"),
                   uiOutput(ns("step5_header")),
                   p("A score tells how strongly your photo's difference picture (from Step 2) matches the pattern of one eigenshoe (from Step 4). We get it by multiplying the two pictures pixel by pixel and adding everything up:"),
                   p("$$z_k = \\sum_{\\text{pixels}} (x - \\bar{x})_p \\cdot v_{k,p}$$"),
                   p("Here, \\(z_k\\) is the score on PC \\(k\\), and \\(v_k\\) is the \\(k\\)-th eigenshoe."),
                   
                   h4("Score and projection"),
                   p("A score (\\(z_k\\)) is one number for each PC. We use the scores in the bars below, and Shoe Search compares the scores of two photos to decide how close they are."),
                   p("A projection (\\(z_k \\cdot v_k\\)) is one picture: the score times the eigenshoe. It shows what PC \\(k\\) adds to your photo. In Step 6, we add these pictures to the average shoe to rebuild your photo."),
                   p("The first two scores, \\(z_1\\) and \\(z_2\\), give the position of your star in Step 3."),
                   
                   h4("Your first K scores"),
                   uiOutput(ns("step5_A_caption")),
                   
                   photo_picker_ui(ns("pick5")),   # ID + K
                   
                   plotly::plotlyOutput(ns("score_bars"), height = "340px"),
                   
                   h4("How is one score computed?"),
                   p(em("Choose a PC with the slider. You see three pictures:")),
                   tags$ol(
                     tags$li(tags$b("Difference picture"), " (x \u2212 x\u0304): your photo minus the average shoe (Step 2)"),
                     tags$li(tags$b("Eigenshoe"), " (v_k): the pattern of this PC (Step 4)"),
                     tags$li(tags$b("Product:"), " the two pictures multiplied pixel by pixel, the sum of this picture's every pixel is z_k")
                   ),
                   photo_picker_ui(ns("pick5b"), show_k = FALSE, extra = tagList(
                     span(style = "margin-left:14px;", tags$b("PC")),
                     sliderInput(ns("score_k"), NULL, min = 1, max = 150, value = 1,
                                 step = 1, ticks = FALSE, width = "220px")
                   )),
                   fluidRow(
                     column(4,
                            h5("Difference picture (x \u2212 x\u0304)"),
                            plotly::plotlyOutput(ns("sc_diff"), height = "280px")
                     ),
                     column(4,
                            h5("Eigenshoe (v_k)"),
                            plotly::plotlyOutput(ns("sc_eigen"), height = "280px")
                     ),
                     column(4,
                            h5("Product"),
                            plotly::plotlyOutput(ns("sc_prod"), height = "280px")
                     )
                   ),
                   uiOutput(ns("step5_B_sum")),
                   p("In the product picture, red pixels are where the first two pictures have the same sign, so they push the score up. Blue pixels are where the signs are different, so they push the score down."),
                   
                   h4("What do we see?"),
                   uiOutput(ns("step5_obs")),
                   
                   h4("Details: Why default to 40 PCs?"),
                   tags$ul(
                     tags$li(tags$b("Most of the variation is kept. "),
                             "The first 40 PCs explain about 83% of the variation in the photos, and they use only about 1% of the 4,096 numbers of a photo."),
                     tags$li(tags$b("More PCs add little. "),
                             "The running total curve is already flattening here. To reach 90%, we need about 60 more PCs, and they add only about 7 percentage points.")
                   )
          ),
          
          # ---------------- Step 6 ----------------
          tabPanel("Step 6. Rebuild", value = "step6",
                   h3("Step 6. Rebuild"),
                   uiOutput(ns("step6_header")),
                   p("We can go back from the scores to a picture. We start with the average shoe (Step 1) and add the projections from Step 5, one PC at a time:"),
                   h4("Formula"),
                   p("$$\\hat{x}_K = \\bar{x} + \\sum_{k=1}^{K} z_k \\, v_k$$"),
                   p("Here, \\(\\hat{x}_K\\) is your photo rebuilt from the first K PCs. With K = 0, we only have the average shoe. The more PCs we add, the closer the rebuilt photo gets to the original."),
                   
                   h4("Rebuilt photos for different K"),
                   uiOutput(ns("step6_A_caption")),
                   
                   photo_picker_ui(ns("pick6")),   # ID + K
                   
                   plotOutput(ns("rebuild_grid"), height = "460px"),
                   
                   h4("Add one PC at a time"),
                   p(em("Choose how many PCs to use with the slider. You see your original photo, the photo rebuilt from the first K PCs, and the difference between them. This difference is what we missed during PCA rebuilding. As you use more PCs, you can see that the difference picture is mostly colored with white which means the rebuild photo is similar to your original photo.")),
                   photo_picker_ui(ns("pick6b"), show_k = FALSE, extra = tagList(
                     span(style = "margin-left:14px;", tags$b("PC")),
                     sliderInput(ns("rb_k"), NULL, min = 0, max = 150, value = 40,
                                 step = 1, ticks = FALSE, width = "220px")
                   )),
                   fluidRow(
                     column(4,
                            h5("Original photo (x)"),
                            plotly::plotlyOutput(ns("rb_orig"), height = "300px")
                     ),
                     column(4,
                            h5("Rebuilt photo"),
                            plotly::plotlyOutput(ns("rb_new"), height = "300px")
                     ),
                     column(4,
                            h5("Difference (original \u2212 rebuilt)"),
                            plotly::plotlyOutput(ns("rb_diff"), height = "300px"),
                            p(style = "text-align:center; color:#666;",
                              "Rebuilt is brighter \u2190 0 \u2192 Rebuilt is darker")
                     )
                   ),
                   uiOutput(ns("step6_B_text")),
                   
                   h4("What do we see?"),
                   uiOutput(ns("step6_obs")),
                   
                   h4("Why do we not use all PCs?"),
                   uiOutput(ns("step6_why")),
                   
                   h4("Details"),
                   tags$ul(
                     tags$li(tags$b("The pieces are the same as before: "),
                             "the average shoe (Step 1), the scores (Step 5), and the eigenshoes (Step 4). Nothing new is added in this step."),
                     tags$li(tags$b("Same rebuild as Shoe Search: "),
                             "Shoe Search shows your photo rebuilt from the first K PCs. Here we can see how the picture changes when K changes.")
                   )
          )
        )
      )
    )
  )
}

# ============================================================
# Server
# ============================================================
explain_pca_server <- function(id, photo_info, model, root_session) {
  moduleServer(id, function(input, output, session) {
    
    safe_info <- reactive(tryCatch(photo_info(), error = function(e) NULL))
    
    for (pid in c("pick2", "pick3", "pick4", "pick4b", "pick5", "pick5b", "pick6", "pick6b")) { 
      photo_picker_server(pid, safe_info, model, root_session)
    }
    
    star_xy <- reactiveVal(NULL)
    observe({
      info <- safe_info()
      star_xy(if (is.null(info)) NULL else project_photo(info$img_vec, model, 2))
    })
    
    score_pts <- local({
      ids <- if (is.null(rownames(model$pca$x))) model$meta$id
      else as.integer(rownames(model$pca$x))
      data.frame(
        id       = ids,
        category = model$meta$category[match(ids, model$meta$id)],
        PC1      = model$pca$x[, 1],
        PC2      = model$pca$x[, 2],
        stringsAsFactors = FALSE
      )
    })
    
    n_pc      <- ncol(model$pca$rotation)
    var_ratio <- model$pca$sdev[seq_len(n_pc)]^2 / model$total_variance
    cum_ratio <- cumsum(var_ratio)
    
    # ---------------- Intro ----------------
    output$intro <- renderUI({
      n_photos <- format(nrow(model$meta), big.mark = ",")
      side     <- model$pixel_size[1]
      n_pixels <- format(prod(model$pixel_size), big.mark = ",")
      cats     <- sort(unique(model$meta$category))
      
      tagList(
        h3("Explain PCA"),
        
        h4("The problem"),
        p(sprintf(
          "A shoe store has a catalogue of %s photos in %d categories: %s. A customer uploads a new photo and wants to see the most similar shoes in the catalogue. The store also wants to guess the category of the new shoe.",
          n_photos, length(cats), join_and(cats))),
        p(sprintf(
          "The simple way is to compare photos pixel by pixel. But each photo has %d \u00d7 %d = %s pixel values, and comparing all of them is heavy. It also reacts to small details that do not tell us much about the type of shoe.",
          side, side, n_pixels)),
        
        h4("The idea of PCA"),
        p("PCA finds a few directions in which the photos differ the most. Each photo is then described by a few numbers, one for each direction, instead of all the pixels. We compare photos using these few numbers."),
        
        h4("What this tab shows"),
        p("This tab follows the photo you chose in Shoe Search through six steps:"),
        tags$ol(
          tags$li(tags$b("Average shoe:"), " the reference point of all photos"),
          tags$li(tags$b("Photo \u2212 average:"), " how your photo differs from the reference"),
          tags$li(tags$b("Score map:"), " where your photo lands among the catalogue photos"),
          tags$li(tags$b("Eigenshoes:"), " the directions PCA finds, drawn as pictures"),
          tags$li(tags$b("Scores:"), " the few numbers that describe your photo"),
          tags$li(tags$b("Rebuild:"), " how well your photo can be rebuilt from those numbers")
        )
      )
    })
    
    # ---------------- Step 1 ----------------
    output$step1_text <- renderUI({
      n_photos <- format(nrow(model$meta), big.mark = ",")
      side     <- model$pixel_size[1]
      n_pixels <- format(prod(model$pixel_size), big.mark = ",")
      
      tagList(
        p(sprintf(
          "Here, n = %s is the number of photos. Each photo has %d \u00d7 %d = %s pixel values. We average the pixels at the same position across all %s photos. This gives us %s numbers, so we can draw them as a %d \u00d7 %d picture.",
          n_photos, side, side, n_pixels, n_photos, n_pixels, side, side)),
        h4("What is centering?"),
        p("Centering means subtracting the average shoe from every photo. The average shoe then becomes the new starting point (the origin). We do the subtraction in Step 2."),
        h4("Where we use it later"),
        p("We will use the average shoe again in Step 2 (difference), Step 5 (scores), and Step 6 (rebuild).")
      )
    })
    
    output$avg_plot <- plotly::renderPlotly({
      plot_gray(model$pca$center, model, "Average brightness")
    })
    
    # ---------------- Step 2 ----------------
    output$step2_header <- renderUI({
      info <- safe_info()
      if (is.null(info)) {
        return(p(em("Pick a photo in the Shoe Search tab first.")))
      }
      tagList(
        p(tags$b("Selected photo: "), photo_label(info)),
        p("In Step 1, we found the average shoe, our reference point. Now we measure how far your photo is from it.")
      )
    })
    
    output$step2_formula_text <- renderUI({
      n_pixels <- format(prod(model$pixel_size), big.mark = ",")
      side     <- model$pixel_size[1]
      p(sprintf(
        "Here, x is your photo and the bar on x is the average shoe. We subtract pixel by pixel, so the result is also %s numbers. We can draw them as a %d \u00d7 %d picture that shows where your photo is brighter or darker than the average shoe.",
        n_pixels, side, side))
    })
    
    output$x_plot <- plotly::renderPlotly({
      info <- safe_info()
      req(info)
      plot_gray(info$img_vec, model, "Brightness")
    })
    
    output$xbar_plot <- plotly::renderPlotly({
      plot_gray(model$pca$center, model, "Average brightness")
    })
    
    output$diff_plot <- plotly::renderPlotly({
      info <- safe_info()
      req(info)
      plot_diff(info$img_vec - model$pca$center, model)
    })
    
    output$step2_obs <- renderUI({
      info <- safe_info()
      req(info)
      d <- info$img_vec - model$pca$center
      brighter <- sprintf("%.1f", mean(d > 0) * 100)
      darker   <- sprintf("%.1f", mean(d < 0) * 100)
      tagList(
        p("Red means your photo is brighter than the average shoe at that spot. Blue means it is darker. White means it is about the same."),
        p(sprintf(
          "Compared with the average shoe, %s%% of the pixels in your photo are brighter and %s%% are darker.",
          brighter, darker))
      )
    })
    
    # ---------------- Step 3 ----------------
    output$step3_header <- renderUI({
      info <- safe_info()
      tagList(
        if (is.null(info)) {
          p(em("Pick a photo in the Shoe Search tab first."))
        } else {
          p(tags$b("Selected photo: "), photo_label(info))
        },
        p("PCA turns each photo into a few numbers. Here we use only the first two, PC1 and PC2, so every photo becomes one dot on a map with x and y axes. PC1 and PC2 are the two directions along which shoes differ the most. We look at what they mean in Step 4: each PC is a column of the rotation matrix, a list of weights (loadings) that tell how much each pixel counts for that PC. We show how each position is computed in Step 5.")
      )
    })
    
    output$score_map <- plotly::renderPlotly({
      plot_score_map(score_pts, model, star_xy())
    })
    
    output$step3_obs <- renderUI({
      info <- safe_info()
      me   <- star_xy()
      var2 <- sum(model$pca$sdev[1:2]^2) / model$total_variance * 100
      
      tagList(
        p("Dots with the same color tend to land near each other. This means shoes of the same type look alike to PCA."),
        p("Boots are mostly in the upper part of the map, so they are separated mainly along PC2. The other categories overlap more. This is because we use only two PCs here."),
        if (!is.null(me)) {
          d2   <- sqrt((score_pts$PC1 - me[1])^2 + (score_pts$PC2 - me[2])^2)
          idx  <- order(d2)
          idx  <- idx[d2[idx] > 0][1:10]          
          near <- table(score_pts$category[idx])
          top  <- names(near)[which.max(near)]
          p(sprintf("Your photo is at PC1 = %.1f and PC2 = %.1f. Among its 10 closest dots on this map, %d are %s.",
                    me[1], me[2], max(near), top))
        },
        
        p(sprintf("These two PCs show only %.1f%% of the full information, so this map is only part of the picture.",
                  var2))
      )
    })
    
    output$step3_remark <- renderUI({
      p(tags$b("Remark. "),
        "Closeness on this map is not the same as the final match. This map uses only two PCs.")
    })
    
    # ---------------- Step 4 ----------------
    
    observe({
      updateSliderInput(session, "score_k", min = 1, max = n_pc)
      updateSliderInput(session, "rb_k", min = 0, max = n_pc)
    })
    
    output$step4_text <- renderUI({
      n_pixels <- format(prod(model$pixel_size), big.mark = ",")
      side     <- model$pixel_size[1]
      tagList(
        p("In Step 3, we used PC1 and PC2 as the two axes of the map. Here we see what these PCs look like."),
        p(sprintf(
          "Each PC is a list of %s weights, one for each pixel. These weights are called loadings. We draw these weights as a %d \u00d7 %d picture and call it an eigenshoe. It shows which pixels matter for that PC. In Step 5, we compare your difference picture (from Step 2) with these eigenshoes to get your scores.",
          n_pixels, side, side))
      )
    })
    
    output$eigen_grid <- renderPlot({
      n_show <- min(12L, n_pc)
      side   <- model$pixel_size[1]
      cols   <- grDevices::colorRampPalette(c("blue", "white", "red"))(101)
      
      op <- graphics::par(mfrow = c(3, 4), mar = c(0.5, 0.5, 2.2, 0.5))
      for (k in seq_len(n_show)) {
        v   <- model$pca$rotation[, k]
        lim <- max(abs(v))
        if (!is.finite(lim) || lim == 0) lim <- 1
        m <- vec_to_img(v, side)
        graphics::image(
          seq_len(side), seq_len(side), t(m)[, side:1],
          col = cols, zlim = c(-lim, lim),
          axes = FALSE, xlab = "", ylab = "", asp = 1, useRaster = TRUE
        )
        graphics::title(
          main = sprintf("PC%d (%.1f%%)", k, var_ratio[k] * 100),
          cex.main = 1.1, line = 0.5
        )
      }
      graphics::par(op)
    }, res = 96)
    
    output$step4_var_caption <- renderUI({
      info <- safe_info()
      k_text <- if (is.null(info)) "the current K" else sprintf("the current K = %d", info$K)
      p(em(sprintf(
        "Bars show the variance explained by each PC. The line shows the running total. The dashed vertical line marks %s. PC1 and PC2, the two PCs used in the map below, are highlighted. To choose a good number of PCs, look for the point where the running total line flattens out. This model saves only the first %d PCs, so the running total may not reach 100%%. Hover over a bar to see the exact numbers.",
        k_text, n_pc)))
    })
    
    output$var_plot <- plotly::renderPlotly({
      info  <- safe_info()
      k_now <- if (is.null(info)) NULL else info$K
      pcs   <- seq_len(n_pc)
      
      p <- plotly::plot_ly()
      p <- plotly::add_bars(
        p, x = pcs, y = var_ratio * 100, name = "Each PC",
        marker = list(color = ifelse(pcs <= 2, "#D55E00", "#7f7f7f")),
        hovertemplate = "PC%{x}<br>Explains %{y:.2f}%<extra></extra>"
      )
      p <- plotly::add_lines(
        p, x = pcs, y = cum_ratio * 100, name = "Running total", yaxis = "y2",
        line = list(color = "#0072B2"),
        hovertemplate = "First %{x} PCs<br>Total %{y:.1f}%<extra></extra>"
      )
      
      shapes <- list()
      notes  <- list()
      if (!is.null(k_now)) {
        shapes <- list(list(
          type = "line", x0 = k_now, x1 = k_now, y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "black", dash = "dash", width = 1.5)
        ))
        notes <- list(list(
          x = k_now, y = 1, yref = "paper", text = sprintf("K = %d", k_now),
          showarrow = FALSE, xanchor = "left", yanchor = "bottom"
        ))
      }
      
      p <- plotly::layout(
        p,
        xaxis  = list(title = "PC number"),
        yaxis  = list(title = "Variance explained by each PC (%)"),
        yaxis2 = list(title = "Running total (%)", overlaying = "y",
                      side = "right", range = c(0, 100)),
        shapes = shapes, annotations = notes,
        legend = list(orientation = "h", y = 1.15),
        margin = list(r = 80)
      )
      plotly::config(p, displayModeBar = FALSE)
    })
    
    # biplot: 사진을 4 x 4 = 16개 영역으로 나누고, 영역마다 PC1, PC2 loading의 평균을 화살표로 그림
    region_n <- 4L
    region_arrows <- local({
      side <- model$pixel_size[1]
      bs   <- side %/% region_n                        # 영역 한 변의 픽셀 수 (16)
      px   <- seq_len(side^2)
      brow <- ((px - 1) %/% side) %/% bs + 1           # 영역 행 (위에서부터)
      bcol <- ((px - 1) %%  side) %/% bs + 1           # 영역 열 (왼쪽부터)
      rid  <- (brow - 1) * region_n + bcol             # 영역 번호 1~16
      load <- rowsum(model$pca$rotation[, 1:2], rid) / bs^2   # 영역별 평균 loading
      ids  <- as.integer(rownames(load))
      # 점수 범위에 맞게 화살표 길이를 늘림 (방향과 상대 길이만 의미)
      s <- 0.8 * min(max(abs(score_pts$PC1)), max(abs(score_pts$PC2))) /
        max(sqrt(rowSums(load^2)))
      data.frame(
        label = sprintf("R%dC%d", (ids - 1) %/% region_n + 1, (ids - 1) %% region_n + 1),
        l1 = load[, 1], l2 = load[, 2],
        x  = load[, 1] * s, y = load[, 2] * s,
        stringsAsFactors = FALSE
      )
    })
    
    output$biplot <- plotly::renderPlotly({
      plot_biplot(score_pts, model, star_xy(), region_arrows)
    })
    
    # 영역 위치 안내 그림: 평균 신발 위에 16개 영역과 이름을 표시
    output$region_ref <- renderPlot({
      side <- model$pixel_size[1]
      bs   <- side / region_n
      m    <- vec_to_img(model$pca$center, side)
      op <- graphics::par(mar = c(0.2, 0.2, 0.2, 0.2))
      graphics::image(
        seq_len(side), seq_len(side), t(m)[, side:1],
        col = grDevices::gray((0:255) / 255), zlim = model$pixel_range,
        axes = FALSE, xlab = "", ylab = "", asp = 1, useRaster = TRUE
      )
      edges   <- 0.5 + (0:region_n) * bs
      centers <- 0.5 + (seq_len(region_n) - 0.5) * bs
      graphics::abline(v = edges, h = edges, col = "#d73027", lwd = 1.5)
      for (r in seq_len(region_n)) {
        for (cc in seq_len(region_n)) {
          graphics::text(centers[cc], side + 1 - centers[r],
                         sprintf("R%dC%d", r, cc),
                         col = "#d73027", cex = 0.9, font = 2)
        }
      }
      graphics::par(op)
    }, res = 96)
    
    output$step4_obs <- renderUI({
      n90 <- first_reach(cum_ratio, 0.90)
      
      tagList(
        if (!is.na(n90)) {
          p(sprintf("The first %d PCs explain 90%% of the variation in the photos.", n90))
        } else {
          p(sprintf("The first %d PCs explain %.1f%% of the variation in the photos.",
                    n_pc, cum_ratio[n_pc] * 100))
        },
        p(sprintf("PC1 alone explains %.1f%% of the variation and PC2 explains %.1f%%, so these two together explain %.1f%%.",
                  var_ratio[1] * 100, var_ratio[2] * 100, cum_ratio[2] * 100)),
        p("The longest arrows are R2C3 and R3C2. R2C3 matters most for PC2, and R3C2 matters most for PC1."),
        p("The arrows in the middle rows (R2 and R3) are long, so the shoes differ most in the middle of the photo. The arrows in the top and bottom rows (R1 and R4) are short, because these rows look similar in all photos."),
        p("Boots are mostly in the upper left part of the map. The arrows of R3C1 and R3C4 point to the upper left, so boots tend to differ from other shoes in row 3."),
        p("A photo gets a high score on a PC when its difference picture matches the red and blue pattern of that eigenshoe.")
      )
    })
    
    # ---------------- Step 5 ----------------
    output$step5_header <- renderUI({
      info <- safe_info()
      if (is.null(info)) {
        p(em("Pick a photo in the Shoe Search tab first."))
      } else {
        p(tags$b("Selected photo: "), photo_label(info))
      }
    })
    
    output$step5_A_caption <- renderUI({
      info <- safe_info()
      k_text <- if (is.null(info)) "K" else sprintf("K = %d", info$K)
      p(em(sprintf(
        "Each bar is the score of your photo on one PC. We show the first %s PCs, the same K that Shoe Search uses. Blue bars are negative scores and red bars are positive scores. Note that images outside the catalogue (such as self-check or upload) are scored using the exact same mean and PCs without rebuilding the model. Hover over a bar to see the exact value.",
        k_text)))
    })
    
    output$score_bars <- plotly::renderPlotly({
      info <- safe_info()
      req(info)
      s   <- as.numeric(info$scores)
      pcs <- seq_along(s)
      
      p <- plotly::plot_ly(
        x = pcs, y = s, type = "bar",
        marker = list(color = ifelse(s >= 0, "#d73027", "#4575b4")),
        hovertemplate = "PC%{x}<br>Score: %{y:.1f}<extra></extra>"
      )
      p <- plotly::layout(
        p,
        xaxis = list(title = "PC number"),
        yaxis = list(title = "Score"),
        bargap = 0.15
      )
      plotly::config(p, displayModeBar = FALSE)
    })
    
    step5_vecs <- reactive({
      info <- safe_info()
      req(info)
      k <- req(input$score_k)
      d <- info$img_vec - model$pca$center
      v <- model$pca$rotation[, k]
      list(d = d, v = v, prod = d * v, k = k)
    })
    
    output$sc_diff <- plotly::renderPlotly({
      s <- step5_vecs()
      plot_diverging(s$d, model, "Difference", "Difference")
    })
    output$sc_eigen <- plotly::renderPlotly({
      s <- step5_vecs()
      plot_diverging(s$v, model, "Weight", "Weight")
    })
    output$sc_prod <- plotly::renderPlotly({
      s <- step5_vecs()
      plot_diverging(s$prod, model, "Product", "Product")
    })
    
    output$step5_B_sum <- renderUI({
      s <- step5_vecs()
      p("Sum of all pixels in the product picture = ",
        tags$b(sprintf("%.1f", sum(s$prod))),
        sprintf(". This number is the score on PC %d. You can double check this result with the bar plot above.", s$k))
    })
    
    output$step5_obs <- renderUI({
      info <- safe_info()
      req(info)
      s   <- as.numeric(info$scores)
      top <- head(order(abs(s), decreasing = TRUE), 2)
      
      tagList(
        p("A positive score means your photo matches the red part of the eigenshoe. A negative score means it matches the blue part. A score close to zero means the photo has little to do with that PC."),
        if (length(top) >= 2) {
          p(sprintf("Your photo has its largest scores on PC %d (%.1f) and PC %d (%.1f).",
                    top[1], s[top[1]], top[2], s[top[2]]))
        } else {
          p(sprintf("Your photo has its largest score on PC %d (%.1f).", top[1], s[top[1]]))
        }
      )
    })
    
    # ---------------- Step 6 ----------------
    
    full_scores <- reactive({
      info <- safe_info()
      req(info)
      as.numeric(project_photo(info$img_vec, model, n_pc))
    })
    
    output$step6_header <- renderUI({
      info <- safe_info()
      if (is.null(info)) {
        p(em("Pick a photo in the Shoe Search tab first."))
      } else {
        p(tags$b("Selected photo: "), photo_label(info))
      }
    })
    
    output$step6_A_caption <- renderUI({
      info <- safe_info()
      k_text <- if (is.null(info)) "the current K" else sprintf("K = %d", info$K)
      p(em(sprintf(
        "The first picture is your original photo. The other pictures are rebuilt from the first K PCs, with K in the title. The red title marks %s, the value used in Shoe Search.",
        k_text)))
    })
    
    output$rebuild_grid <- renderPlot({
      info <- safe_info()
      req(info)
      sc    <- full_scores()
      side  <- model$pixel_size[1]
      rng   <- model$pixel_range
      k_now <- min(info$K, n_pc)
      ks    <- sort(unique(pmin(c(1, 5, 10, 20, 50, 100, k_now), n_pc)))
      
      panels <- c(list(list(v = info$img_vec, title = "Original", col = "black")),
                  lapply(ks, function(k) {
                    is_now <- (k == k_now)
                    list(v = rebuild_vec(sc, model, k),
                         title = if (is_now) sprintf("K = %d (Shoe Search)", k)
                         else sprintf("K = %d", k),
                         col = if (is_now) "#d73027" else "black")
                  }))
      
      op <- graphics::par(mfrow = c(2, ceiling(length(panels) / 2)),
                          mar = c(0.5, 0.5, 2.2, 0.5))
      for (pn in panels) {
        m <- vec_to_img(pn$v, side)
        graphics::image(
          seq_len(side), seq_len(side), t(m)[, side:1],
          col = grDevices::gray((0:255) / 255), zlim = rng,
          axes = FALSE, xlab = "", ylab = "", asp = 1, useRaster = TRUE
        )
        graphics::title(main = pn$title, cex.main = 1.1, line = 0.5,
                        col.main = pn$col)
      }
      graphics::par(op)
    }, res = 96)
    
    step6_vecs <- reactive({
      info <- safe_info()
      req(info)
      k <- req(input$rb_k)
      k <- min(as.integer(k), n_pc)
      rb <- rebuild_vec(full_scores(), model, k)
      list(orig = info$img_vec, rb = rb, diff = info$img_vec - rb, k = k)
    })
    
    output$rb_orig <- plotly::renderPlotly({
      s <- step6_vecs()
      plot_gray(s$orig, model, "Brightness")
    })
    output$rb_new <- plotly::renderPlotly({
      s <- step6_vecs()
      plot_gray(s$rb, model, "Brightness")
    })
    output$rb_diff <- plotly::renderPlotly({
      s <- step6_vecs()
      plot_diverging(s$diff, model, "Difference", "Difference")
    })
    
    output$step6_B_text <- renderUI({
      s <- step6_vecs()
      err <- mean(abs(s$diff))
      if (s$k == 0) {
        p(sprintf("K = 0: no PC is used, so the rebuilt photo is the average shoe. On average, a pixel is off by %.1f brightness levels.", err))
      } else {
        p(sprintf("K = %d: the first %d PCs explain %.1f%% of the variation in the catalogue. On average, a pixel is off by %.1f brightness levels.",
                  s$k, s$k, cum_ratio[s$k] * 100, err))
      }
    })
    
    output$step6_obs <- renderUI({
      info <- safe_info()
      req(info)
      sc    <- full_scores()
      k_now <- min(info$K, n_pc)
      err_now <- mean(abs(info$img_vec - rebuild_vec(sc, model, k_now)))
      err_0   <- mean(abs(info$img_vec - rebuild_vec(sc, model, 0)))
      err_all <- mean(abs(info$img_vec - rebuild_vec(sc, model, n_pc)))
      
      tagList(
        p("With few PCs, the rebuilt photo shows only the rough shape and the large light and dark areas. With more PCs, finer details come back and the difference picture becomes lighter."),
        p(sprintf("For your photo, a pixel is off by %.1f brightness levels on average with the average shoe alone (K = 0), %.1f with K = %d (Shoe Search), and %.1f with all %d PCs.",
                  err_0, err_now, k_now, err_all, n_pc)),
        p("[TODO: describe what you see, e.g., at which K the shoe type becomes clear.]")
      )
    })
    
    output$step6_why <- renderUI({
      info <- safe_info()
      k_now <- if (is.null(info)) NULL else min(info$K, n_pc)
      n_pixels_num <- prod(model$pixel_size)
      n_pixels <- format(n_pixels_num, big.mark = ",")
      
      tagList(
        if (!is.null(k_now)) {
          p(sprintf("Your photo has %s pixel values. With K = %d, we describe it with only %d scores, which is %.1f%% of the original size.",
                    n_pixels, k_now, k_now, k_now / n_pixels_num * 100))
        } else {
          p(sprintf("Each photo has %s pixel values, but we describe it with only K scores.", n_pixels))
        },
        tags$ul(
          tags$li(tags$b("Smaller: "), "we keep and compare far fewer numbers for each photo."),
          tags$li(tags$b("Faster: "), "comparing K numbers is much quicker than comparing every pixel."),
          tags$li(tags$b("Main shape first: "), "the first PCs hold the large patterns, so the small details we drop are mostly not what tells shoe types apart.")
        ),
        p(sprintf("The cost: the rebuilt photo is not exactly the original, because the details in the later PCs are lost. This model has %d PCs in total, and we choose K to keep the main shape with as few numbers as possible.",
                  n_pc))
      )
    })
    
  })
}
