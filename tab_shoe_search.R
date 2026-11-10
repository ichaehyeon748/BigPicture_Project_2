
# ============================================================
# Project 2 - Shoe Search Tab
# Statistical Graphics with Big Data
#
# Requires:
#   pca_core.R
#   pca_model.rds
#
# Distance: Cosine (defined in pca_core.R)
# Default K: 40
#
# Optimization:
#   Catalogue: reuse precomputed PC scores.
#   Self-check / Upload: calculate 150 PC scores once.
#   K changes: reuse the first K scores.
# ============================================================


# ------------------------------------------------------------
# 1. Shoe Search UI
# ------------------------------------------------------------

shoe_search_ui <- function() {
  
  shiny::tagList(
    
    shiny::h3("Find Similar Shoes"),
    
    # Photo source
    shiny::radioButtons(
      "src",
      "Photo source",
      choices = c("Catalogue", "Self-check", "Upload"),
      selected = "Catalogue",
      inline = TRUE
    ),
    
    # Catalogue photo
    shiny::conditionalPanel(
      "input.src == 'Catalogue'",
      
      shiny::numericInput(
        "cat_id",
        "Catalogue ID",
        value = 1,
        min = 1,
        max = 2000,
        step = 1
      )
    ),
    
    # Self-check photo
    shiny::conditionalPanel(
      "input.src == 'Self-check'",
      
      shiny::selectInput(
        "sc_file",
        "Self-check photo",
        choices = sprintf("selfcheck_%02d.png", 1:4)
      )
    ),
    
    # Uploaded photo
    shiny::conditionalPanel(
      "input.src == 'Upload'",
      
      shiny::fileInput(
        "photo",
        "Upload a shoe photo",
        accept = c(".jpg", ".jpeg", ".png")
      )
    ),
    
    # Number of principal components
    shiny::sliderInput(
      "K",
      "Number of principal components (K)",
      min = 1,
      max = 150,
      value = 40,
      step = 1
    ),
    
    shiny::hr(),
    
    # Image comparison
    shiny::h4("Best Match and PCA Reconstruction"),
    
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::h5("Selected photo"),
        shiny::plotOutput("shoe_selected", height = "190px")
      ),
      
      shiny::column(
        3,
        shiny::h5("Closest catalogue shoe"),
        shiny::plotOutput("shoe_best", height = "190px")
      ),
      
      shiny::column(
        3,
        shiny::h5("Catalogue average"),
        shiny::plotOutput("shoe_average", height = "190px")
      ),
      
      shiny::column(
        3,
        shiny::h5("Rebuilt with K PCs"),
        shiny::plotOutput("shoe_rebuilt", height = "190px")
      )
    ),
    
    # Top 5 results
    shiny::h4("Top 5 Matches"),
    shiny::tableOutput("shoe_top5"),
    
    # Category prediction
    shiny::h4("Prediction"),
    shiny::verbatimTextOutput("shoe_prediction")
  )
}


# ------------------------------------------------------------
# 2. Shoe Search Server
# ------------------------------------------------------------

# Returns photo_info() for Explain PCA and other tabs.

shoe_search_server <- function(
    input,
    output,
    session,
    model,
    shoes_dir = "shoes",
    selfcheck_dir = "selfcheck"
) {
  
  # ----------------------------------------------------------
  # 2-1. Identify selected photo
  # ----------------------------------------------------------
  
  current_photo <- shiny::reactive({
    
    src <- input$src
    
    if (identical(src, "Catalogue")) {
      
      id <- as.integer(input$cat_id)
      
      shiny::validate(
        shiny::need(
          length(id) == 1L &&
            !is.na(id) &&
            id %in% model$meta$id,
          "Choose a valid catalogue ID."
        )
      )
      
      list(
        source = src,
        id = id,
        path = shoe_path(id, shoes_dir),
        true_category = model$meta$category[
          match(id, model$meta$id)
        ]
      )
      
    } else if (identical(src, "Self-check")) {
      
      file <- input$sc_file
      shiny::req(file)
      
      labels <- load_selfcheck_labels(selfcheck_dir)
      
      shiny::validate(
        shiny::need(
          file %in% labels$photo,
          "Self-check label not found."
        )
      )
      
      list(
        source = src,
        id = NULL,
        path = file.path(selfcheck_dir, file),
        true_category = labels$category[
          match(file, labels$photo)
        ]
      )
      
    } else {
      
      shiny::req(input$photo)
      
      extension <- tolower(
        tools::file_ext(input$photo$name)
      )
      
      shiny::validate(
        shiny::need(
          length(extension) == 1L &&
            extension %in% c("jpg", "jpeg", "png"),
          "Upload JPG or PNG."
        )
      )
      
      list(
        source = "Upload",
        id = NULL,
        path = input$photo$datapath,
        true_category = NA_character_
      )
    }
  })
  
  
  # ----------------------------------------------------------
  # 2-2. Read selected image once per photo change
  # ----------------------------------------------------------
  
  photo_pixels <- shiny::reactive({
    
    path <- current_photo()$path
    
    shiny::validate(
      shiny::need(
        file.exists(path),
        "Image file not found. Please check the file path."
      )
    )
    
    read_shoe_image(path)
  })
  
  
  # ----------------------------------------------------------
  # 2-3. Get or calculate PC scores
  # ----------------------------------------------------------
  
  # Catalogue:
  # Use the scores already saved in pca_model.rds.
  #
  # Self-check / Upload:
  # Project the new photo onto all 150 stored PCs.
  #
  # This reactive does not depend on input$K.
  
  photo_scores <- shiny::reactive({
    
    info <- current_photo()
    
    if (identical(info$source, "Catalogue")) {
      
      # Locate the selected catalogue shoe
      idx <- match(info$id, model$meta$id)
      
      # Reuse its precomputed PC scores
      scores <- as.numeric(
        model$pca$x[idx, , drop = TRUE]
      )
      
      names(scores) <- colnames(model$pca$x)
      
      scores
      
    } else {
      
      # New photo: project onto all available PCs
      project_photo(
        img_vec = photo_pixels(),
        model = model,
        K = ncol(model$pca$rotation)
      )
    }
  })
  
  
  # ----------------------------------------------------------
  # 2-4. Search and predict using selected K
  # ----------------------------------------------------------
  
  photo_info <- shiny::reactive({
    
    K <- as.integer(input$K)
    
    shiny::validate(
      shiny::need(
        length(K) == 1L &&
          !is.na(K) &&
          K >= 1L &&
          K <= ncol(model$pca$rotation),
        "Choose a valid K."
      )
    )
    
    # Use the first K of the available PC scores
    scores <- photo_scores()[seq_len(K)]
    
    # Find Top 5 using Cosine distance
    top5 <- find_matches(
      scores = scores,
      model = model,
      K = K,
      exclude_id = current_photo()$id,
      n = 5L
    )
    
    predicted <- predict_cat(top5)
    
    # Shared information for Explain PCA
    list(
      img_vec = photo_pixels(),
      scores = scores,
      K = K,
      source = current_photo()$source,
      id = current_photo()$id,
      true_category = current_photo()$true_category,
      top5 = top5,
      prediction = predicted
    )
  })
  
  
  # ----------------------------------------------------------
  # 3. Display images
  # ----------------------------------------------------------
  
  # Selected photo
  output$shoe_selected <- shiny::renderPlot({
    
    plot_shoe_pixels(photo_pixels())
  })
  
  # Best match
  output$shoe_best <- shiny::renderPlot({
    
    info <- photo_info()
    shiny::req(nrow(info$top5) > 0L)
    
    closest_id <- info$top5$id[1]
    
    plot_shoe_pixels(
      read_shoe_image(
        shoe_path(closest_id, shoes_dir)
      )
    )
  })
  
  # Average shoe
  output$shoe_average <- shiny::renderPlot({
    shiny::req(photo_pixels())
    plot_shoe_pixels(model$pca$center)
  })
  
  # Reconstructed photo
  output$shoe_rebuilt <- shiny::renderPlot({
    
    info <- photo_info()
    
    rebuilt <- rebuild_photo(
      scores = info$scores,
      model = model,
      K = info$K
    )
    
    plot_shoe_pixels(rebuilt)
  })
  
  
  # ----------------------------------------------------------
  # 4. Display Top 5 results
  # ----------------------------------------------------------
  
  output$shoe_top5 <- shiny::renderTable({
    
    result <- photo_info()$top5
    
    result$id <- sprintf("%04d", result$id)
    result$distance <- round(result$distance, 3)
    
    result
    
  }, rownames = FALSE)
  
  
  # ----------------------------------------------------------
  # 5. Display category prediction
  # ----------------------------------------------------------
  
  output$shoe_prediction <- shiny::renderText({
    
    info <- photo_info()
    actual <- info$true_category
    
    if (is.na(actual) || !nzchar(actual)) {
      
      sprintf(
        "Predicted: %s | Correct? Unknown (uploaded image has no label)",
        info$prediction
      )
      
    } else {
      
      sprintf(
        "Predicted: %s | Actual: %s | %s",
        info$prediction,
        actual,
        if (identical(info$prediction, actual)) {
          "Correct"
        } else {
          "Incorrect"
        }
      )
    }
  })
  
  
  # Return reactive information to the main app
  photo_info
}
