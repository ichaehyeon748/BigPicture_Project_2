
# ============================================================
# Project 2 - Shared PCA Core
# Statistical Graphics with Big Data
#
# Image processing, PCA, similarity search,
# category prediction, and reconstruction.
#
# Distance: Cosine
# PCA components: 150 (default)
# Pixel values: 0-255
#
# Required packages (install once if missing):
# install.packages(c("magick", "shiny", "ggplot2"))
#
# magick: image processing
# shiny: Shiny application
# ggplot2: evaluation / validation graphs
# ============================================================


# ------------------------------------------------------------
# 1. Create catalogue image path
# ------------------------------------------------------------

shoe_path <- function(id, shoes_dir = "shoes") {
  
  file.path(
    shoes_dir,
    sprintf("%04d.png", as.integer(id))
  )
}


# ------------------------------------------------------------
# 2. Read image as a 4096-dimensional vector
# ------------------------------------------------------------

read_shoe_image <- function(path) {
  
  if (!file.exists(path)) {
    stop("Image file not found: ", path)
  }
  
  img <- magick::image_read(path)
  
  # Convert to grayscale and resize to 64 x 64
  img <- magick::image_convert(
    img,
    colorspace = "gray"
  )
  
  img <- magick::image_resize(
    img,
    "64x64!"
  )
  
  # Extract grayscale pixels (0-255)
  pixels <- magick::image_data(
    img,
    channels = "gray"
  )
  
  # Convert image to row-major vector
  vec <- as.numeric(
    t(
      matrix(
        as.integer(pixels),
        nrow = 64,
        ncol = 64
      )
    )
  )
  
  if (length(vec) != 4096L) {
    stop("Image must contain 4096 pixels.")
  }
  
  vec
}


# ------------------------------------------------------------
# 3. Load catalogue metadata
# ------------------------------------------------------------

load_shoe_metadata <- function(
    path = "shoes_meta.csv"
) {
  
  if (!file.exists(path)) {
    stop("Metadata file not found: ", path)
  }
  
  meta <- utils::read.csv(
    path,
    stringsAsFactors = FALSE
  )
  
  required_columns <- c("id", "category")
  
  if (!all(required_columns %in% names(meta))) {
    stop("Metadata must contain id and category.")
  }
  
  meta$id <- as.integer(meta$id)
  meta$category <- as.character(meta$category)
  
  if (anyNA(meta$id) || anyDuplicated(meta$id)) {
    stop("Metadata contains invalid or duplicate IDs.")
  }
  
  if (anyNA(meta$category) || any(!nzchar(meta$category))) {
    stop("Metadata contains invalid categories.")
  }
  
  meta <- meta[order(meta$id), , drop = FALSE]
  rownames(meta) <- NULL
  
  meta
}


# ------------------------------------------------------------
# 4. Load self-check labels
# ------------------------------------------------------------

load_selfcheck_labels <- function(
    dir = "selfcheck"
) {
  
  label_path <- file.path(dir, "labels.csv")
  
  if (!file.exists(label_path)) {
    label_path <- file.path(dir, "label.csv")
  }
  
  if (!file.exists(label_path)) {
    stop("Self-check label file not found.")
  }
  
  labels <- utils::read.csv(
    label_path,
    stringsAsFactors = FALSE
  )
  
  if (!all(c("photo", "category") %in% names(labels))) {
    stop("Self-check labels must contain photo and category.")
  }
  
  labels$photo <- as.character(labels$photo)
  labels$category <- as.character(labels$category)
  
  labels
}


# ------------------------------------------------------------
# 5. Load all catalogue images
# ------------------------------------------------------------

load_all_shoes <- function(
    meta,
    shoes_dir = "shoes"
) {
  
  n <- nrow(meta)
  
  X <- matrix(
    0,
    nrow = n,
    ncol = 4096L
  )
  
  for (i in seq_len(n)) {
    
    path <- shoe_path(
      meta$id[i],
      shoes_dir
    )
    
    X[i, ] <- read_shoe_image(path)
  }
  
  rownames(X) <- sprintf(
    "%04d",
    meta$id
  )
  
  X
}


# ------------------------------------------------------------
# 6. Build PCA model
# ------------------------------------------------------------

build_pca_model <- function(
    meta_path = "shoes_meta.csv",
    shoes_dir = "shoes",
    n_pc = 150L
) {
  
  meta <- load_shoe_metadata(meta_path)
  
  # Catalogue image matrix: 2000 x 4096
  X <- load_all_shoes(
    meta,
    shoes_dir
  )
  
  n_pc <- as.integer(n_pc)
  
  if (length(n_pc) != 1L ||
      is.na(n_pc) ||
      n_pc < 1L ||
      n_pc > min(nrow(X) - 1L, ncol(X))) {
    stop("Invalid number of principal components.")
  }
  
  # Total variance across all original pixel variables
  # Used as the denominator of explained variance ratio
  N <- nrow(X)
  
  total_variance <- (
    sum(X * X) -
      N * sum(colMeans(X)^2)
  ) / (N - 1L)
  
  if (!is.finite(total_variance) ||
      total_variance <= 0) {
    stop("Invalid total variance.")
  }
  
  # Fit PCA without standardizing pixel variables
  pca <- stats::prcomp(
    X,
    center = TRUE,
    scale. = FALSE,
    rank. = n_pc
  )
  
  # PCA model and related information
  model <- list(
    pca = pca,
    meta = meta,
    n_pc = n_pc,
    pixel_size = c(64L, 64L),
    pixel_range = c(0, 255),
    total_variance = total_variance
  )
  
  model
}


# ------------------------------------------------------------
# 7. Load saved PCA model
# ------------------------------------------------------------

load_pca_model <- function(
    path = "pca_model.rds"
) {
  
  if (!file.exists(path)) {
    stop("PCA model file not found: ", path)
  }
  
  model <- readRDS(path)
  
  required <- c(
    "pca",
    "meta",
    "n_pc",
    "pixel_size",
    "pixel_range",
    "total_variance"
  )
  
  if (!all(required %in% names(model))) {
    stop("Invalid PCA model structure.")
  }
  
  model
}


# ------------------------------------------------------------
# 8. Project photo onto PCA space
# ------------------------------------------------------------

project_photo <- function(
    img_vec,
    model,
    K
) {
  
  K <- as.integer(K)
  
  if (length(K) != 1L ||
      is.na(K) ||
      K < 1L ||
      K > ncol(model$pca$rotation)) {
    stop("Invalid K")
  }
  
  if (length(img_vec) != length(model$pca$center)) {
    stop("Invalid image vector length.")
  }
  
  # Center image using catalogue mean
  centered <- as.numeric(img_vec) - model$pca$center
  
  # Project onto first K PCs
  scores <- as.numeric(
    centered %*%
      model$pca$rotation[
        , seq_len(K), drop = FALSE
      ]
  )
  
  scores
}


# ------------------------------------------------------------
# 9. Find nearest catalogue shoes
# ------------------------------------------------------------

find_matches <- function(
    scores,
    model,
    K = length(scores),
    exclude_id = NULL,
    n = 5L
) {
  
  K <- as.integer(K)
  n <- as.integer(n)
  
  if (length(K) != 1L ||
      is.na(K) ||
      K < 1L ||
      K > ncol(model$pca$x)) {
    stop("Invalid K")
  }
  
  if (length(n) != 1L ||
      is.na(n) ||
      n < 1L) {
    stop("Invalid number of matches")
  }
  
  if (length(scores) < K) {
    stop("Not enough PC scores")
  }
  
  # Catalogue scores in K-dimensional PCA space
  catalogue_scores <- model$pca$x[
    , seq_len(K), drop = FALSE
  ]
  
  # Selected photo PC scores
  query_scores <- as.numeric(scores)[seq_len(K)]
  
  # Cosine similarity:
  # similarity = dot(a, b) / (norm(a) * norm(b))
  
  dot_products <- as.numeric(
    catalogue_scores %*% query_scores
  )
  
  catalogue_norms <- sqrt(
    rowSums(catalogue_scores^2)
  )
  
  query_norm <- sqrt(
    sum(query_scores^2)
  )
  
  denominator <- catalogue_norms * query_norm
  
  # Avoid division by zero
  cosine_similarity <- dot_products /
    pmax(denominator, .Machine$double.eps)
  
  # Cosine distance = 1 - cosine similarity
  distance <- 1 - cosine_similarity
  
  # Keep distances within the theoretical range [0, 2]
  distance <- pmax(
    pmin(distance, 2),
    0
  )
  
  # Exclude the selected catalogue photo itself
  if (!is.null(exclude_id)) {
    
    distance[
      model$meta$id %in% as.integer(exclude_id)
    ] <- Inf
  }
  
  # Sort by distance, then by ID for ties
  ordered <- order(
    distance,
    model$meta$id
  )
  
  ordered <- ordered[
    is.finite(distance[ordered])
  ]
  
  # Return nearest N photos (default: 5)
  nearest <- head(ordered, n)
  
  data.frame(
    id = model$meta$id[nearest],
    category = model$meta$category[nearest],
    distance = distance[nearest],
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# 10. Predict category using Top 5 majority vote
# ------------------------------------------------------------

predict_cat <- function(top5) {
  
  if (nrow(top5) == 0L) {
    return(NA_character_)
  }
  
  categories <- as.character(top5$category)
  
  counts <- table(categories)
  max_count <- max(counts)
  
  # Categories tied for the highest frequency
  winners <- names(counts)[
    counts == max_count
  ]
  
  # If tied, choose the category of the nearest shoe
  for (category in categories) {
    
    if (category %in% winners) {
      return(category)
    }
  }
  
  NA_character_
}


# ------------------------------------------------------------
# 11. Reconstruct photo using first K PCs
# ------------------------------------------------------------

rebuild_photo <- function(
    scores,
    model,
    K = length(scores)
) {
  
  K <- as.integer(K)
  
  if (length(K) != 1L ||
      is.na(K) ||
      K < 1L ||
      K > ncol(model$pca$rotation)) {
    stop("Invalid K")
  }
  
  if (length(scores) < K) {
    stop("Not enough PC scores")
  }
  
  # Reconstruct the centered image
  rebuilt_centered <- as.numeric(
    as.numeric(scores)[seq_len(K)] %*%
      t(
        model$pca$rotation[
          , seq_len(K), drop = FALSE
        ]
      )
  )
  
  # Add the mean image back
  rebuilt <- rebuilt_centered + model$pca$center
  
  rebuilt
}


# ------------------------------------------------------------
# 12. Display shoe image from pixel vector
# ------------------------------------------------------------

plot_shoe_pixels <- function(pixels) {
  
  if (length(pixels) != 4096L) {
    stop("Image vector must contain 4096 values.")
  }
  
  # Clip pixel values to 0-255
  pixels <- pmax(
    0,
    pmin(255, as.numeric(pixels))
  )
  
  # Normalize to 0-1 for display only
  pixels <- pixels / 255
  
  # Convert vector to 64 x 64 image
  img_matrix <- matrix(
    pixels,
    nrow = 64,
    ncol = 64,
    byrow = TRUE
  )
  
  # Convert grayscale values to image colors
  image_colors <- matrix(
    grDevices::gray(as.vector(img_matrix)),
    nrow = 64,
    ncol = 64
  )
  
  # Display image
  grid::grid.newpage()
  
  grid::grid.raster(
    image_colors,
    interpolate = FALSE
  )
}
