library(plumber)
library(dplyr)

# Dataset load karo (same folder mein hona chahiye)
customers <- read.csv("store_customers.csv", check.names = FALSE, na.strings = c("", "NA"))
customers <- na.omit(customers)

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* Customer Segmentation API
#* @param algorithm K-Means or Hierarchical
#* @param k Number of clusters
#* @serializer unboxedJSON
#* @get /cluster
function(algorithm = "K-Means", k = 5) {
  k <- as.numeric(k)
  data <- customers

  # Features select & scale karo
  features <- data %>%
    select(`Annual Income (k$)`, `Spending Score (1-100)`)
  scaled_features <- scale(features)

  # Clustering run karo
  if (algorithm == "K-Means") {
    set.seed(123)
    model <- kmeans(scaled_features, centers = k, nstart = 25)
    data$Cluster <- as.numeric(model$cluster)
  } else {
    dist_mat <- dist(scaled_features, method = "euclidean")
    model <- hclust(dist_mat, method = "ward.D2")
    data$Cluster <- as.numeric(cutree(model, k = k))
  }

  # Cluster Profiles calculate karo
  profiles <- data %>%
    group_by(Cluster) %>%
    summarise(
      count = n(),
      avg_age = round(mean(Age), 1),
      avg_income = round(mean(`Annual Income (k$)`), 1),
      avg_spending = round(mean(`Spending Score (1-100)`), 1),
      .groups = "drop"
    )

  # Overall Stats
  stats <- list(
    total_customers = nrow(data),
    total_clusters = k,
    avg_income = round(mean(data$`Annual Income (k$)`), 1),
    avg_spending = round(mean(data$`Spending Score (1-100)`), 1)
  )

  # Clean customer list bhejo
  records <- data %>%
    select(
      id = CustomerID,
      age = Age,
      income = `Annual Income (k$)`,
      score = `Spending Score (1-100)`,
      cluster = Cluster
    )

  list(
    stats = stats,
    profiles = profiles,
    customers = records
  )
}

#* Serve HTML Dashboard
#* @get /
#* @serializer html
function() {
  paste(readLines("customer.html", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}