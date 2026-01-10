library(dplyr)
library(ggplot2)

# ------------------------------
# Load original dataset
# ------------------------------
df_orig <- read.csv("online_shoppers_intention.csv")

# Create Season variable
df_orig$Season <- dplyr::case_when(
  df_orig$Month %in% c("Nov", "Dec", "Jan", "Feb") ~ "Winter",
  df_orig$Month %in% c("May", "Jun", "Jul", "Aug") ~ "Summer",
  TRUE ~ "Other"
)

# Filter Summer/Winter only & convert Revenue to numeric
df_season <- df_orig %>%
  filter(Season %in% c("Summer", "Winter")) %>%
  mutate(Revenue_num = as.numeric(Revenue))

# Errorbar helper
mean_se <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x)
  se <- sd(x) / sqrt(length(x))
  data.frame(
    y = m,
    ymin = m - 1.96 * se,
    ymax = m + 1.96 * se
  )
}

# ------------------------------
# Plot: Purchase Rate by Season
# ------------------------------
p_season_bar <- ggplot(df_season, aes(x = Season, y = Revenue_num, fill = Season)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2) +
  labs(title = "Purchase Rate by Season", y = "Mean Purchase Rate") +
  theme_minimal()

print(p_season_bar)
ggsave("plot_season_purchase_rate.png", p_season_bar, width = 6, height = 4, dpi = 300)

# ------------------------------
# Plot: Season × VisitorType
# ------------------------------
p_season_visit <- ggplot(df_season,
                         aes(x = VisitorType, y = Revenue_num, fill = Season)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.8)) +
  labs(title = "Season × VisitorType: Purchase Rate", y = "Purchase Rate") +
  theme_minimal()

print(p_season_visit)
ggsave("plot_season_visitor_purchase.png", p_season_visit, width = 7, height = 4, dpi = 300)

# ------------------------------
# Logistic regression by season
# ------------------------------
model_summer <- glm(Revenue_num ~ ProductRelated + ProductRelated_Duration + BounceRates,
                    data = df_season[df_season$Season == "Summer",],
                    family = "binomial")

model_winter <- glm(Revenue_num ~ ProductRelated + ProductRelated_Duration + BounceRates,
                    data = df_season[df_season$Season == "Winter",],
                    family = "binomial")

summary(model_summer)
summary(model_winter)

# ------------------------------
# Clustering (preprocessed dataset)
# ------------------------------
df_pre <- read.csv("online_shoppers_preprocessed.csv")

features <- df_pre %>%
  dplyr::select(
    num__ProductRelated,
    num__ProductRelated_Duration,
    num__BounceRates,
    num__ExitRates,
    num__PageValues,
    num__SpecialDay
  )

features_scaled <- scale(features)

set.seed(123)
wss <- sapply(2:6, function(k) {
  kmeans(features_scaled, centers = k, nstart = 20)$tot.withinss
})

plot(2:6, wss, type = "b",
     xlab = "Number of Clusters (k)",
     ylab = "Within-cluster SS",
     main = "Elbow Method for K-means")

set.seed(123)
km4 <- kmeans(features_scaled, centers = 4, nstart = 20)
df_pre$cluster <- factor(km4$cluster)

p_cluster <- ggplot(df_pre,
                    aes(x = num__ProductRelated,
                        y = num__PageValues,
                        color = cluster)) +
  geom_point(alpha = 0.5, size = 1) +
  labs(title = "Customer Clusters by Shopping Behavior",
       x = "ProductRelated (scaled)",
       y = "PageValues (scaled)") +
  theme_minimal()

print(p_cluster)

ggsave("plot_clusters_pagevalues_productrelated.png",
       p_cluster, width = 6, height = 4, dpi = 300)

cluster_summary <- df_pre %>%
  group_by(cluster) %>%
  summarise(
    purchase_rate = mean(Revenue),
    n = n()
  )

print(cluster_summary)
write.csv(cluster_summary, "cluster_summary.csv", row.names = FALSE)

