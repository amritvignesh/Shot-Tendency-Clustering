library(nbastatR)
library(dplyr)
library(factoextra)
library(tidyverse)
library(hoopR)
library(gt)
library(gtExtras)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 10)

shots <- data.frame() 
for (szn in 2015:2024) {
  shots_szn <- teams_shots(all_active_teams = TRUE, season_types = "Regular Season", seasons = szn)
  shot_data <- shots_szn %>%
    group_by(idPlayer, zoneBasic) %>%
    summarize(att = n(), team = paste(unique(nameTeam), collapse = ", ")) %>%
    pivot_wider(names_from = zoneBasic, values_from = att, values_fill = 0) %>%
    ungroup() %>%
    mutate(att = rowSums(across(-1))) %>%
    mutate(across(.cols = 2:8, ~./att * 100), season = szn) %>%
    arrange(-att) %>%   
    head(100) %>%
    select(-Backcourt)
  shots <- rbind(shots, shot_data)
}

scaled <- scale(shots[,c(2:7)])

set.seed(0)
kmeans <- kmeans(scaled, centers = 4, nstart = 25, iter.max = 20)
kmeans_centers <- as.data.frame(kmeans$centers)

kmeans_centers$cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4')

kmeans_centers <- kmeans_centers %>%
  pivot_longer(!cluster, names_to = 'statname', values_to = 'statvalue')

kmeans_centers %>%
  ggplot(aes(x=statname, y=statvalue, color=cluster)) +
  geom_point() +
  facet_grid(~ cluster, scales='free_y', switch='y', space='free_y') +
  labs(x = "Statistic Predictor", y = "Scaled Statistical Value Center Per Cluster",
       title = "Cluster Compositions for NBA Players (Top 100 In Shot Attempts Per Season)") +
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), 
        panel.grid.minor = element_blank(), plot.title = element_text(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"))

pca <- prcomp(scaled)
pca_summary <- summary(pca)

twopcas <- as.data.frame(pca$x[,1:2])
twopcas$cluster <- as.factor(kmeans$cluster)
variance_1 <- 100 *round(pca_summary$importance[2,1], 4) 
variance_2 <- 100 *round(pca_summary$importance[2,2], 4) 

twopcas %>%
  ggplot(aes(x=PC1, y=PC2, color= cluster)) + 
  geom_point(alpha=0.3) + 
  stat_ellipse(level=(2/3)) + 
  labs(x = paste0('PC1 (Accounts for ', variance_1, '% of Variance)'), 
       y = paste0('PC2 (Accounts for ', variance_2, '% of Variance)'), 
       title = 'K-Means Cluster Differences for NBA Players (Top 100 in Shot Attempts Per Season)') +
  theme(plot.title = element_text(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"))

name_cluster <- data.frame(id = shots$idPlayer, season = shots$season, cluster=kmeans$cluster)

players <- nba_players() %>%
  select(id = idPlayer, player = namePlayer)

name_cluster <- left_join(name_cluster, players, by = "id")

name_cluster <- name_cluster %>%
  mutate(adj = ifelse(cluster == 1, "Shooter But No Corners", ifelse(cluster == 2, "Paint Is Effective", ifelse(cluster == 3, "Corners Are Fine", "Mid-Range Is Underrated"))))

unique_players <- unique(name_cluster$id)

total_data <- expand.grid(id = unique_players, season = 2015:2024)

data <- left_join(total_data, name_cluster, by = c("season", "id"))

data$adj[which(is.na(data$adj))] <- "DNP/DNQ"

data <- left_join(data, players, by = "id") %>%
  mutate(player = player.y) %>%
  select(-player.x, -player.y)

team_data <- bref_players_stats(seasons = 2015:2024, tables = "totals") %>%
  select(season = yearSeason, id = idPlayerNBA, team = slugTeamsBREF)

data <- left_join(data, team_data, by = c("season", "id"))

data$team[which(is.na(data$cluster))] <- NA

data <- data %>%
  mutate(team = substring(team, nchar(team) - 2, nchar(team)))

data$team[which(is.na(data$team))] <- ""

logos <- espn_nba_teams() %>%
  select(team = abbreviation, logo)

logos$team[which(logos$team == "BKN")] <- "BRK"
logos$team[which(logos$team == "CHA")] <- "CHO"
logos$team[which(logos$team == "GS")] <- "GSW"
logos$team[which(logos$team == "NO")] <- "NOP"
logos$team[which(logos$team == "NY")] <- "NYK"
logos$team[which(logos$team == "PHX")] <- "PHO"
logos$team[which(logos$team == "SA")] <- "SAS"
logos$team[which(logos$team == "UTAH")] <- "UTA"
logos$team[which(logos$team == "WSH")] <- "WAS"

final_data <- left_join(data, logos, by = "team") 

unique_ids <- unique(data$id)

final_data$adj <- as.factor(final_data$adj)

subfolder_path <- "players/"
dir.create(subfolder_path, showWarnings = FALSE)

for (i_d in unique_ids) {
  indiv_data <- final_data %>%
    filter(id == i_d) 
  
  player <- indiv_data$player[1] 
  
  indiv_data <- indiv_data %>%
    select(-id, -cluster, -player, -team)
  
  table <- indiv_data %>% gt() %>%
    gt_img_rows(columns = logo) %>%
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = c(season, logo, adj)
    ) %>%
    data_color(
      columns = adj,
      method = "factor",
      palette = "viridis"
    ) %>%
    cols_label(
      season = md("**Season**"),
      logo = md("**Team**"),
      adj = md("**Shot Style**")
    ) %>%
    tab_header(
      title = md(player),
      subtitle = "Shot Style From 2015 to 2024 Using K-Means Clustering"
    ) 
  
  gtsave(table, file.path(subfolder_path, paste0(player, ".png")))
}

