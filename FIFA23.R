# Pakete laden 
library(dplyr)
library(dbplyr)
library(DBI)
library(ggplot2)
library(readr)
library(corrplot)
library(DBI)
library(tidymodels)
tidymodels_prefer()

library(modeldata)
library(themis)
library(doMC)

# Verbindung zur Datenbank herstellen und Tabellen extrahieren 
con <- dbConnect(odbc::odbc(), "fifa23", timeout = 10)
cleaned_players <- dbGetQuery(con, "SELECT * FROM cleaned_players")
players <- dbGetQuery(con, "SELECT * FROM players")
clubs <- dbGetQuery(con, "SELECT * FROM clubs")
coaches <- dbGetQuery(con, "SELECT * FROM coaches")
leagues <- dbGetQuery(con, "SELECT * FROM leagues")
nation_teams <- dbGetQuery(con, "SELECT * FROM nation_teams")
player_positions <- dbGetQuery(con, "SELECT * FROM player_positions")
player_tags <- dbGetQuery(con, "SELECT * FROM player_tags")
player_traits <- dbGetQuery(con, "SELECT * FROM player_traits")

# 1.Verschaffen Sie sich einen Überblick und ein Verständnis der vorliegenden Daten durch deskriptive Analysen und grafische Darstellungen.

# Funktion zum Erstellen von deskriptiven Statistiken
summary_stats <- function(df) {
  data.frame(
    Feature = colnames(df),
    Mean = sapply(df, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
    Median = sapply(df, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA),
    SD = sapply(df, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA),
    Min = sapply(df, function(x) if(is.numeric(x)) min(x, na.rm = TRUE) else NA),
    Max = sapply(df, function(x) if(is.numeric(x)) max(x, na.rm = TRUE) else NA),
    NA_Count = sapply(df, function(x) sum(is.na(x)))
  )
}

players_summary <- summary_stats(players)
clubs_summary <- summary_stats(clubs)
coaches_summary <- summary_stats(coaches)
leagues_summary <- summary_stats(leagues)
nation_teams_summary <- summary_stats(nation_teams)
player_positions_summary <- summary_stats(player_positions)
player_traits_summary <- summary_stats(player_traits)
player_tags_summary <- summary_stats(player_tags)

# Anzeigen der deskriptiven Statistiken für players
print(players_summary)

# Korrelation der wichtigsten Merkmale mit dem Spielergehalt
corr_matrix <- cor(players %>% select(wage_eur, overall, potential, value_eur, age), use = "complete.obs")
corrplot(corr_matrix, method = "circle")


# Histogramm der Spielergehälter
ggplot(players, aes(x = wage_eur)) +
  geom_histogram(binwidth = 10000, fill = 'blue', color = 'black', alpha = 0.7) +
  ggtitle('Verteilung der Spielergehälter') +
  xlab('Gehalt in Euro') +
  ylab('Häufigkeit')

# Verteilung der Marktwerte
ggplot(players, aes(x = value_eur)) +
  geom_histogram(binwidth = 1000000, fill = 'green', color = 'black', alpha = 0.7) +
  ggtitle('Verteilung der Marktwerte') +
  xlab('Marktwert in Euro') +
  ylab('Häufigkeit')

# Boxplot der Gehälter nach Positionen
ggplot(players, aes(x = club_position, y = wage_eur)) +
  geom_boxplot(fill = 'orange') +
  ggtitle('Gehälter nach Positionen') +
  xlab('Position') +
  ylab('Gehalt in Euro')

# 2. Säubern Sie die Daten falls notwendig und leiten Sie wenn möglich neue “schlaue” Variablen her, die für die Vorhersagen genutzt werden können. Hierzu können Sie sämtliche Tabellen der Datenbank nutzen.

# Behandlung fehlender Werte in der players Tabelle
players$wage_eur[is.na(players$wage_eur)] <- median(players$wage_eur, na.rm = TRUE)
players$value_eur[is.na(players$value_eur)] <- median(players$value_eur, na.rm = TRUE)
players$pace[is.na(players$pace)] <- 0
players$shooting[is.na(players$shooting)] <- 0
players$passing[is.na(players$passing)] <- 0
players$dribbling[is.na(players$dribbling)] <- 0
players$defending[is.na(players$defending)] <- 0
players$physic[is.na(players$physic)] <- 0
players$attacking_crossing[is.na(players$attacking_crossing)] <- 0
players$attacking_finishing[is.na(players$attacking_finishing)] <- 0
players$attacking_heading_accuracy[is.na(players$attacking_heading_accuracy)] <- 0
players$attacking_short_passing[is.na(players$attacking_short_passing)] <- 0
players$attacking_volleys[is.na(players$attacking_volleys)] <- 0
players$skill_dribbling[is.na(players$skill_dribbling)] <- 0
players$skill_curve[is.na(players$skill_curve)] <- 0
players$skill_fk_accuracy[is.na(players$skill_fk_accuracy)] <- 0
players$skill_long_passing[is.na(players$skill_long_passing)] <- 0
players$skill_ball_control[is.na(players$skill_ball_control)] <- 0
players$movement_acceleration[is.na(players$movement_acceleration)] <- 0
players$movement_sprint_speed[is.na(players$movement_sprint_speed)] <- 0
players$movement_agility[is.na(players$movement_agility)] <- 0
players$movement_reactions[is.na(players$movement_reactions)] <- 0
players$movement_balance[is.na(players$movement_balance)] <- 0
players$power_shot_power[is.na(players$power_shot_power)] <- 0
players$power_jumping[is.na(players$power_jumping)] <- 0
players$power_stamina[is.na(players$power_stamina)] <- 0
players$power_strength[is.na(players$power_strength)] <- 0
players$power_long_shots[is.na(players$power_long_shots)] <- 0
players$mentality_aggression[is.na(players$mentality_aggression)] <- 0
players$mentality_interceptions[is.na(players$mentality_interceptions)] <- 0
players$mentality_positioning[is.na(players$mentality_positioning)] <- 0
players$mentality_vision[is.na(players$mentality_vision)] <- 0
players$mentality_penalties[is.na(players$mentality_penalties)] <- 0
players$mentality_composure[is.na(players$mentality_composure)] <- 0
players$defending_marking_awareness[is.na(players$defending_marking_awareness)] <- 0
players$defending_standing_tackle[is.na(players$defending_standing_tackle)] <- 0
players$defending_sliding_tackle[is.na(players$defending_sliding_tackle)] <- 0
players$goalkeeping_diving[is.na(players$goalkeeping_diving)] <- 0
players$goalkeeping_handling[is.na(players$goalkeeping_handling)] <- 0
players$goalkeeping_kicking[is.na(players$goalkeeping_kicking)] <- 0
players$goalkeeping_positioning[is.na(players$goalkeeping_positioning)] <- 0
players$goalkeeping_reflexes[is.na(players$goalkeeping_reflexes)] <- 0
players$goalkeeping_speed[is.na(players$goalkeeping_speed)] <- 0
players$release_clause_eur[is.na(players$release_clause_eur)] <- 0

# Zusammenführen der Daten
players <- merge(players, player_positions, by = "player_id")

# Berechnen der neuen Spalten mit Dummy-Variablen 
cleaned_players <- players %>%
  mutate(
    overall_potential_diff = potential - overall,
    normalized_age = (age - min(age)) / (max(age) - min(age)),
    position_CAM = ifelse(grepl("CAM", player_positions), 1, 0),
    position_CB = ifelse(grepl("CB", player_positions), 1, 0),
    position_CDM = ifelse(grepl("CDM", player_positions), 1, 0),
    position_CF = ifelse(grepl("CF", player_positions), 1, 0),
    position_CM = ifelse(grepl("CM", player_positions), 1, 0),
    position_GK = ifelse(grepl("GK", player_positions), 1, 0),
    position_LB = ifelse(grepl("LB", player_positions), 1, 0),
    position_LM = ifelse(grepl("LM", player_positions), 1, 0),
    position_LW = ifelse(grepl("LW", player_positions), 1, 0),
    position_LWB = ifelse(grepl("LWB", player_positions), 1, 0),
    position_RB = ifelse(grepl("RB", player_positions), 1, 0),
    position_RM = ifelse(grepl("RM", player_positions), 1, 0),
    position_RW = ifelse(grepl("RW", player_positions), 1, 0),
    position_RWB = ifelse(grepl("RWB", player_positions), 1, 0),
    position_ST = ifelse(grepl("ST", player_positions), 1, 0)
  )

# Gruppieren nach Spieler-ID um diese eindeutig festzulegen und Duplikate zu löschen 
cleaned_players <- cleaned_players %>%
  group_by(player_id) %>%
  summarise(
    short_name = first(short_name),
    long_name = first(long_name),
    overall = first(overall),
    potential = first(potential),
    value_eur = first(value_eur),
    wage_eur = first(wage_eur),
    age = first(age),
    dob = first(dob),
    height_cm = first(height_cm),
    weight_kg = first(weight_kg),
    league_id = first(league_id),
    club_team_id = first(club_team_id),
    club_position = first(club_position),
    club_jersey_number = first(club_jersey_number),
    club_loaned_from = first(club_loaned_from),
    club_joined_date = first(club_joined_date),
    club_contract_valid_until_year = first(club_contract_valid_until_year),
    nationality = first(nationality),
    nation_team_id = first(nation_team_id),
    nation_position = first(nation_position),
    nation_jersey_number = first(nation_jersey_number),
    preferred_foot = first(preferred_foot),
    weak_foot = first(weak_foot),
    skill_moves = first(skill_moves),
    international_reputation = first(international_reputation),
    work_rate = first(work_rate),
    body_type = first(body_type),
    real_face = first(real_face),
    release_clause_eur = first(release_clause_eur),
    pace = first(pace),
    shooting = first(shooting),
    passing = first(passing),
    dribbling = first(dribbling),
    defending = first(defending),
    physic = first(physic),
    attacking_crossing = first(attacking_crossing),
    attacking_finishing = first(attacking_finishing),
    attacking_heading_accuracy = first(attacking_heading_accuracy),
    attacking_short_passing = first(attacking_short_passing),
    attacking_volleys = first(attacking_volleys),
    skill_dribbling = first(skill_dribbling),
    skill_curve = first(skill_curve),
    skill_fk_accuracy = first(skill_fk_accuracy),
    skill_long_passing = first(skill_long_passing),
    skill_ball_control = first(skill_ball_control),
    movement_acceleration = first(movement_acceleration),
    movement_sprint_speed = first(movement_sprint_speed),
    movement_agility = first(movement_agility),
    movement_reactions = first(movement_reactions),
    movement_balance = first(movement_balance),
    power_shot_power = first(power_shot_power),
    power_jumping = first(power_jumping),
    power_stamina = first(power_stamina),
    power_strength = first(power_strength),
    power_long_shots = first(power_long_shots),
    mentality_aggression = first(mentality_aggression),
    mentality_interceptions = first(mentality_interceptions),
    mentality_positioning = first(mentality_positioning),
    mentality_vision = first(mentality_vision),
    mentality_penalties = first(mentality_penalties),
    mentality_composure = first(mentality_composure),
    defending_marking_awareness = first(defending_marking_awareness),
    defending_standing_tackle = first(defending_standing_tackle),
    defending_sliding_tackle = first(defending_sliding_tackle),
    goalkeeping_diving = first(goalkeeping_diving),
    goalkeeping_handling = first(goalkeeping_handling),
    goalkeeping_kicking = first(goalkeeping_kicking),
    goalkeeping_positioning = first(goalkeeping_positioning),
    goalkeeping_reflexes = first(goalkeeping_reflexes),
    goalkeeping_speed = first(goalkeeping_speed),
    overall_potential_diff = first(overall_potential_diff),
    normalized_age = first(normalized_age),
    position_CAM = max(position_CAM),
    position_CB = max(position_CB),
    position_CDM = max(position_CDM),
    position_CF = max(position_CF),
    position_CM = max(position_CM),
    position_GK = max(position_GK),
    position_LB = max(position_LB),
    position_LM = max(position_LM),
    position_LW = max(position_LW),
    position_LWB = max(position_LWB),
    position_RB = max(position_RB),
    position_RM = max(position_RM),
    position_RW = max(position_RW),
    position_RWB = max(position_RWB),
    position_ST = max(position_ST)
  )
# Neue Variablen mit Hilfe von Dummy-Variablen herstellen 
cleaned_players <- cleaned_players %>% 
  mutate(
    preferred_foot_right = ifelse(preferred_foot == "Right", 1, 0),
    preferred_foot_left = ifelse(preferred_foot == "Left", 1, 0)
  )
# Player Tags zählen und den Spielern zuordnen
player_tag_counts <- player_tags %>%
  group_by(player_id) %>%
  summarise(tag_count = n())

cleaned_players <- cleaned_players %>%
  left_join(player_tag_counts, by = "player_id")

# Wenn kein Tag vorhanden dann 0
cleaned_players$tag_count[is.na(cleaned_players$tag_count)] <- 0

# Das selbe mit den Traits
player_trait_counts <- player_traits %>%
  group_by(player_id) %>%
  summarise(trait_count = n())

cleaned_players <- cleaned_players %>%
  left_join(player_trait_counts, by = "player_id")

cleaned_players$trait_count[is.na(cleaned_players$trait_count)] <- 0

# Prestige der jeweiligen Nationalität hinzufügen
nation_teams_prestige <- nation_teams %>% 
  select(nationality, international_prestige)

cleaned_players <- cleaned_players %>% 
  left_join(nation_teams_prestige, by = "nationality")  

# Das selbe mit den Vereinen 
club_team_prestige <- clubs %>% 
  select(club_id, domestic_prestige, international_prestige, club_worth_eur)

cleaned_players <- cleaned_players %>% 
  left_join(club_team_prestige, by = c("club_team_id" = "club_id")) 

# Entfernen nicht relevanter Spalten für die Gehaltsberechnung 
cleaned_players <- cleaned_players %>% 
  select(-dob, -club_joined_date, -club_loaned_from, -nation_team_id,
         -nation_position, -nation_jersey_number, -league_id, -club_team_id, 
         -club_jersey_number, -club_position, -club_contract_valid_until_year, -long_name,
         -preferred_foot, -work_rate, -body_type, -real_face, -short_name, -nationality) 

cleaned_players <- cleaned_players %>%
  # Duplikate entfernen, falls vorhanden
  distinct() %>%
  # Umgang mit fehlenden Werten
  mutate(
    age = replace_na(age, median(age, na.rm = TRUE)),
    height_cm = replace_na(height_cm, median(height_cm, na.rm = TRUE)),
    weight_kg = replace_na(weight_kg, median(weight_kg, na.rm = TRUE)),
    overall = replace_na(overall, median(overall, na.rm = TRUE)),
    potential = replace_na(potential, median(potential, na.rm = TRUE)),
    value_eur = replace_na(value_eur, median(value_eur, na.rm = TRUE)),
    wage_eur = replace_na(wage_eur, median(wage_eur, na.rm = TRUE))
  )
cleaned_players$international_prestige.x[is.na(cleaned_players$international_prestige.x)] <- 0
cleaned_players$domestic_prestige[is.na(cleaned_players$domestic_prestige)] <-0
cleaned_players$international_prestige.y[is.na(cleaned_players$international_prestige.y)] <- 0
cleaned_players$club_worth_eur[is.na(cleaned_players$club_worth_eur)] <- 0

# Zusammenfassen der Werte um Spaltenanzahl zu verringern
cleaned_players <- cleaned_players %>% 
  mutate(attacking = attacking_crossing + attacking_finishing + attacking_heading_accuracy +
           attacking_short_passing + attacking_volleys, 
         skill = skill_dribbling + skill_curve + skill_fk_accuracy + skill_long_passing + 
           skill_ball_control,
         movement = movement_acceleration + movement_sprint_speed + movement_agility + movement_reactions +
           movement_balance,
         power = power_shot_power + power_jumping + power_stamina + power_strength +
           power_long_shots,
         mentality = mentality_aggression + mentality_interceptions + mentality_positioning +
           mentality_vision + mentality_penalties + mentality_composure,
         defence = defending_marking_awareness + defending_standing_tackle + defending_sliding_tackle,
         goalkeeping = goalkeeping_diving + goalkeeping_handling + goalkeeping_kicking +
           goalkeeping_positioning + goalkeeping_reflexes + goalkeeping_speed) %>% 
  
  select(-attacking_crossing, -attacking_finishing, -attacking_heading_accuracy, -attacking_short_passing,
         -attacking_volleys, -skill_dribbling, -skill_curve, -skill_fk_accuracy, -skill_long_passing,
         - skill_ball_control, -movement_acceleration, -movement_sprint_speed, -movement_agility,
         -movement_reactions, -movement_balance, -power_shot_power, -power_jumping, -power_stamina,
         -power_strength, -power_long_shots, -mentality_aggression, -mentality_interceptions, 
         -mentality_positioning, -mentality_vision, -mentality_penalties, -mentality_composure, 
         -defending_marking_awareness, -defending_standing_tackle, -defending_sliding_tackle, 
         -goalkeeping_diving, -goalkeeping_handling, -goalkeeping_kicking, -goalkeeping_positioning,
         -goalkeeping_reflexes, -goalkeeping_speed)

# 1. Ergebnis: bereinigte Tabelle 
cleaned_players

# 3. Nutzen Sie die Ihnen bekannten geeigneten Machine Learning Algorithmen zur Regression, um ein Regressionsmodell zu erstellen. Tunen Sie gegebenenfalls die Hyperparameter des Modells.

#Registrierung für Parallelisierung schnellere Bearbeitung 

registerDoMC(cores = 8)

# Datenaufteilung
set.seed(123)
data_split <- initial_split(cleaned_players, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Rezept für Datenvorbereitung
data_recipe <- recipe(wage_eur ~ ., data = train_data)
recipes <- list(lm_recipe = data_recipe,
                rf_recipe = data_recipe,
                xgb_recipe = data_recipe)

# Modelle definieren
lm_model <- linear_reg() %>%
  set_engine("lm")

rf_model <- rand_forest(trees = tune(), min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

xgb_model <- boost_tree(trees = tune(), tree_depth = tune(), learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

models <- list(lm_model = lm_model,
               rf_model = rf_model,
               xgb_model = xgb_model)

# Workflows erstellen
workflows <- workflow_set(preproc = recipes,
                          models = models,
                          cross = FALSE)

# Cross-Validation
set.seed(123)
cv_folds <- vfold_cv(train_data, v = 3)


# Funktion zur Modellauswertung definieren
tune_and_evaluate <- function(workflow, cv_folds) {
  workflows %>%
    workflow_map("tune_grid",
                 seed = 123,
                 verbose = TRUE,
                 metrics = metric_set(rmse, rsq),
                 resamples = cv_folds, 
                 control = control_resamples(save_pred = TRUE,
                                             parallel_over = "everything",
                                             save_workflow = FALSE))
}

# Modelle trainieren und tunen
results <- tune_and_evaluate(workflows, cv_folds)


# 4. Messen Sie die Güte des Modells bzw. vergleichen Sie die Güte der Modelle und wählen ein finales Modell. Nutzen Sie dazu statistische Kennzahlen, und bewerten Sie Ihre Vorhersagen auch aus ökonomischer Perspektive. Bewerten Sie die Güte des Modells auf einem Testdatensatz, der 20% der Daten umfasst und den Sie nicht für das Training verwendet haben.

# Metriken aufnehmen 
collect_metrics(results)

# Visualisierung 
#2. Ergebnis
autoplot(results,
         rank_metric = "rsq",  
         metric = "rsq",       
         select_best = TRUE)

# zeigen des besten Ergebnisses pro workflow 
# 3. Ergebnis
results %>% rank_results(rank_metric = "rsq",
                         select_best = TRUE) %>% 
  filter(.metric == "rsq")


# Extrahieren des besten Ergebnisses 
best_results <- 
  results %>% 
  extract_workflow_set_result("xgb_recipe_xgb_model") %>% 
  select_best(metric = "rsq")

# 4. Ergebnis
best_results

# Test Ergebnisse 
best_test_results <- 
  results %>% 
  extract_workflow("xgb_recipe_xgb_model") %>% 
  finalize_workflow(best_results) %>% 
  last_fit(split = data_split)

# 5. Ergebnis 
best_test_results %>% 
  collect_metrics()

# Vorhersagen 
best_test_results$.predictions

# 5.Ermitteln Sie, welche Merkmale sich gut zur Vorhersage eignen bzw. die Spielergehälter beinflussen und verschaffen Sie sich ein Verständnis, wie diese Zusammenhänge konkret aussehen.

best_workflow <- 
  workflows %>% 
  extract_workflow("xgb_recipe_xgb_model") %>% 
  finalize_workflow(best_results)


# Anpassen des besten Workflow auf die Trainingsdaten
final_fit <- fit(best_workflow, data = train_data)

# Anwendung auf Testdaten 
test_results <- predict(final_fit, test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = wage_eur, estimate = .pred)

test_results

# Berechnen der Feature Importance Matrix
final_xgb_model <- pull_workflow_fit(final_fit)$fit

importance_matrix <- xgb.importance(model = final_xgb_model)

# Plot Feature Importance
# 6. Ergebnis 
xgb.plot.importance(importance_matrix, main = "XGBoost Feature Importance")

#7. Ergebnis 
importance_matrix

