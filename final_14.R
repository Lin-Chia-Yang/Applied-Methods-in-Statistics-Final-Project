library(ggplot2)
data = read.csv("nba_stat.csv", head = T)
point_guard <- subset(data, Pos == "PG")
shooting_guard <- subset(data, Pos == "SG")
small_forward <- subset(data, Pos == "SF")
power_forward <- subset(data, Pos == "PF")
center <- subset(data, Pos == "C")
guard <- rbind(point_guard, shooting_guard)
frontcourt <-rbind(small_forward, power_forward, center)
fans_votes = 0
players_votes = 0
media_votes = 0
for(i in 1:nrow(data)){
  fans_votes = fans_votes + data[i, "FANS"]
  players_votes = players_votes + data[i, "PLAYERS"]
  media_votes = media_votes + data[i, "MEDIA"]
}
print(fans_votes / nrow(data))
print(players_votes / nrow(data))
print(media_votes / nrow(data))
fans_guard = lm(FANS ~ Tm +  GS + MP + FG + FGA  + X3P + X3PA + X3P.
              + X2P. + eFG. + FT +  TRB
              + AST + TOV + PF + PTS, data = guard)
fg_summary <-summary(fans_guard)
fg_p_values <- fg_summary$coef[, "Pr(>|t|)"]
fg_names <- names(fg_summary$coef[, "Pr(>|t|)"])
fg_coef <-fg_summary$coefficients
fg_category = c()
fg_value = c()
fg_pvalue = c()

for(i in 1:length(fg_p_values)){
  if(fg_p_values[i] < 0.05){
    fg_category <- c(fg_category, fg_names[i])
    fg_value <- c(fg_value, fg_coef[i])
    fg_pvalue <- c(fg_pvalue, fg_p_values[i])
  }
}
fg_df <- data.frame(Name = fg_category, Value = fg_value)
fg_pvalue <- data.frame(Name = fg_category, Value = fg_pvalue)
print(fg_pvalue)
ggplot(fg_df, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("Correlation") +
  ggtitle("Fans Vote For Guards")
ggplot(fg_pvalue, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("P-value") +
  ggtitle("Fans Vote For Guards")

fans_frontcourt = lm(FANS ~ Tm + MP + FG + FGA + X3P + X3PA 
                     + X2P + X2P. + X2PA + eFG. + FT  + ORB +  TRB
                     + AST + STL + BLK + TOV + PF + PTS, data = frontcourt)
ff_summary <-summary(fans_frontcourt)
ff_p_values <- ff_summary$coef[, "Pr(>|t|)"]
ff_names <- names(ff_summary$coef[, "Pr(>|t|)"])
ff_coef <-ff_summary$coefficients
ff_category = c()
ff_value = c()
ff_pvalue = c()
for(i in 1:length(ff_p_values)){
  if(ff_p_values[i] < 0.05){
    ff_category <- c(ff_category, ff_names[i])
    ff_value <- c(ff_value, ff_coef[i])
    ff_pvalue <- c(ff_pvalue, ff_p_values[i])
  }
}
ff_df <- data.frame(Name = ff_category, Value = ff_value)
ff_pvalue <- data.frame(Name = ff_category, Value = ff_pvalue)

ggplot(ff_df, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("Correlation") +
  ggtitle("Fans Vote For Frontcourts")
ggplot(ff_pvalue, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("P-value") +
  ggtitle("Fans Vote For Frontcourts")

players_guard = lm(PLAYERS ~ Tm + GS + MP + FG + FGA  + X3P + X3PA 
                   +  X2P. + eFG. + FT + FTA + TRB
                   +  STL +  TOV + PF + PTS, data = guard)
pg_summary <-summary(players_guard)
pg_p_values <- pg_summary$coef[, "Pr(>|t|)"]
pg_names <- names(pg_summary$coef[, "Pr(>|t|)"])
pg_coef <-pg_summary$coefficients
pg_category = c()
pg_value = c()
pg_pvalue = c()

for(i in 1:length(pg_p_values)){
  if(pg_p_values[i] < 0.05){
    pg_category <- c(pg_category, pg_names[i])
    pg_value <- c(pg_value, pg_coef[i])
    pg_pvalue <- c(pg_pvalue, pg_p_values[i])
  }
}
pg_df <- data.frame(Name = pg_category, Value = pg_value)
pg_pvalue <- data.frame(Name = pg_category, Value = pg_pvalue)
print(pg_pvalue)
ggplot(pg_df, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("Correlation") +
  ggtitle("Players Vote For Guards")
ggplot(pg_pvalue, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("P-value") +
  ggtitle("Players Vote For Guards")

players_frontcourt = lm(PLAYERS ~ Tm + MP + FG + FGA + FG. + X3P + X3PA 
                        +  X2P. + X2PA + FT + ORB +  TRB
                        + AST + STL + BLK + TOV + PF + PTS, data = frontcourt)
pf_summary <-summary(players_frontcourt)
pf_p_values <- pf_summary$coef[, "Pr(>|t|)"]
pf_names <- names(pf_summary$coef[, "Pr(>|t|)"])
pf_coef <-pf_summary$coefficients
pf_category = c()
pf_value = c()
pf_pvalue = c()

for(i in 1:length(pf_p_values)){
  if(pf_p_values[i] < 0.05){
    pf_category <- c(pf_category, pf_names[i])
    pf_value <- c(pf_value, pf_coef[i])
    pf_pvalue <- c(pf_pvalue, pf_p_values[i])
  }
}
pf_df <- data.frame(Name = pf_category, Value = pf_value)
pf_pvalue <- data.frame(Name = pf_category, Value = pf_pvalue)
print(pf_pvalue)
ggplot(pf_df, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("Correlation") +
  ggtitle("Players Vote For Frontcourts")
ggplot(pf_pvalue, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("P-value") +
  ggtitle("Players Vote For Frontcourts")

media_guard = lm(MEDIA ~ Tm + GS + MP + FG + FGA + X3P + X3PA 
                 + X2P. + eFG. + FT + FTA  + TRB
                 +  TOV + PF + PTS, data = guard)
mg_summary <-summary(media_guard)
mg_p_values <- mg_summary$coef[, "Pr(>|t|)"]
mg_names <- names(mg_summary$coef[, "Pr(>|t|)"])
mg_coef <-mg_summary$coefficients
mg_category = c()
mg_value = c()
mg_pvalue = c()

for(i in 1:length(mg_p_values)){
  if(mg_p_values[i] < 0.05){
    mg_category <- c(mg_category, mg_names[i])
    mg_value <- c(mg_value, mg_coef[i])
    mg_pvalue <- c(mg_pvalue, mg_p_values[i])
  }
}
mg_df <- data.frame(Name = mg_category, Value = mg_value)
mg_pvalue <- data.frame(Name = mg_category, Value = mg_pvalue)
print(mg_pvalue)
ggplot(mg_df, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("Correlation") +
  ggtitle("Media Vote For Guards")
ggplot(mg_pvalue, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("P-value") +
  ggtitle("Media Vote For Guards")

media_frontcourt = lm(MEDIA ~ Tm + G  + MP + FG + FG.  + X3P.
                      + X2P + X2PA  + FT + FT. + ORB + TRB
                      + AST + STL + PTS, data = frontcourt)
mf_summary <-summary(media_frontcourt)
mf_p_values <- mf_summary$coef[, "Pr(>|t|)"]
mf_names <- names(mf_summary$coef[, "Pr(>|t|)"])
mf_coef <-mf_summary$coefficients
mf_category = c()
mf_value = c()
mf_pvalue = c()

for(i in 1:length(mf_p_values)){
  if(mf_p_values[i] < 0.05){
    mf_category <- c(mf_category, mf_names[i])
    mf_value <- c(mf_value, mf_coef[i])
    mf_pvalue <- c(mf_pvalue, mf_p_values[i])
  }
}
mf_df <- data.frame(Name = mf_category, Value = mf_value)
mf_pvalue <- data.frame(Name = mf_category, Value = mf_pvalue)
print(mf_pvalue)
ggplot(mf_df, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("Correlation") +
  ggtitle("Media Vote For Frontcourts")
ggplot(mf_pvalue, aes(x = Name, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Category") +
  ylab("P-value") +
  ggtitle("Media Vote For Frontcourts")

fans_guard_interaction = lm(FANS ~ PTS * X3PA, data = guard)
summary(fans_guard_interaction)
players_guard_interaction = lm(PLAYERS ~ PTS * X3PA, data = guard)
summary(players_guard_interaction)
players_frontcourt_interaction = lm(PLAYERS ~ PTS * FGA, data = frontcourt)
summary(players_frontcourt_interaction)
media_guard_interaction = lm(MEDIA ~ PTS * X3PA, data = guard)
summary(media_guard_interaction)
media_frontcourt_interaction = lm(MEDIA ~ PTS * X2P, data = frontcourt)
summary(media_frontcourt_interaction)

residuals(fans_guard)
par(mfrow = c(1, 2))
plot(fans_guard)

residuals(fans_frontcourt)
par(mfrow = c(1, 2))
plot(fans_frontcourt)

residuals(players_guard)
par(mfrow = c(1, 2))
plot(players_guard)

residuals(players_frontcourt)
par(mfrow = c(1, 2))
plot(players_frontcourt)

residuals(media_guard)
par(mfrow = c(1, 2))
plot(media_guard)

residuals(media_frontcourt)
par(mfrow = c(1, 2))
plot(media_frontcourt)

data_2023 = read.csv("nba_2023.csv", head = T)

f_p_df <- data.frame()
p_p_df <- data.frame()
m_p_df <- data.frame()

for(i in 1:nrow(data_2023)){
  if(data_2023[i, "Pos"] == "PG"){
    f_pg_p <- round(predict(fans_guard, newdata = data_2023[i, ]))
    f_p_df <- rbind(f_p_df, data.frame(name = data_2023[i, "Player"], FANS = f_pg_p))
    p_pg_p <- round(predict(players_guard, newdata = data_2023[i, ]))
    p_p_df <- rbind(p_p_df, data.frame(name = data_2023[i, "Player"], PLAYERS = p_pg_p))
    m_pg_p <- round(predict(media_guard, newdata = data_2023[i, ]))
    m_p_df <- rbind(m_p_df, data.frame(name = data_2023[i, "Player"], PLAYERS = m_pg_p))
  }
  else if(data_2023[i, "Pos"] == "SG"){
    f_sg_p <- round(predict(fans_guard, newdata = data_2023[i, ]))
    f_p_df <- rbind(f_p_df, data.frame(name = data_2023[i, "Player"], FANS = f_sg_p))
    p_sg_p <- round(predict(players_guard, newdata = data_2023[i, ]))
    p_p_df <- rbind(p_p_df, data.frame(name = data_2023[i, "Player"], PLAYERS = p_sg_p))
    m_sg_p <- round(predict(media_guard, newdata = data_2023[i, ]))
    m_p_df <- rbind(m_p_df, data.frame(name = data_2023[i, "Player"], PLAYERS = m_sg_p))
  }
  else if(data_2023[i, "Pos"] == "SF"){
    f_sf_p <- round(predict(fans_frontcourt, newdata = data_2023[i, ]))
    f_p_df <- rbind(f_p_df, data.frame(name = data_2023[i, "Player"], FANS = f_sf_p))
    p_sf_p <- round(predict(players_frontcourt, newdata = data_2023[i, ]))
    p_p_df <- rbind(p_p_df, data.frame(name = data_2023[i, "Player"], PLAYERS = p_sf_p))
    m_sf_p <- round(predict(media_frontcourt, newdata = data_2023[i, ]))
    m_p_df <- rbind(m_p_df, data.frame(name = data_2023[i, "Player"], PLAYERS = m_sf_p))
  }
  else if(data_2023[i, "Pos"] == "PF"){
    f_pf_p <- round(predict(fans_frontcourt, newdata = data_2023[i, ]))
    f_p_df <- rbind(f_p_df,  data.frame(name = data_2023[i, "Player"], FANS = f_pf_p))
    p_pf_p <- round(predict(players_frontcourt, newdata = data_2023[i, ]))
    p_p_df <- rbind(p_p_df, data.frame(name = data_2023[i, "Player"], PLAYERS = p_pf_p))
    m_pf_p <- round(predict(media_frontcourt, newdata = data_2023[i, ]))
    m_p_df <- rbind(m_p_df, data.frame(name = data_2023[i, "Player"], PLAYERS = m_pf_p))
  }
  else if(data_2023[i, "Pos"] == "C"){
    f_c_p <- round(predict(fans_frontcourt, newdata = data_2023[i, ]))
    f_p_df <- rbind(f_p_df,data.frame(name = data_2023[i, "Player"], FANS = f_c_p))
    p_c_p <- round(predict(players_frontcourt, newdata = data_2023[i, ]))
    p_p_df <- rbind(p_p_df, data.frame(name = data_2023[i, "Player"], PLAYERS = p_c_p))
    m_c_p <- round(predict(media_frontcourt, newdata = data_2023[i, ]))
    m_p_df <- rbind(m_p_df, data.frame(name = data_2023[i, "Player"], PLAYERS = m_c_p))
  }
}

write.table(f_p_df, file = "fans_prediction.csv", col.names = !file.exists("output.csv"), row.names = FALSE)
write.table(p_p_df, file = "players_prediction.csv", col.names = !file.exists("output.csv"), row.names = FALSE)
write.table(m_p_df, file = "media_prediction.csv", col.names = !file.exists("output.csv"), row.names = FALSE)

