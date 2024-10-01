library(tidyverse)
library(ggplot2)

Lebron <- read_csv("LeBron2023.csv")
library(dplyr)

Lebron
summary(Lebron)

ggplot(Lebron, aes(x = G, y = PlusMinus, group = 1)) + 
  geom_line() + 
  labs(x = "Game", y = "Plus/Minus", 
       title = "LeBron James: Plus/Minus over Time") +
  scale_x_continuous(breaks = seq(min(Lebron$G), max(Lebron$G), 1))



#FG r^2 = 0.008
ggplot(Lebron, aes(x = FG, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Field Goals Made", y = "Plus/Minus", 
       title = "LeBron James: Field Goals vs Plus/Minus") 

model <- lm(PlusMinus ~ FG, data = Lebron)
summary(model)

#FGA r^2 = 0.002
ggplot(Lebron, aes(x = FGA, y = PlusMinus)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Field Goal Attempts", y = "Plus/Minus",
       title = "LeBron James: Field Goal Attempts vs Plus/Minus")
model1 <- lm(PlusMinus ~ FGA, data = Lebron)
summary(model1)

#FG% r^2 = 0.02
ggplot(Lebron, aes(x = FGPercent, y = PlusMinus)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Field Goal Percentage", y = "Plus/Minus",
       title = "LeBron James: Field Goal Percentage vs Plus/Minus") 

model2 <- lm(PlusMinus ~ FGPercent, data = Lebron)
summary(model2)

#3 point FG r^2 = 0.01
ggplot(Lebron, aes(x = ThreeP, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "3-Pointers Made", y = "Plus/Minus", 
       title = "LeBron James: 3-Pointers vs Plus/Minus") 

model3 <- lm(PlusMinus ~ ThreeP, data = Lebron)
summary(model3)

#3 point FGA r^2 = 0.04
ggplot(Lebron, aes(x = ThreePA, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "3-Point Attempts", y = "Plus/Minus", 
       title = "LeBron James: 3-Point Attempts vs Plus/Minus") 

model4 <- lm(PlusMinus ~ ThreePA, data = Lebron)
summary(model4)

#3 point percentage r^2 = 0.0002
ggplot(Lebron, aes(x = ThreePPercent, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Three Point Percentage", y = "Plus/Minus", 
       title = "LeBron James: Three Point Percentage vs Plus/Minus") 

model5 <- lm(PlusMinus ~ ThreePPercent, data = Lebron)
summary(model5)

#Free throws r^2 = 0.0004
ggplot(Lebron, aes(x = FT, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Free Throws Made", y = "Plus/Minus", 
       title = "LeBron James: Free Throws vs Plus/Minus") 

model6 <- lm(PlusMinus ~ FT, data = Lebron)
summary(model6)

#Free throw attempts r^2 = 0.00005
ggplot(Lebron, aes(x = FTA, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Free Throws Attempted", y = "Plus/Minus", 
       title = "LeBron James: Free Throws Attempted vs Plus/Minus") 

model7 <- lm(PlusMinus ~ FTA, data = Lebron)
summary(model7)

#free throw percentage r^2 =0.02
ggplot(Lebron, aes(x = FTPercent, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Free Throw Percentage", y = "Plus/Minus", 
       title = "LeBron James: Free Throw Percentage vs Plus/Minus") 

model8 <- lm(PlusMinus ~ FTPercent, data = Lebron)
summary(model8)

#offensive rebounds r^2 = 0.01
ggplot(Lebron, aes(x = ORB, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Offensive Rebounds", y = "Plus/Minus", 
       title = "LeBron James: Offensive Rebounds vs Plus/Minus")

model9 <- lm(PlusMinus ~ ORB, data = Lebron)
summary(model9)

#defensive rebounds r^2 = 0.0001
ggplot(Lebron, aes(x = DRB, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Defensive Rebounds", y = "Plus/Minus", 
       title = "LeBron James: Defensive Rebounds vs Plus/Minus")

model10 <- lm(PlusMinus ~ DRB, data = Lebron)
summary(model10)

#TRB r^2= 0.003
ggplot(Lebron, aes(x = TRB, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Total Rebounds", y = "Plus/Minus", 
       title = "LeBron James: Total Rebounds vs Plus/Minus") 

model11 <- lm(PlusMinus ~ TRB, data = Lebron)
summary(model11)

#AST r^2 = 0.01
ggplot(Lebron, aes(x = AST, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Assists", y = "Plus/Minus", 
       title = "LeBron James: Assists vs Plus/Minus") 

model12 <- lm(PlusMinus ~ AST, data = Lebron)
summary(model12)

#steals r^2 =0.008
ggplot(Lebron, aes(x = STL, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Steals", y = "Plus/Minus", 
       title = "LeBron James: Steals vs Plus/Minus") 

model13 <- lm(PlusMinus ~ STL, data = Lebron)
summary(model13)

#blocks r^2 = 0.000007
ggplot(Lebron, aes(x = BLK, y = PlusMinus)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Blocks", y = "Plus/Minus", 
       title = "LeBron James: Blocks vs Plus/Minus") 


model14 <- lm(PlusMinus ~ BLK, data = Lebron)
summary(model14)

#turnovers r^2 = 0.0004
ggplot(Lebron, aes(x = TOV, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Turnovers", y = "Plus/Minus", 
       title = "LeBron James: Turnovers vs Plus/Minus") 


model15 <- lm(PlusMinus ~ TOV, data = Lebron)
summary(model15)

#personal fouls r^2 =0.001
ggplot(Lebron, aes(x = PF, y = PlusMinus)) + 
  geom_point(alpha = 0.5, color = "darkblue") + 
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Personal Fouls", y = "Plus/Minus", 
       title = "LeBron James: Personal Fouls vs Plus/Minus") 


model16 <- lm(PlusMinus ~ PF, data = Lebron)
summary(model16)

#PTS
ggplot(Lebron, aes(x = PTS, y = PlusMinus)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Points", y = "Plus/Minus",
       title = "LeBron James: Points vs Plus/Minus") 

model17 <- lm(PlusMinus ~ PTS, data = Lebron)
summary(model17)

#pts vs fgpercent  r^2 =0.521
ggplot(Lebron, aes(x = PTS, y = FGPercent)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Points", y = "Field Goal Percentage",
       title = "LeBron James: Points vs Field Goal Percentage")
model18 <- lm(FGPercent ~ PTS, data = Lebron)
summary(model18)

#pts vs Three points r^2 =0.276
ggplot(Lebron, aes(x = PTS, y = ThreeP)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Points", y = "Three-Pointers",
       title = "LeBron James: Points vs Three-Pointers ")

model19 <- lm(ThreeP ~ PTS, data = Lebron)
summary(model19)

#pts vs three point attempts r^2 =0.161
ggplot(Lebron, aes(x = PTS, y = ThreePA)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Points", y = "Three-Point Attempts",
       title = "LeBron James: Points vs Three-Point Attempts")

model20 <- lm(ThreePA ~ PTS, data = Lebron)
summary(model20)

#pts vs three point % r^2-0.182
ggplot(Lebron, aes(x = PTS, y = ThreePPercent)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Points", y = "Three-Point Percentage",
       title = "LeBron James: Points vs Three-Point Percentage")

model21 <- lm(ThreePPercent ~ PTS, data = Lebron)
summary(model21)

#pts vs free throws r^2 =0.296
ggplot(Lebron, aes(x = PTS, y = FT)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Points", y = "Free Throws Made",
       title = "LeBron James: Points vs Free Throws Made")

model23 <- lm(FT ~ PTS, data = Lebron)
summary(model23)

#pts vs free throw attempts r^2 =.2381
ggplot(Lebron, aes(x = PTS, y = FTA)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Points", y = "Free Throw Attempts",
       title = "LeBron James: Points vs Free Throw Attempts")

model24 <- lm(FTA ~ PTS, data = Lebron)
summary(model24)

#pts vs free throw percentage r^2 = 0.065
ggplot(Lebron, aes(x = PTS, y = FTPercent)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Points", y = "Free Throw Percentage",
       title = "LeBron James: Points vs Free Throw Percentage")

model25 <- lm(FTPercent ~ PTS, data = Lebron)
summary(model25)

#pts vs field goals r^2 = 0.879
ggplot(Lebron, aes(x = PTS, y = FG)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Points", y = "Field Goals",
       title = "LeBron James: Points vs Field Goals")

model26 <- lm(FG ~ PTS, data = Lebron)
summary(model26)

#pts vs field goal attempts r^2 = 0.419
ggplot(Lebron, aes(x = PTS, y = FGA)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Points", y = "Field Goal Attempts",
       title = "LeBron James: Points vs Field Goal Attempts")

model27 <- lm(FGA ~ PTS, data = Lebron)
summary(model27)