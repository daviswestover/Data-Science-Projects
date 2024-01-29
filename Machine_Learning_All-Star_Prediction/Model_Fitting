## Exploratory Data Analysis

```{r, echo = FALSE}
data %>%
  select(is.numeric) %>%
  cor() %>%
  corrplot()

#gives us early clue that AS has correlation with points

#PTS, FTs, Turnovers


```{r, echo = FALSE}

ggplot(data, aes(x = factor(AS), y = PTS, fill = AS)) + 
  geom_bar(stat = "summary", fun = "mean") +
  xlab('All-Star')

ggplot(data, aes(x = factor(AS), y = MP, fill = AS)) + 
  geom_bar(stat = "summary", fun = "mean") +
  xlab('All-Star')


ggplot(data, aes(x = factor(AS), y = FT, fill = AS)) + 
  geom_bar(stat = "summary", fun = "mean") +
  xlab('All-Star')

```

```{r, echo = FALSE}
data20 <- filter(data, MP >= 20)

ggplot(data20, aes(x = factor(AS), y = PTS, fill = AS)) + 
  geom_bar(stat = "summary", fun = "mean") +
  xlab('All-Star')

ggplot(data20, aes(x = factor(AS), y = MP, fill = AS)) + 
  geom_bar(stat = "summary", fun = "mean") +
  xlab('All-Star')


ggplot(data20, aes(x = factor(AS), y = FT, fill = AS)) + 
  geom_bar(stat = "summary", fun = "mean") +
  xlab('All-Star')

```


## Model Fitting


```{r, echo = FALSE}
nba_split <- initial_split(data, strata = "AS", prop = 0.75)

nba_train <- training(nba_split)
nba_test <- testing(nba_split)

nba_folds <- vfold_cv(nba_train, v = 10)

```

```{r, echo = FALSE}
nonAllstars <- dim(filter(data, data$AS == 0)) / (dim(filter(data,data$AS == 0)) + dim(filter(data,data$AS == 1))) #.95
Allstars <- dim(filter(data,data$AS == 1)) / (dim(filter(data,data$AS == 0)) + dim(filter(data,data$AS == 1))) #.05
```

#### Recipe


```{r, echo = FALSE}
nba_recipe <- recipe(AS ~ Age + G + GS + MP + FG +`FG%` + `3P` + `3P%` + `2P%` + `eFG%` + FT + `FT%` + ORB + DRB + AST + STL + BLK + TOV + PF + PTS + Market, data = nba_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_predictors()) %>%   # standardizing our predictors
  step_scale(all_predictors()) %>%
  step_upsample(AS, over_ratio = 0.3, skip = TRUE)

prep <- prep(nba_recipe) %>% bake(new_data = nba_train)

 prep_ratio <- prep(nba_recipe) %>% bake(new_data = nba_train) %>% 
  group_by(AS) %>% 
  summarise(count = n())


```

#### The Models

#### Fit


```{r, echo = FALSE}
#set up

#knn
knn_mod_cv <- nearest_neighbor(neighbors = tune()) %>%
  set_mode("classification") %>%
  set_engine("kknn")

knn_wkflow_cv <- workflow() %>% 
  add_model(knn_mod_cv) %>% 
  add_recipe(nba_recipe)

#log
log_mod <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

log_wkflow <- workflow() %>% 
  add_model(log_mod) %>% 
  add_recipe(nba_recipe)

#net
netlog_mod <- multinom_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

netlog_wkflow <- workflow() %>% 
  add_model(netlog_mod) %>% 
  add_recipe(nba_recipe)

#qda
qda_mod <- discrim_quad() %>%
  set_mode("classification") %>%
  set_engine("MASS")

qda_wkflow <- workflow() %>%
  add_model(qda_mod) %>%
  add_recipe(nba_recipe)

```

```{r, echo = FALSE}
#grids
neighbors_grid <- grid_regular(neighbors(range = c(1,8)), levels = 5)

elas_grid <-  grid_regular(penalty(), mixture(range = c(0,1)), levels = 5)
```

```{r,echo = FALSE}
#fit

knn_fit  <- tune_grid(
  knn_wkflow_cv,
  resamples = nba_folds, 
  grid = neighbors_grid
)


log_folds <- log_wkflow %>% 
  fit_resamples(resamples = nba_folds)



qda_folds <- qda_wkflow %>% 
  fit_resamples(resamples = nba_folds)


netlog_fit <- tune_grid(
  object = netlog_wkflow, 
  resamples = nba_folds, 
  grid = elas_grid
)

```

```{r, echo = FALSE}
autoplot(knn_fit)
```

```{r, echo = FALSE}
autoplot(netlog_fit)
```


```{r, include = FALSE }
collect_metrics(knn_fit)
collect_metrics(netlog_fit)
collect_metrics(qda_folds)
collect_metrics(log_folds)

best_netlog <- select_best(netlog_fit, metric = "roc_auc", penalty, mixture) #p=.00000000001 m=.25
best_knn <- select_best(knn_fit, metric = "roc_auc",neighbors) #k=8
best_netlog #.98269
best_knn #.9439
```

```{r, echo = FALSE}
Scores <- c(.9381951, .9825563, 
                .9826456, .9667013)
Models <- c("KNN", "Logistic Regression", "Elastic Net", "QDA")
results <- tibble( Models = Models,Scores = Scores)
results %>%
  arrange(-Scores)
#best is netlog
```


## Results

```{r, echo = FALSE}
best_fit <- finalize_workflow(netlog_wkflow, best_netlog)

best_fit <- fit(best_fit, nba_train)

augment(best_fit, new_data = nba_test) %>%
  roc_curve(truth = AS, estimate = .pred_0) %>%
  autoplot()
```


```{r, echo = FALSE}
augment(best_fit, new_data = nba_test) %>%
  conf_mat(truth = AS, estimate = .pred_class) %>%
  autoplot(type = "heatmap")
#89, 96
```
