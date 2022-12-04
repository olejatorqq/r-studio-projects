---
title: "Lab1"
output: html_document
date: "2022-10-19"
---


## Проанализировать встроенный в пакет dplyr набор данных starwars с помощью языка R и ответить на вопросы:

```{r}
library(dplyr)
```

### 1. Сколько строк в датафрейме?

```{r}
starwars %>% nrow()
```


### 2. Сколько столбцов в датафрейме?

```{r}
starwars %>% ncol()
```


### 3. Как просмотреть примерный вид датафрейма?

```{r}
starwars %>% glimpse()
```


### 4. Сколько уникальных рас персонажей (species) представлено в данных?

```{r}
nrow(unique(starwars[c("species")]))
```


### 5. Найти самого высокого персонажа.
```{r}
print(starwars$name[which.max(starwars$height)])
```

### 6. Найти всех персонажей ниже 170
```{r}
print(starwars$name[which(starwars$height < 170)])
```

### 7. Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ подсчитать по формуле 𝐼 = 𝑚/ℎ2 , где 𝑚– масса (weight), а ℎ – рост (height).
```{r}
starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi)
```

### 8. Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по отношению массы (mass) к росту
(height) персонажей.
```{r}
starwars %>% 
  mutate(vyt=mass/height) %>%
  arrange(vyt) %>% 
  slice(1:10)
``` 

### 9. Найти средний возраст персонажей каждой расы вселенной Звездных войн.
```{r}
starwars %>% 
  filter_at(vars(species, birth_year), all_vars(!is.na(.))) %>%
  group_by(species) %>% 
  summarise(mean_age = mean(birth_year), .groups = 'drop')
```

### 10. Найти самый распространенный цвет глаз персонажей вселенной Звездных войн.
```{r}
starwars %>% 
  filter(!is.na(eye_color)) %>%
  count(eye_color, sort = TRUE) %>%
  slice(1:1)
```

### 11. Подсчитать среднюю длину имени в каждой расе вселенной Звездных войн.
```{r}
starwars %>% 
  filter_at(vars(species, name), all_vars(!is.na(.))) %>%
  group_by(species) %>% 
  summarise(mean_name = mean(nchar(name)), .groups = 'drop')
```
