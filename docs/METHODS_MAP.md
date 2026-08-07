# Соответствие кода тексту курсовой

| Элемент исследования | Скрипт | Основной результат |
|---|---|---|
| Подготовка панели и ручные исправления | `R/01_prepare_data.R` | `data/processed/analysis_panel.csv` |
| Описательная статистика | `R/02_descriptive_analysis.R` | `02_descriptive_statistics.csv` |
| Распределения индексов и внимания | `R/02_descriptive_analysis.R` | рисунки `01`–`03` |
| Корреляционная матрица | `R/02_descriptive_analysis.R` | `03_correlation_matrix.csv` |
| Три VIF-спецификации | `R/02_descriptive_analysis.R` | `04_vif.csv` |
| Шесть базовых регрессий | `R/03_main_models.R` | `05_main_models_full_sample.html` |
| Нелинейная модель полной выборки | `R/03_main_models.R` | `06_preferred_full_models.html` |
| BP, RESET и cluster RESET | `R/03_main_models.R` | `06_full_sample_diagnostics.csv` |
| Частичные остатки полной выборки | `R/03_main_models.R` | рисунок `05` |
| Cook’s distance `> 4/n` | `R/04_diagnostics_and_cook.R` | таблицы `07_*` |
| Сравнение четырёх функциональных форм | `R/04_diagnostics_and_cook.R` | `08_functional_form_comparison.csv` |
| Итоговая no-Cook модель | `R/04_diagnostics_and_cook.R` | `09_preferred_no_cook_models.html` |
| Частичные остатки no-Cook | `R/04_diagnostics_and_cook.R` | рисунок `06` |
| Формы внимания и interaction | `R/05_robustness.R` | таблицы `10_*` |
| Альтернативные выборки и sector-year FE | `R/05_robustness.R` | таблицы `11_*` |
| Firm FE + year FE | `R/05_robustness.R` | таблицы `12_*` |

Не включены PSM, first differences, lead placebo, Hausman, Mundlak и within-between decomposition, поскольку эти проверки отсутствуют в тексте курсовой и не участвуют в её выводах.

