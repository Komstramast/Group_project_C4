# scripts/04_detect_anomalies_h2o.R

library(h2o)
library(dplyr)
library(readr)

# 🚀 Инициализация H2O
h2o.init(nthreads = -1)

# 📥 Загрузка данных
commits <- read_csv("data/commits_transformed.csv")

# 🧠 Подготовим только числовые признаки
features <- commits %>%
  select(loc_change, added, deleted, message_length, hour, n_files) %>%
  mutate_all(~replace_na(., 0))  # заменим NA на 0

# Преобразуем в H2O Frame
h2o_data <- as.h2o(features)

# 📦 Загрузка предобученной модели автоэнкодера
# Убедись, что путь корректный и модель была обучена ранее
model_path <- "models/pretrained_autoencoder"
autoencoder <- h2o.loadModel(model_path)

# 🔍 Получаем реконструкционные ошибки
recon_error <- h2o.anomaly(autoencoder, h2o_data, per_feature = FALSE)
recon_error_df <- as.data.frame(recon_error)
commits$recon_error <- recon_error_df$Reconstruction.MSE

# 📊 Определим аномалии (например, top 5% по ошибке)
threshold <- quantile(commits$recon_error, 0.95)
commits$anomaly_flag <- commits$recon_error > threshold

# 💾 Сохраняем результат
if (!dir.exists("outputs")) dir.create("outputs")
write_csv(commits, "outputs/anomalies.csv")

message("✅ Аномалии сохранены в outputs/anomalies.csv")

