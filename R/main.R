# 1. качаем гит репо какой то в дир /repos
# 2. создаем промпт и генерируем выход через https://github.com/sonsoleslp/rlmstudio.git
# 3. .env для хранения инфы

devtools::install_github("sonsoleslp/rlmstudio")

library("rlmstudio")

prompt_lm("Tell me a joke!")