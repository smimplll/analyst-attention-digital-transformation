# Как загрузить проект на GitHub

Рекомендуемое название репозитория:

`analyst-attention-digital-transformation`

Для портфолио репозиторий обычно делают публичным. Если вы пока не уверены, что имеете право публично распространять подготовленные данные, сначала создайте приватный репозиторий — видимость можно изменить позднее.

## Вариант 1. GitHub Desktop — самый простой

1. Скачайте и распакуйте архив проекта. Загружать сам ZIP в репозиторий не нужно.
2. Установите GitHub Desktop и войдите в свой GitHub-аккаунт.
3. Выберите **File → Add Local Repository** и укажите распакованную папку проекта.
4. Если GitHub Desktop сообщит, что папка ещё не является репозиторием, выберите создание репозитория для этой папки.
5. В поле Summary напишите `Initial cleaned course project` и нажмите **Commit to main**.
6. Нажмите **Publish repository**.
7. Оставьте имя `analyst-attention-digital-transformation`.
8. Для публичного портфолио снимите флажок **Keep this code private**. Для приватной публикации оставьте его включённым.
9. Нажмите **Publish Repository**.

Официальная инструкция GitHub:  
https://docs.github.com/en/desktop/adding-and-cloning-repositories/adding-an-existing-project-to-github-using-github-desktop

## Вариант 2. Через терминал

Откройте терминал в распакованной папке и выполните:

```bash
git init
git add .
git commit -m "Initial cleaned course project"
git branch -M main
```

Затем создайте на GitHub пустой репозиторий. Не добавляйте при создании README, `.gitignore` или лицензию — они уже находятся в проекте.

После создания замените `YOUR_USERNAME` своим логином:

```bash
git remote add origin https://github.com/YOUR_USERNAME/analyst-attention-digital-transformation.git
git push -u origin main
```

Официальная инструкция GitHub:  
https://docs.github.com/en/migrations/importing-source-code/using-the-command-line-to-import-source-code/adding-locally-hosted-code-to-github

## Вариант 3. Через сайт GitHub

1. Создайте новый репозиторий.
2. Откройте его и выберите **Add file → Upload files**.
3. Перетащите содержимое распакованной папки, а не ZIP-файл целиком.
4. Введите сообщение `Initial cleaned course project`.
5. Нажмите **Commit changes**.

Через браузер GitHub принимает отдельные файлы размером до 25 MiB; файлы свыше 100 MiB GitHub блокирует. В этом проекте таких файлов нет.

Официальные инструкции:  
https://docs.github.com/en/repositories/working-with-files/managing-files/adding-a-file-to-a-repository  
https://docs.github.com/en/repositories/working-with-files/managing-large-files/about-large-files-on-github

## Как обновлять проект позднее

После изменений выполните:

```bash
git add .
git commit -m "Describe the update"
git push
```

В GitHub Desktop те же действия выполняются кнопками **Commit to main** и **Push origin**.

## Что проверить после публикации

- README отображается на главной странице;
- папки `R`, `data`, `docs` и `report` открываются;
- в репозитории нет старого 8 000-строчного скрипта;
- нет файлов с названием «копия», `v2`, `merged`, PSM или старой папки результатов;
- `data/raw/panel_input.csv` отображается как таблица;
- адрес репозитория добавлен в резюме.

