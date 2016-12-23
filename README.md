# Url Shortener Bot for Telegram
### Проектное задание по курсу CS314 Функциональное программирование "Агрегатор url-сократителей".  

#### Исполнители:  
* Валиева Яна
* Данилин Дмитрий 
* Черкасов Владимир

#### Постановка задачи: 
##### Минимальные реализуемые возможности:  
1. Реализация в виде бота для мессенджера Telegram.  
2. Назначение: предоставление пользователю укороченных ссылок с возможностью выбора сервиса.
3. Доступные для выбора сервисы сервисы:   
    * [google](https://goo.gl/ "Сервис по умолчанию")
    * [bitly] (https://bitly.com/)
    * [qps](http://qps.ru/)
4. Формат доступных команд:
    * **/service_name link** - *запрос на создание укороченной ссылки с использованием конкретного сервиса из списка доступных*
    * **link** - *преобразование ссылки с помощью сервиса по умолчанию (google)*
    * **/default service** - *для установки сервиса по умолчанию, подразумевается авторизация пользователей и хранение пользовательских настроек в базе данных*
5. Используемые библиотеки: 
    * [Telegram Bot API] (https://hackage.haskell.org/package/telegram-api)
    * [HTTP-requests](https://hackage.haskell.org/package/http-client)
    * [Persistent] (https://hackage.haskell.org/package/persistent) 
6. Сервер находится в облаке Azure.   

Бота можно найти здесь: telegram.me/url314bot  


