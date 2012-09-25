<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/docs-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="local-name">
    <xsl:text>Руководство пользователя</xsl:text>
</xsl:template>

<xsl:template name="s-title-docs">
    <xsl:call-template name="local-name"/>
</xsl:template>

<xsl:template name="e-docs-head">
    <xsl:call-template name="local-name"/>
</xsl:template>

<xsl:template name="s-about-docs">
    <nav class="b-a">
        <ul class="b-ac-ul m-docs-p">
            <li class="e-ac-ul">
                <a href="#ht-mainwindow">
                    <xsl:text>Главное окно личного кабинета рекламодателя.</xsl:text>
                </a>
            </li>
            <li class="e-ac-ul">
                <a href="#ht-new">
                    <xsl:text>Создание новой рекламной кампании.</xsl:text>
                </a>
            </li>
            <li class="e-ac-ul">
                <a href="#ht-view">
                    <xsl:text>Просмотр настроек рекламной кампании.</xsl:text>
                </a>
            </li>
            <li class="e-ac-ul">
                <a href="#ht-start-stop">
                    <xsl:text>Запуск/остановка рекламной кампании.</xsl:text>
                </a>
            </li>
            <li class="e-ac-ul">
                <a href="#ht-delete">
                    <xsl:text>Удаление рекламной кампании.</xsl:text>
                </a>
            </li>
            <li class="e-ac-ul">
                <a href="#ht-stat">
                    <xsl:text>Просмотр статистики по рекламным кампаниям.</xsl:text>
                </a>
            </li>
        </ul>
    </nav>
    <article class="b-a" id="ht-mainwindow">
        <xsl:call-template name="l-mainwindow"/>
    </article>
    <article class="b-a" id="ht-new">
        <xsl:call-template name="l-new"/>
    </article>
    <article class="b-a" id="ht-view">
        <xsl:call-template name="l-view"/>
    </article>
    <article class="b-a" id="ht-start-stop">
        <xsl:call-template name="l-start-stop"/>
    </article>
    <article class="b-a" id="ht-delete">
        <xsl:call-template name="l-delete"/>
    </article>
    <article class="b-a" id="ht-stat">
        <xsl:call-template name="l-stat"/>
    </article>
</xsl:template>

<!--
    ГЛАВНОЕ ОКНО ЛИЧНОГО КАБИНЕТА РЕКЛАМОДАТЕЛЯ
-->
<xsl:template name="l-mainwindow">
<h3 class="b-ah m-ah-ht1">
    <xsl:text>Главное окно личного кабинета рекламодателя</xsl:text>
</h3>
<section class="b-ac">
    <p class="m-docs-p">
        <xsl:text>После аутентификации, пользователь (рекламодатель) попадает </xsl:text>
        <xsl:text>в личный кабинет, главное окно которого представляет собой следующий вид:</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-1.jpg" />
    <p class="m-docs-p">
        <xsl:text>Панель меню главного окна личного кабинета содержит два меню: </xsl:text>
        <xsl:text>«Рекламные кампании» и «Аккаунт».</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-2.jpg" />
</section>
<section class="b-ac">
    <p class="m-docs-p">
        <xsl:text>Меню «Рекламные кампании» состоит из двух пунктов: </xsl:text>
        <xsl:text>«Видео» и «Статистика».</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-3.jpg" />
    <p class="m-docs-p">
        <xsl:text>Пункт «Видео» предназначен для непосредственной работы </xsl:text>
        <xsl:text>с рекламными кампаниями (создание, запуск, просмотр настроек </xsl:text>
        <xsl:text>или рекламного ролика, удаление кампании).</xsl:text>
        <xsl:text>Пункт «Статистика» предназначен для просмотра статистики</xsl:text>
        <xsl:text>по существующим рекламным кампаниям.</xsl:text>
    </p>
</section>
<section class="b-ac">
    <p class="m-docs-p">
        <xsl:text>Меню «Аккаунт» также состоит из двух пунктов: </xsl:text>
        <xsl:text>«Профиль» и «Выход».</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-4.jpg" />
    <p class="m-docs-p">
        <xsl:text>Выбрав пункт «Профиль», можно посмотреть профиль пользователя, </xsl:text>
        <xsl:text>под именем которого был осуществлен вход в личный кабинет. </xsl:text>
        <xsl:text>Выбрав пункт «Выход», можно выйти из личного кабинета. </xsl:text>
        <xsl:text>Также выйти из личного кабинета можно</xsl:text>
        <xsl:text>по нажатию кнопки зеленого цвета «Выйти из кабинета», </xsl:text>
        <xsl:text>расположенной в правом верхнем углу главного окна.</xsl:text>
    </p>
</section>
<section class="b-ac">
    <p class="m-docs-p">
        <xsl:text>По умолчанию, сразу после входа в личный кабинет (аутентификации) </xsl:text>
        <xsl:text>пользователь оказывается в окне «Видео», </xsl:text>
        <xsl:text>в главной таблице которого можно увидеть список всех</xsl:text>
        <xsl:text>существующих рекламных кампаний рекламодателя.</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-5.jpg" />
    <p class="m-docs-p">
        <xsl:text>Таблица состоит из семи колонок:</xsl:text>
    </p>
    <ul class="b-ac-ul m-docs-p">
        <li class="e-ac-ul">
            <xsl:text># – внутренний идентификатор рекламной кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Название – наименование рекламной кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Статус – статус кампании, показывающий разрешена она </xsl:text>
            <xsl:text>к запуску или нет (выставляется администратором сайта Tvzavr.ru);</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Состояние – текущее состояние рекламной кампании,  </xsl:text>
            <xsl:text>показывающее запущена она или остановлена;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Показов – количество показов рекламы  </xsl:text>
            <xsl:text>с начала запуска данной рекламной кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Кликов – количество кликов (переходов) по рекламе  </xsl:text>
            <xsl:text>на сайт рекламодателя с начала запуска данной рекламной кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Дата начала – дата начала данной рекламной кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Дата конца – дата окончания данной рекламной кампании.</xsl:text>
        </li>
    </ul>
</section>
</xsl:template>

<!--
    СОЗДАНИЕ НОВОЙ РЕКЛАМНОЙ КАМПАНИИ
-->
<xsl:template name="l-new">
<h3 class="b-ah m-ah-ht1">
    <xsl:text>Создание новой рекламной кампании</xsl:text>
</h3>
<section class="b-ac">
    <p class="m-docs-p">
        <xsl:text>Чтобы создать новую рекламную кампанию, необходимо нажать </xsl:text>
        <xsl:text>на расположенную на панели инструментов окна «Видео» кнопку «Создать».</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-6.jpg" />
</section>
<section class="b-ac" id="ht-common">
    <h4 class="b-ah m-ah-ht2">
        <xsl:text>Общая информация</xsl:text>
    </h4>
    <p class="m-docs-p">
        <xsl:text>Откроется окно создания «Общая информация», состоящее из четырех полей: </xsl:text>
        <xsl:text>Название, Комментарий, Дата начала и Дата конца.</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-7.jpg" />
    <ul class="b-ac-ul m-docs-p">
        <li class="e-ac-ul">
            <xsl:text>Название – вводится наименование рекламной кампании </xsl:text>
            <xsl:text>(обязательное для заполнения поле, не более 50 символов);</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Комментарий – в случае необходимости вводится комментарий </xsl:text>
            <xsl:text>к рекламной кампании, который будет виден только создателю этой кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Дата начала – вводится или выбирается при помощи кнопки выбора, </xsl:text>
            <xsl:text>расположенной с правого края поля, дата начала рекламной кампании (обязательное поле);</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Дата конца – вводится или выбирается при помощи кнопки выбора, </xsl:text>
            <xsl:text>расположенной с правого края поля, дата окончания рекламной кампании (обязательное поле);</xsl:text>
        </li>
    </ul>
    <p class="m-ht-next">
        <xsl:text>После заполнения всех необходимых полей, </xsl:text>
        <xsl:text>нажимается кнопка «Далее».</xsl:text>
    </p>
</section>
<section class="b-ac" id="ht-upload">
    <h4 class="b-ah m-ah-ht2">
        <xsl:text>Загрузка видео</xsl:text>
    </h4>
    <p class="m-docs-p">
        <xsl:text>Откроется окно создания «Загрузка видео», состоящее из трех обязательных </xsl:text>
        <xsl:text>для заполнения полей: Ссылка по клику, Текст ссылки, Файл.</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-8.jpg" />
    <ul class="b-ac-ul m-docs-p">
        <li class="e-ac-ul">
            <xsl:text>Ссылка по клику – вводится ссылка на сайт, на который перейдет зритель, </xsl:text>
            <xsl:text>кликнув по рекламному ролику (длина ссылки от 3 до 50 символов);</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Текст ссылки – текст, который всплывает, когда зритель наводит курсор </xsl:text>
            <xsl:text>на рекламный ролик (длина текста от 3 до 50 символов);;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Файл – при помощи расположенной справа от поля кнопки выбора </xsl:text>
            <xsl:text>здесь указывается видеофайл рекламного ролика </xsl:text>
            <xsl:text>(файлы могут иметь расширения: mp4, m4v, swf).</xsl:text>
        </li>
    </ul>
    <p class="m-ht-next">
        <xsl:text>После заполнения всех необходимых полей, </xsl:text>
        <xsl:text>нажимается кнопка «Далее».</xsl:text>
    </p>
</section>
<section class="b-ac" id="ht-show">
    <h4 class="b-ah m-ah-ht2">
        <xsl:text>Показ видео</xsl:text>
    </h4>
    <p class="m-docs-p">
    <xsl:text>Откроется окно создания «Показ видео», состоящее из трех разделов: </xsl:text>
    <xsl:text>Количество показов, Размещение ролика, Задержка повторного показа определенному зрителю.</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-10.jpg" />
    <ul class="b-ac-ul m-docs-p">
        <li class="e-ac-ul">
            <xsl:text>Количество показов – задается максимальное количество показов рекламного ролика;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Размещение ролика – задается место показа рекламного ролика; </xsl:text>
            <xsl:text>раздел состоит из трех флажков: Preroll (показывать перед началом фильма), </xsl:text>
            <xsl:text>Midroll (показывать внутри фильма), Postroll (показывать после окончания фильма); </xsl:text>
            <xsl:text>взводятся от 1 до 3 флажков;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Задержка повторного показа – здесь можно задавать частоту показа </xsl:text>
            <xsl:text>рекламного ролика одному и тому же зрителю; если флажок взведен, </xsl:text>
            <xsl:text>необходимо задать временной промежуток (количество часов и количество минут), </xsl:text>
            <xsl:text>в течение которого рекламный ролик не будет повторно показываться одному и тому же зрителю.</xsl:text>
        </li>
    </ul>
    <p class="m-ht-next">
        <xsl:text>После взведения всех необходимых флажков и заполнения полей, </xsl:text>
        <xsl:text>нажимается кнопка «Далее».</xsl:text>
    </p>
</section>
<section class="b-ac" id="ht-userstargeting">
    <h4 class="b-ah m-ah-ht2">
        <xsl:text>Таргетирование по зрителям</xsl:text>
    </h4>
    <p class="m-docs-p">
        <xsl:text>Откроется окно создания «Таргетирование по зрителям», </xsl:text>
        <xsl:text>состоящее из трех разделов: </xsl:text>
        <xsl:text>Пол, Возраст, Время показа.</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-11.jpg" />
    <ul class="b-ac-ul m-docs-p">
        <li class="e-ac-ul">
            <xsl:text>Пол – флажок взводится, если данная рекламная кампания </xsl:text>
            <xsl:text>рассчитана на аудиторию определенного пола; </xsl:text>
            <xsl:text>в этом случае в поле выбора, расположенном под флажком, </xsl:text>
            <xsl:text>выбирается определенный пол зрителей;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Возраст – флажок взводится, если данная рекламная кампания </xsl:text>
            <xsl:text>рассчитана на аудиторию определенного возраста; </xsl:text>
            <xsl:text>в этом случае в полях «От» и «До», </xsl:text>
            <xsl:text>расположенных под флажком, задается возрастной диапазон зрителей;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Время показа – флажок взводится, если данный рекламный </xsl:text>
            <xsl:text>ролик необходимо показывать только в определенное время суток; </xsl:text>
            <xsl:text>в этом случае в полях «От» и «До», </xsl:text>
            <xsl:text>расположенных под флажком, задается временной промежуток показа ролика.</xsl:text>
        </li>
    </ul>
    <p class="m-docs-p">
        <xsl:text>Если зритель не зарегистрирован на сайте (находится на нем анонимно), </xsl:text>
        <xsl:text>а настройки «Пол» и «Возраст» заданы (накладывают определенные ограничения), </xsl:text>
        <xsl:text>то для такого зрителя данная реклама показываться не будет </xsl:text>
        <xsl:text>(так как не известны его пол и возраст).</xsl:text>
    </p>
    <p class="m-ht-next">
        <xsl:text>После взведения всех необходимых флажков и заполнения полей,</xsl:text>
        <xsl:text>нажимается кнопка «Далее».</xsl:text>
    </p>
</section>
<section class="b-ac" id="ht-regiontargeting">
    <h4 class="b-ah m-ah-ht2">
        <xsl:text>Таргетирование по регионам</xsl:text>
    </h4>
    <p class="m-docs-p">
        <xsl:text>Откроется окно создания «Таргетирование по регионам», которое состоит </xsl:text>
        <xsl:text>из списка стран, на которые имеется возможность демонстрировать рекламный ролик.</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-12.jpg" />
    <p class="m-docs-p">
        <xsl:text>Необходимо взвести флажки, расположенные слева от названия тех стран, </xsl:text>
        <xsl:text>в которых будет проводиться данная рекламная кампания.</xsl:text>
        <xsl:text>Если данная реклама должна показываться не во всем регионе, </xsl:text>
        <xsl:text>а только в определенных городах, необходимо двойным кликом левой кнопкой </xsl:text>
        <xsl:text>мыши по региону раскрыть список расположенных в нем городов.</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-13.jpg" />
    <p class="m-docs-p">
        <xsl:text>После этого необходимо взвести флажки, </xsl:text>
        <xsl:text>расположенные слева от названия тех городов, </xsl:text>
        <xsl:text>в которых будет проводиться данная рекламная кампания.</xsl:text>
    </p>
    <p class="m-ht-next">
        <xsl:text>После взведения всех необходимых флажков, нажимается кнопка «Далее».</xsl:text>
    </p>
</section>
<section class="b-ac" id="ht-categorytargeting">
    <h4 class="b-ah m-ah-ht2">
        <xsl:text>Таргетирование по категориям</xsl:text>
    </h4>
    <p class="m-docs-p">
        <xsl:text>Откроется окно создания «Таргетирование по категориям», состоящее из двух таблиц:</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-14.jpg" />
    <p class="m-docs-p">
        <xsl:text>В левой таблице расположены все жанры (категории) фильмов, </xsl:text>
        <xsl:text>размещенных на Tvzavr.ru. </xsl:text>
        <xsl:text>Те категории, в которых должен демонстрироваться рекламный ролик,</xsl:text>
        <xsl:text>необходимо перенести в правую таблицу.</xsl:text>
        <xsl:text>Для этого необходимо:</xsl:text>
    </p>
    <ol class="b-ac-ul m-docs-p">
        <li class="e-ac-ul">
            <xsl:text>Взвести флажки, расположенные слева от тех категорий, </xsl:text>
            <xsl:text>в которых должен демонстрироваться рекламный ролик.  </xsl:text>
            <xsl:text>Для поиска нужных категорий можно воспользоваться расположенным  </xsl:text>
            <xsl:text>под названием окна полем поиска.  </xsl:text>
            <xsl:text>При введении в это поле начальных букв названия категорий или поисковой фразы,  </xsl:text>
            <xsl:text>в списке останутся только те категории, которые удовлетворяют введенному условию поиска.</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Нажать на кнопку «+», расположенную в центре окна. </xsl:text>
            <xsl:text>Категории, отмеченные флажками, переместятся в правую таблицу.  </xsl:text>
            <xsl:text>Рекламный ролик будет демонстрироваться во всех фильмах, </xsl:text>
            <xsl:text>принадлежащих категориям, перенесенным в правую таблицу.</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Если необходимо убрать какую-то категорию из правой таблицы, </xsl:text>
            <xsl:text>необходимо взвести флажок, расположенный слева от нее, и нажать на кнопку «–».</xsl:text>
        </li>
    </ol>
    <p class="m-ht-next">
        <xsl:text>После того, как будут заданы все необходимые категории, </xsl:text>
        <xsl:text>по нажатию кнопки «Послать!» созданная рекламная кампания </xsl:text>
        <xsl:text>добавится в список уже существующих кампаний.</xsl:text>
    </p>
</section>
<section class="b-ac m-docs-p" id="new-end">
    <xsl:text>На этом создание новой рекламной кампании завершено.</xsl:text>
</section>
</xsl:template>


<!--
    ПРОСМОТР НАСТРОЕК РЕКЛАМНОЙ КАМПАНИИ
-->
<xsl:template name="l-view">
<h3 class="b-ah m-ah-ht1">
    <xsl:text>Просмотр настроек рекламной кампании</xsl:text>
</h3>
<section class="b-ac">
    <p class="m-docs-p">
        <xsl:text>Чтобы просмотреть настройки уже созданной рекламной кампании, </xsl:text>
        <xsl:text>необходимо дважды кликнуть по ней левой кнопкой мыши. </xsl:text>
    </p>
    <p class="m-docs-p">
        <xsl:text>Откроется окно «Информация», разделенное на два части:</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-15.jpg" />
    <p class="m-docs-p">
        <xsl:text>В левой части окна можно увидеть настройки данной рекламной </xsl:text>
        <xsl:text>кампании (название, дату начала и конца, URL ролика, таргетирование и др). </xsl:text>
        <xsl:text>В правой части окна, нажав кнопку «Play», можно просмотреть рекламный ролик.</xsl:text>
    </p>
    <p class="m-docs-p">
        <xsl:text>Чтобы выйти из режима просмотра, необходимо нажать на кнопку «Назад», </xsl:text>
        <xsl:text>расположенную над окном «Информация».</xsl:text>
    </p>
</section>
</xsl:template>

<!--
    ЗАПУСК/ОСТАНОВКА РЕКЛАМНОЙ КАМПАНИИ
-->
<xsl:template name="l-start-stop">
<h3 class="b-ah m-ah-ht1">
    <xsl:text>Запуск/остановка рекламной кампании</xsl:text>
</h3>
<section class="b-ac">
    <p class="m-docs-p">
        <xsl:text>После того, как рекламная кампания создана, и ее статус установлен </xsl:text>
        <xsl:text>администратором сайта в значение «Разрешена» (поле «Статус»), </xsl:text>
        <xsl:text>можно эту кампанию запускать.</xsl:text>
        <xsl:text>Для этого необходимо установить курсор </xsl:text>
        <xsl:text>на ту рекламную кампанию, которую требуется запустить, </xsl:text>
        <xsl:text>и нажать на расположенную на панели инструментов </xsl:text>
        <xsl:text>окна «Видео» кнопку «Запустить».</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-16.jpg" />
    <p class="m-docs-p">
        <xsl:text>Поле «Состояние» данной рекламной кампании примет значение «Запущена».</xsl:text>
    </p>
    <p class="m-docs-p">
        <xsl:text>Чтобы остановить (приостановить) данную рекламную кампанию, </xsl:text>
        <xsl:text>необходимо установить на нее курсор и нажать на кнопку «Остановить», </xsl:text>
        <xsl:text>расположенную на панели инструментов окна «Видео» справа </xsl:text>
        <xsl:text>от кнопки «Запустить».</xsl:text>
    </p>
</section>
</xsl:template>

<!--
    УДАЛЕНИЕ РЕКЛАМНОЙ КАМПАНИИ
-->
<xsl:template name="l-delete">
<h3 class="b-ah m-ah-ht1">
    <xsl:text>Удаление рекламной кампании</xsl:text>
</h3>
<section class="b-ac">
    <p class="m-docs-p">
        <xsl:text>Чтобы удалить рекламную кампанию, необходимо установить </xsl:text>
        <xsl:text>на нее курсор и нажать на расположенную </xsl:text>
        <xsl:text>на панели инструментов окна «Видео» кнопку «Удалить». </xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-17.jpg" />
</section>
</xsl:template>

<!--
    ПРОСМОТР СТАТИСТИКИ ПО РЕКЛАМНЫМ КАМПАНИЯМ
-->
<xsl:template name="l-stat">
<h3 class="b-ah m-ah-ht1">
    <xsl:text>Просмотр статистики по рекламным кампаниям</xsl:text>
</h3>
<section class="b-ac">
    <p class="m-docs-p">
        <xsl:text>Чтобы просмотреть статистику по существующим </xsl:text>
        <xsl:text>рекламным кампаниям (идущим или остановленным), </xsl:text>
        <xsl:text>необходимо в меню «Рекламные кампании» выбрать пункт «Статистика».</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht" src="/i/ht-18.jpg" />
    <p class="m-docs-p">
        <xsl:text>Откроется главное окно для просмотра статистики </xsl:text>
        <xsl:text>по существующим рекламным кампаниям «Статистика»:</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-19.jpg" />
    <p class="m-docs-p">
        <xsl:text>Статистика по рекламным кампаниям формируется за определенный период, </xsl:text>
        <xsl:text>поэтому, чтобы ее получить, необходимо:</xsl:text>
    </p>
    <ol class="b-ac-ul m-docs-p">
        <li class="e-ac-ul">
            <xsl:text>в полях, расположенных над главной таблицей окна «Статистика», </xsl:text>
            <xsl:text>ввести или выбрать при помощи кнопок выбора, </xsl:text>
            <xsl:text>расположенных с правого края этих полей, </xsl:text>
            <xsl:text>даты начала и конца периода, </xsl:text>
            <xsl:text>за который требуется получить статистику;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>нажать на кнопку «Отправить», </xsl:text>
            <xsl:text>расположенную справа от поля даты конца периода.</xsl:text>
        </li>
    </ol>
    <p class="m-docs-p">
        <xsl:text>В главной таблице окна «Статистика» </xsl:text>
        <xsl:text>появится список рекламных кампаний, </xsl:text>
        <xsl:text>проводившихся или проводящихся в указанный период:</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-20.jpg" />
    <p class="m-docs-p">
        <xsl:text>Главная таблица состоит из восьми колонок:</xsl:text>
    </p>
    <ol class="b-ac-ul m-docs-p">
        <li class="e-ac-ul">
            <xsl:text># – внутренний идентификатор рекламной кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Название – наименование рекламной кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Дата начала – дата начала данной рекламной кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Дата конца – дата окончания данной рекламной кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Показы за период – количество показов </xsl:text>
            <xsl:text>рекламы за указанный период;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Всего показов – общее количество показов рекламы </xsl:text>
            <xsl:text>с начала запуска данной рекламной кампании;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Клики за период – количество переходов (кликов) </xsl:text>
            <xsl:text>на сайт рекламодателя за указанный период;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Всего кликов – общее количество переходов (кликов) на сайт </xsl:text>
            <xsl:text>рекламодателя с начала запуска данной рекламной кампании.</xsl:text>
        </li>
    </ol>
    <p class="m-docs-p">
        <xsl:text>Если требуется получить статистику по какой-то рекламной </xsl:text>
        <xsl:text>кампании в разрезе фильмов, необходимо установить </xsl:text>
        <xsl:text>на нее курсор и нажать на расположенную на панели </xsl:text>
        <xsl:text>инструментов окна «Статистика» кнопку «Подробнее».</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-21.jpg" />
    <p class="m-docs-p">
        <xsl:text>В окне «Статистика» откроется новая таблица, </xsl:text>
        <xsl:text>содержащая информацию о том, </xsl:text>
        <xsl:text>в каких именно фильмах была показана данная реклама:</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-22.jpg" />
    <p class="m-docs-p">
        <xsl:text>Таблица состоит из четырех колонок:</xsl:text>
    </p>
    <ol class="b-ac-ul m-docs-p">
        <li class="e-ac-ul">
            <xsl:text>Урл – URL фильма, в котором был осуществлен показ рекламы;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Название – название фильма, в котором был осуществлен показ рекламы;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Показан – количество показов рекламы в данном фильме;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Клики – количество переходов (кликов) на сайт рекламодателя, </xsl:text>
            <xsl:text>произведенных из данного фильма.</xsl:text>
        </li>
    </ol>
    <p class="m-docs-p">
        <xsl:text>В этом окне так же имеется возможность задать новый период </xsl:text>
        <xsl:text>и сформировать отчет заново по той рекламной кампании, </xsl:text>
        <xsl:text>которая была выбрана на предыдущем уровне.</xsl:text>
    </p>
    <p class="m-docs-p">
        <xsl:text>Чтобы вернуться на предыдущий уровень (в предыдущую таблицу), </xsl:text>
        <xsl:text>необходимо нажать на расположенную </xsl:text>
        <xsl:text>на панели инструментов окна «Статистика» кнопку «Назад».</xsl:text>
    </p>
    <p class="m-docs-p">
        <xsl:text>Если требуется получить более подробную статистику, </xsl:text>
        <xsl:text>касающуюся показа рекламы в конкретном фильме, </xsl:text>
        <xsl:text>необходимо установить на него курсор и нажать на расположенную </xsl:text>
        <xsl:text>на панели инструментов окна «Статистика» кнопку «Подробнее».</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-23.jpg" />
    <p class="m-docs-p">
        <xsl:text>В окне «Статистика» откроется новая таблица:</xsl:text>
    </p>
    <img class="e-docs-i m-docs-iht m-docs-ib" src="/i/ht-24.jpg" />
    <p class="m-docs-p">
        <xsl:text>Таблица состоит из пяти колонок:</xsl:text>
    </p>
    <ol class="b-ac-ul m-docs-p">
        <li class="e-ac-ul">
            <xsl:text># – внутренний идентификатор записи;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>ip – ip-адрес, с которого просматривалась реклама;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Время начала – дата и время начала просмотра рекламы;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Время конца – дата и время окончания просмотра рекламы;</xsl:text>
        </li>
        <li class="e-ac-ul">
            <xsl:text>Дата клика – дата и время перехода (клика) </xsl:text>
            <xsl:text>на сайт рекламодателя, если таковой был.</xsl:text>
        </li>
    </ol>
    <p class="m-docs-p">
        <xsl:text>В этом окне так же имеется возможность задать новый период </xsl:text>
        <xsl:text>и сформировать отчет заново по тому фильму, </xsl:text>
        <xsl:text>который был выбран на предыдущем уровне.</xsl:text>
    </p>
    <p class="m-docs-p">
        <xsl:text>Чтобы вернуться на предыдущий уровень (в предыдущую таблицу), </xsl:text>
        <xsl:text>необходимо нажать на расположенную </xsl:text>
        <xsl:text>на панели инструментов окна «Статистика» кнопку «Назад».</xsl:text>
    </p>
</section>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>
