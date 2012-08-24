<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/docs-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="local-name">
    <xsl:text>Прайс-лист</xsl:text>
</xsl:template>

<xsl:template name="s-title-docs">
    <xsl:call-template name="local-name"/>
</xsl:template>

<xsl:template name="e-docs-head">
    <xsl:call-template name="local-name"/>
</xsl:template>


<!--
    Быдлогод.
    Но для макета сгодится.
    Всего скорее надо переверстывать.

    <br/> ==>  <p class="m-docs-p">*</p>
-->

<xsl:template name="s-about-docs">
    <article class="b-a" id="offer-video">
        <xsl:call-template name="l-video"/>
    </article>
    <article class="b-a" id="offer-over">
        <xsl:call-template name="l-over"/>
    </article>
</xsl:template>

<!--
    ВИДЕО-РЕКЛАМА
-->
<xsl:template name="l-video">
<table class="b-ac m-docs-t" width="100%" cellspacing="0" cellpadding="0">
    <caption class="b-ah m-ah-ht1">
        <xsl:text>Видео-реклама</xsl:text>
    </caption>
    <tbody>
        <tr class="m-docs-ttrh">
            <th scope="col" width="150px">
                <p>
                    <xsl:text>Формат</xsl:text>
                </p>
            </th>
            <th scope="col" width="50%">
                <p>
                    <xsl:text>Описание</xsl:text>
                </p>
            </th>
            <th scope="col" width="">
                <p>
                    <xsl:text>Стоимость</xsl:text>
                </p>
                <p>
                    <xsl:text>(без учета НДС)</xsl:text>
                </p>
            </th>
            <th scope="col" width="">
                <p>
                    <xsl:text>Трафик в неделю/</xsl:text>
                </p>
                <p>
                    <xsl:text>тыс. показов</xsl:text>
                </p>
            </th>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row">
                <xsl:text>Pre-roll</xsl:text>
            </th>
            <td>
                <xsl:text>рекламный ролик перед показом видео (до 30 сек)</xsl:text>
            </td>
            <td>
                <xsl:text>700 руб/</xsl:text>
                <br/>
                <xsl:text>тыс. показов</xsl:text>
            </td>
            <td>800</td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row" >
                <xsl:text>Post-roll</xsl:text>
            </th>
            <td>
                <xsl:text>рекламный ролик после  показа видео</xsl:text>
            </td>
            <td>
                <xsl:text>400 руб/</xsl:text>
                <br/>
                <xsl:text>тыс. показов</xsl:text>
            </td>
            <td>
                <xsl:text>300</xsl:text>
            </td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row" >
                <xsl:text>Mid-roll</xsl:text>
            </th>
            <td>
                <xsl:text>рекламный ролик в середине просматриваемого видео</xsl:text>
            </td>
            <td>
                <xsl:text>500 руб/</xsl:text>
                <br/>
                <xsl:text>тыс. показов</xsl:text>
            </td>
            <td>
                <xsl:text>700</xsl:text>
            </td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row">
                <xsl:text>Pause-roll</xsl:text>
            </th>
            <td>
                <xsl:text>Рекламный баннер при нажатии кнопки «Pause» во время просмотра видео</xsl:text>
            </td>
            <td>
                <xsl:text>300 руб/</xsl:text><br/><xsl:text>тыс. показов</xsl:text>
            </td>
            <td>
                <xsl:text>400</xsl:text>
            </td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row" >
                <xsl:text>Вирусное видео</xsl:text>
            </th>
            <td>
                <xsl:text>Ролик рекламодателя на главной странице в  одном из окон раздела «Рекомендуемые»</xsl:text>
            </td>
            <td>
                <xsl:text>20.000 руб/сутки</xsl:text>
            </td>
            <td>
                <xsl:text>-</xsl:text>
            </td>
        </tr>
    </tbody>
</table>
   
<br />
<br />
<h3 class="b-ah">
    <xsl:text>Баннерная реклама</xsl:text>
</h3> 
<br />
<table class="b-ac m-docs-t" width="100%" cellspacing="0" cellpadding="0">
    <tr class="m-docs-ttrc">
        <td width="150px">Формат</td>
        <td>Расположение</td>
        <td width="144px">Стоимость (без НДС)</td>
        <td width="150px">Трафик в неделю/тыс. показов</td>
    </tr>
    <tr class="m-docs-ttrc">
        <td>240х400</td>
        <td>Первый экран, правая колонка, все страницы сайта</td>
        <td>400 руб./ тыс. показов</td>
        <td>2500</td>
    </tr>
    <tr class="m-docs-ttrc">
        <td>728х90</td>
        <td>Второй экран, середина,            все страницы сайта</td>
        <td>300 руб./ тыс. показов</td>
        <td>2000</td>
    </tr>
</table>
   
<br />
<br />
<h3 class="b-ah">
    <xsl:text>Спецпроекты</xsl:text>
</h3> 
<br />
<table class="b-ac m-docs-t" width="100%" cellspacing="0" cellpadding="0">
    <tr class="m-docs-ttrc">
        <td class="m-bold" width="150px">Брендирование подложки главной страницы</td>
        <td>Оформление подложки в первом экране главной страницы в желаемом дизайне рекламодателя</td> 
    </tr>
    <tr class="m-docs-ttrc" height="51px">
        <td class="m-bold" >Брендирование плеера </td>
        <td>Оформление плеера по желанию клиента (например, плеер в форме дисплея ноутбука и т.п.)</td> 
    </tr>
    <tr class="m-docs-ttrc">
        <td class="m-bold">«Спонсорский канал»</td>
        <td>Формирование списка-play-листа фильмов, выбранных клиентом в соответствии с тематикой их деятельности, с брендированием задействованных в проекте страниц</td> 
    </tr>
    <tr class="m-docs-ttrc">
        <td class="m-bold">Авторский спецпроект</td>
        <td>Разработка идеи и концепции продвижения продукта рекламодателя на сайте TVzavr.ru, используя контент, дизайн и рекламные возможности</td> 
    </tr>
</table>
<br />
<br />
<h3 class="b-ah">
    <xsl:text>Наценки</xsl:text>
</h3>  
<br />
<table class="b-ac m-docs-t" width="100%" cellspacing="0" cellpadding="0"> 
    <tbody>
        <tr class="m-docs-ttrh">
            <th scope="col" width="150px"  >
                <xsl:text>Таргетинг</xsl:text>
            </th>
            <th scope="col" >
                <xsl:text>Наценка</xsl:text>
            </th>
            <th scope="col" >
                <xsl:text>Комментарий</xsl:text>
            </th>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row">
                <xsl:text>Частота показов</xsl:text>
            </th>
            <td>
                <xsl:text>20%</xsl:text>
            </td>
            <td>
                <xsl:text>При настройке количества показоврекламы одному пользователю в сутки</xsl:text>
            </td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row" >
                <xsl:text>Гео-таргетинг</xsl:text>
            </th>
            <td>
                <xsl:text>20%</xsl:text>
            </td>
            <td>
                <xsl:text>При настройке показа рекламы в определённых городах России</xsl:text>
            </td>
        </tr>
        <tr class="m-docs-ttrc">
            <th scope="row" >
                <xsl:text>Rich-media</xsl:text>
            </th>
            <td>
                <xsl:text>50%</xsl:text>
            </td>
            <td>
                <xsl:text>При заказе форматов рекламы с использованием RichMedia технологии</xsl:text>
            </td>
        </tr>
    </tbody>
</table>
</xsl:template>

<!--
    Наценки
-->

<xsl:template name="l-over">
<p class="m-bold">
Минимальный заказ – 10.000 руб. без НДС
</p>
</xsl:template>



<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>
