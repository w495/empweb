<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/docs-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="local-name">
    <xsl:text>Контент</xsl:text>
</xsl:template>

<xsl:template name="s-title-docs">
    <xsl:call-template name="local-name"/>
</xsl:template>

<xsl:template name="e-docs-head">
    <xsl:call-template name="local-name"/>
</xsl:template>


<xsl:template name="s-about-docs">
    <article class="b-a">
        <div class="b-ac">
            <p class="b-ac-p" >
                <xsl:text>Более 10 000 часов видео </xsl:text>
            </p>
            <p class="b-ac-p" >
                <xsl:text>Самая крупная в Рунете коллекция фильмов, ставших призерами Каннского, Берлинского, Венецианского и других фестивалей. </xsl:text>
            </p> 
            <p class="b-ac-p" >
                <xsl:text>10 основных тематических рубрик: </xsl:text>
        	<img src="/i/au-0.jpg" style="width:650px; float:right;" />
            </p>
            <ul class="b-ac-ul">
                <li class="e-ac-ul">
                    <xsl:text>HD-фильмы</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Художественные фильмы </xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Сериалы</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Документальные фильмы</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Мультфильмы</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Юмор</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Спорт</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Музыка</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Видеоуроки</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Каналы (сформированные плэйлисты по тематикам) </xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Все рубрики классифицированы по жанрам</xsl:text>
                </li> 
            </ul>
        </div>
    </article>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>
