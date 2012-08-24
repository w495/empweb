<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp "&#160;">
    <!ENTITY raquo  "&#187;">
    <!ENTITY laquo  "&#171;">
]>

<!--
    Самая верхняя строка с кнопками и поиском.
    Всего скорее будет одинакова для всех страниц сайта.
-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >

<xsl:template name="top-line-container">
    <div id="top-line-choose" >
        <div id="goto-normal-version">
            <span>← </span>
            <a href="/Home/Spec/Off{/data/meta/current-path}" >Обычная версия сайта</a>
        </div>
        <div id="choose-spec-type">
            <xsl:variable name="isBig"><xsl:if test="/data/meta/spec-variant = 'big'">current</xsl:if></xsl:variable>
            <xsl:variable name="isMedium"><xsl:if test="/data/meta/spec-variant = 'medium'">current</xsl:if></xsl:variable>
            <xsl:variable name="isSmall"><xsl:if test="/data/meta/spec-variant = 'small'">current</xsl:if></xsl:variable>
            <span><h6 class="spec-type-header">Размер шрифта:</h6>&nbsp;</span>            
            <ul id="choose-spec-variant">
                <li class="{$isBig}">
                    <a id="choose-spec-type-big" href="/Home/Spec/Big{/data/meta/current-path}" title="Большой вариант">
                        <xsl:text>A</xsl:text>
                        <span class="unvisible" >Большой вариант</span>
                    </a>
                </li>
                <li class="{$isMedium}" >
                    <a id="choose-spec-type-medium" href="/Home/Spec/Medium{/data/meta/current-path}" title="Средний вариант" >
                        <xsl:text>A</xsl:text>
                        <span class="unvisible" >Средний вариант</span>
                    </a>
                </li>
                <li class="{$isSmall}">
                    <a id="choose-spec-type-small" href="/Home/Spec/Small{/data/meta/current-path}" title="Маленький вариант" >
                        <xsl:text>A</xsl:text>
                        <span class="unvisible" >Маленький вариант</span>
                    </a>
                </li>
            </ul>
            <xsl:variable name="isBlack"><xsl:if test="/data/meta/spec-color = 'black'">current</xsl:if></xsl:variable>
            <xsl:variable name="isBlue"><xsl:if test="/data/meta/spec-color = 'blue'">current</xsl:if></xsl:variable>
            <xsl:variable name="isWhite"><xsl:if test="/data/meta/spec-color = 'white'">current</xsl:if></xsl:variable>
            <span><h6 class="spec-type-header" >Цвет сайта:</h6>&nbsp;</span>
            <ul id="choose-spec-color">
                <li class="{$isBlack}">
                    <a id="choose-spec-type-black" href="/Home/Spec/Black{/data/meta/current-path}" title="Черный вариант">
                        <span class="unvisible" >Черный вариант</span>
                    </a>
                </li>
                <li class="{$isBlue}" >
                    <a id="choose-spec-type-blue" href="/Home/Spec/Blue{/data/meta/current-path}" title="Синий вариант" >
                        <span class="unvisible" >Синий вариант</span>
                    </a>
                </li>
                <li class="{$isWhite}">
                    <a id="choose-spec-type-white" href="/Home/Spec/White{/data/meta/current-path}" title="Белый вариант" >
                        <span class="unvisible" >Белый вариант</span>
                    </a>
                </li>
            </ul>
        </div>
    </div>
    <div id="top-line-search-holder">
        <div id="top-line-search">             
            <form action="/Search/" method="POST" >
                <input id="search-field" type="text" size="32"  name="q" placeholder="Введите поисковый запрос" />
                <input id="search-submitter"  class="relief"  type="submit"  value="искать" />
            </form>
        </div>
        <a href="/Contacts/List/" id="contacts-link" class="relief" title="контакты">
            <xsl:text>контакты</xsl:text>
        </a>
        <a href="/Hotline/Index/" id="contacts-link" class="relief" title="контакты">
            <xsl:text>горячая линия</xsl:text>
        </a>
    </div>
</xsl:template>

</xsl:stylesheet> 
