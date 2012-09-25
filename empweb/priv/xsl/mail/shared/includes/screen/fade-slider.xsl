<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp "&#160;">
    <!ENTITY raquo  "&#187;">
    <!ENTITY laquo  "&#171;">
    <!ENTITY bull     "&#8226;"> 
]>

<!--
    Самая верхняя строка с кнопками и поиском.
    Всего скорее будет одинакова для всех страниц сайта.
-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >
    
<xsl:template name="fade-slider">
    <div id="slider">
        <ul class="slider-items">
            <li class="active">
                <a href="/Transport/Surface/">
                    <img src="/static/site-media/images/fade-slider/slide1.png" />
                    <span class="slider-item-capture"><xsl:text>Наземный транспорт</xsl:text></span>
                </a>
            </li>
            <li >
                <a href="/Transport/Air/">
                    <img src="/static/site-media/images/fade-slider/slide2.png" />
                    <span class="slider-item-capture"><xsl:text>Воздушный транспорт</xsl:text></span>
                </a>
            </li>        
            <li >
                <a href="/Transport/Water/">
                    <img src="/static/site-media/images/fade-slider/slide3.png" />
                    <span class="slider-item-capture"><xsl:text>Водный транспорт</xsl:text></span>
                </a>
            </li>
        </ul>
        <div class="slider-bar">
            <a href="javascript:void(0);" id="slider-prev" class="slider-button">
                <xsl:text>&laquo;</xsl:text>
            </a>
            <span class="slider-circle">
                <span><xsl:text>&bull;</xsl:text></span>
                <span class="slider-circle-c"><xsl:text>&bull;</xsl:text></span>
                <span><xsl:text>&bull;</xsl:text></span>
            </span>
            <a href="javascript:void(0);" id="slider-next" class="slider-button">
                <xsl:text>&raquo;</xsl:text>
            </a>
        </div>
    </div>
</xsl:template>


</xsl:stylesheet> 
