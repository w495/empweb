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

<xsl:template name="game-container">
    <div id="game-top">&nbsp;</div> 
    <div id="game-body" >
        <a id="game-body-link" href="/Game/Index/">
            <img src="/static/site-media/images/defaults/right-banner.png" id="game-body-img" height="398px" width="238px" />
        </a> 
    </div>
</xsl:template>

</xsl:stylesheet> 
