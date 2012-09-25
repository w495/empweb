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
    <div id="top-line-links">
        <a href="/Home/Spec/On{/data/meta/current-path}" id="visually-impaired-version" title="Версия для слабовидящих">&nbsp;</a>
        <a href="/Hotline/Index/" id="phone-link" title="горячая линия">&nbsp;</a>
        <a href="/Contacts/List/" id="e-mail-link" title="контакты">&nbsp;</a>
        <a href="/Modile/" title="мобильная версия" >мобильная версия</a>
    </div>
    <div id="top-line-search"> 
        <a href="#" id="search-link" title="Поиск">&nbsp;</a>
        <form action="#" method="GET" >
            <input id="search-field" type="text" size="32"  placeholder="Введите поисковый запрос" />
            <input id="search-submitter"  type="button"  value="искать" />
        </form>
    </div>
</xsl:template>


</xsl:stylesheet> 
