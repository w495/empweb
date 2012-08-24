<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp "&#160;">
    <!ENTITY raquo  "&#187;">
    <!ENTITY laquo  "&#171;">
]>

<!--
    Верхнее меню с кнопками
    Всего скорее будет одинакова для всех страниц сайта.
-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >
    
<xsl:template name="menu-container">
        <ul id="menu">
            <li>
                <a href="/Regulatory/List/">
                    <xsl:text disable-output-escaping="yes">Нормативно-правовые документы</xsl:text>
                </a>
            </li>
            <li>
                <a href="/Secure/List/">
                    <xsl:text disable-output-escaping="yes">Безопасность на транспорте</xsl:text>
                </a>
            </li>
            <li>
                <a href="/Terms/List/">
                    <xsl:text disable-output-escaping="yes">Термины</xsl:text>
                </a>
            </li>
            <li>
                <a href="/Tests/List/">
                    <xsl:text disable-output-escaping="yes">Онлайн-тестирование</xsl:text>
                </a>
            </li>
            <li>
                <a href="/Conf/List/">
                    <xsl:text disable-output-escaping="yes">Онлайн-конференции</xsl:text>
                </a>
            </li>
        </ul>
</xsl:template>


</xsl:stylesheet> 
