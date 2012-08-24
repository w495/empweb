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
    
<xsl:template name="footer-container">
        <div id="footer-wraper">
            <div id="copyright" >
                <p>
                    <a href="#">
                        <xsl:text disable-output-escaping="yes">Создание сайта</xsl:text>
                    </a>
                    <span>
                        <xsl:text disable-output-escaping="yes">&nbsp;Copyright &laquo;МЧС России&raquo;, 2011</xsl:text>
                    </span>
                </p>	
                <p>
                    <xsl:text disable-output-escaping="yes">Все материалы находящиеся на&nbsp;сайте, охраняются в&nbsp;соответсвии с&nbsp;законодательством РФ.</xsl:text>
                </p>	
                <p>
                    <xsl:text disable-output-escaping="yes">При полном или частичном копировании материалов ссылка на&nbsp;mchs.gov.ru обязательна.</xsl:text>
                </p>	
            </div>
            <div id="footer-icons">
                <a href="/Home/Index/" id="footer-home">&nbsp;</a>
                <!-- 
                    <a href="/TEST/" id="footer-map">&nbsp;</a>
                -->
            </div>
        </div>
</xsl:template>

</xsl:stylesheet> 
