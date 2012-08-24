<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/docs-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<!--
    Совсем по-хорошему всю
    документацию надо переверстывать.
-->

<xsl:template name="local-name">
    <xsl:text>Контактная информация</xsl:text>
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
            <img src="/i/dc-0.jpg" class="m-f-r" />
    <br />   
    <br />   
    <br />
    <br />    
            <h3 class="b-ah">
                <xsl:text>ООО «ТиВиЗавр»</xsl:text>
            </h3>
            <p class="b-ac-p">    
                <p>Москва, ул. Докукина, 8</p> 
                <p> 
                    Сайт: <a href="http://www.tvzavr.ru">www.TVzavr.ru</a>
                </p> 
                <p> 
                    <a href="#" onclick="ContactUs()">Отправить нам сообщение</a>
                </p> 
            </p>   
        </div>
    </article> 
    <article class="b-a">   
        <div class="b-ac"> 
	        <h3 class="b-ah">
				<xsl:text>Отдел рекламы</xsl:text>
	        </h3>
        	<p class="b-ac-p">  
				<p>
					Руководитель - Елена Анохина
				</p>
				<p>
					E-mail: <a href="mailto:anokhina.e@tvzavr.ru">anokhina.e@tvzavr.ru</a>
				</p>  
				<p>
					Тел.: +7 (495) 640-13-97
				</p>
				<p>
					Моб.: +7 (926) 606-97-96
				</p> 
        	</p>    
        </div>
    </article>   
</xsl:template>
 

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>
