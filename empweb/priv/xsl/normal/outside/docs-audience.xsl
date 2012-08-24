<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/docs-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />


<xsl:template name="local-name">
    <xsl:text>Aудитория</xsl:text>
</xsl:template>

<xsl:template name="s-title-docs">
    <xsl:call-template name="local-name"/>
</xsl:template>

<xsl:template name="e-docs-head">
    <xsl:call-template name="local-name"/>
</xsl:template>

<!--
    Быдлогод.
    Но для макета сгодиться.
    Всего скорее надо переверстывать.

    <br/> ==>  <p>*</p>
-->

<xsl:template name="s-about-docs">
    <article class="b-a"> 
        <div class="b-ac">
            <p class="b-ac-p" >  
            	<p class="b-ac-p">  
					Ежемесячная аудитория TVzavr.ru составляет более 2 млн посетителей, ежедневно ресурс посещают более 120.000 человек.
                </p>
                <p class="b-ac-p"> 
                    Более 100.000 зарегистрированных пользователей, активно участвующих в проводимых порталом различного рода мероприятиях, и это число увеличивается ежедневно. 
                </p>
                <p class="b-ac-p">
                    Ядро аудитории TVzavr.ru - люди с активной жизненной позицией и современными взглядами, умеющие оценить по достоинству качество предлагаемого продукта и  воспользоваться современными технологиями для его приобретения.                   
                </p>
                <p class="b-ac-p">
                    Структура (данные TNS Web Index 03/2012):
				</p>
				<br />
				<p class="b-ac-p m-center">
                    <span style="display: inline-block; width: 50%; text-align:center;">Пол аудитории</span>
                    <span style="display: inline-block; width: 50%; text-align:center;">Возрастные категории</span>
				</p>
				<br />
				<p class="m-center"> 
                    <img src="/i/gender.png" width="477px" /> 
                    <img src="/i/age.png" width="477px" />
				</p>
				<p class="b-ac-p">
					Более подробные характеристики аудитории TVzavr.ru смотрите в <a href="/d/tvzavr_presentation.pptx">Презентации TVzavr.ru</a>
				</p>    
            </p> 
        </div>
    </article> 
     
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>
