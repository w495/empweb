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
    <xsl:text>Размещение рекламы</xsl:text>
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
        	<p class="b-ac-p">  
				<xsl:text>Мы размещаем рекламу, ориентированную на конечного потребителя –  посетителя TVzavr.ru и его семью.</xsl:text>
        	</p>   
        	<p class="b-ac-p">  
				<xsl:text>Посетители нашего ресурса являются целевой аудиторией для многих компаний, как оказывающих финансовые, телекоммуникационные, образовательные и развлекательные услуги, так и предлагающих различные товары              средней ценовой категории.</xsl:text> 
			</p>    
        </div>
    </article> 
    <article class="b-a">
        <h3 class="b-ah">
			<xsl:text>Потенциальные и ожидаемые рекламодатели </xsl:text>
        </h3>
        <div class="b-ac"> 
            <ul class="b-ac-ul">
                <li class="e-ac-ul">
 					<xsl:text>
                        Реклама на TVzavr.ru ориентирована на конечного потребителя –  посетителя TVzavr.ru и его семью.
                    </xsl:text>
                </li>   
                <li class="e-ac-ul">
                    <xsl:text>
                        Посетители нашего ресурса являются целевой аудиторией для многих компаний, как оказывающих финансовые, телекоммуникационные, образовательные и развлекательные услуги, так и предлагающих различные товары средней ценовой категории.
                    </xsl:text>
                </li>    
            </ul>
        </div>
    </article> 
    <article class="b-a">
        <h3 class="b-ah">
			Не принимаются к рекламе 
        </h3>
        <div class="b-ac"> 
            <ul class="b-ac-ul">
                <li class="e-ac-ul"> 
                	Материалы эротического характера 
                </li>
                <li class="e-ac-ul">
 					Материалы с заведомо ложной информацией
                </li>
                <li class="e-ac-ul">
 					Материалы, сделанные вопреки Закону о рекламе
                </li>
                <li class="e-ac-ul">
 					Реклама алкоголя и табака не показывается в детском контенте (детскиефильмы, сериалы, мультфильмы, детские программы)
                </li> 
            </ul>
        </div>
    </article> 
</xsl:template>






<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>
