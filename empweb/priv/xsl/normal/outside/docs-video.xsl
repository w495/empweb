<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="docs.xsl"/>

<xsl:include href="includes/signin-form-mini.xsl" />
<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="s-title-base">
    <xsl:text>Документация</xsl:text>
</xsl:template>

<xsl:template name="s-signin">
    <xsl:call-template name="signin-form-mini">
        <xsl:with-param name="Action" select="concat('/signin/post', /data/meta/self-retpath)" />
        <xsl:with-param name="Method" select="'POST'"/>
        <xsl:with-param name="Has_errors" select="/data/meta/has-errors"/>
        <xsl:with-param name="Error_message" select="/data/meta/error-mess"/>
    </xsl:call-template>
</xsl:template>
 
<xsl:template name="s-news">
    <header class="b-news-header">
        <h1 class="e-news-head">
            <xsl:text>Видеореклама</xsl:text>
        </h1>
        <a class="b-news-doc" href="/docs" >
            <xsl:text>Документация</xsl:text>
        </a>
    </header> 
</xsl:template>



<xsl:template name="s-about">
    <article class="b-a">
        <h3 class="b-ah">
            <xsl:text>Вирусное видео</xsl:text>
        </h3>
        <div class="b-ac">
            <p class="b-ac-p" >
            	<img style="width: 400px;float: right;" src="/i/dv-2.jpg" />
                <xsl:text> 
				</xsl:text>
            </p>
            <ul class="b-ac-ul">
                <li class="e-ac-ul">
                    <xsl:text>Рекламное видео в разделе «Рекомендуемые»</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Суточный трафик – 1-10 тыс. просмотров (зависит от тематики, актуальности и красочности креатива) </xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Стоимость по прайс-листу - 20.000 руб./сутки без НДС</xsl:text>
                </li> 
            </ul>
        </div>
    </article>
    <br clear="all" />
    <article class="b-a">
        <h3 class="b-ah">
            <xsl:text>Pre-roll</xsl:text>
        </h3>
        <div class="b-ac">
            <p class="b-ac-p" >
            	<img style="width: 400px;float: right;" src="/i/dv-0.jpg" />
                <xsl:text>
                	Рекламный ролик перед показом видео (до 30 сек), 
					недельный трафик – 300 тыс.показов, 
					стоимость по прайс-листу – 
					700 руб/тыс. показов без НДС
				</xsl:text>
            </p> 
        </div>
    </article>
    <br clear="all" />
    <article class="b-a">
        <h3 class="b-ah">
            <xsl:text>Mid-roll</xsl:text>
        </h3>
        <div class="b-ac">
            <p class="b-ac-p" >
            	<img style="width: 400px;float: right;" src="/i/dv-1.jpg" />
                <xsl:text>
                	Рекламный ролик во время 
					показа видео (через 10-15 минут после старта), 
					недельная ёмкость - 250 тыс показов
					Стоимость по прайс-листу – 
					500 руб./тыс. показов без НДС
				</xsl:text>
            </p> 
        </div>
    </article>
    <br clear="all" />
    <article class="b-a">
        <h3 class="b-ah">
            <xsl:text>Pause-roll</xsl:text>
        </h3>
        <div class="b-ac">
            <p class="b-ac-p" >
            	<img style="width: 400px;float: right;" src="/i/dv-3.jpg" />
                <xsl:text>
                	Рекламный баннер, показывающийся при нажатии кнопки «Pause» во время просмотра видео, 
					недельный трафик - 150 тыс. показов,
					Стоимость по прайс-листу – 
					300 руб./тыс. показов без НДС
				</xsl:text>
            </p> 
        </div>
    </article>
    <br clear="all" />
    <article class="b-a">
        <h3 class="b-ah">
            <xsl:text>Post-roll</xsl:text>
        </h3>
        <div class="b-ac">
            <p class="b-ac-p" >
                <xsl:text>
                	Рекламный ролик после  показа видео, 
					недельный трафик -100 тыс. показов,
					Стоимость по прайс-листу – 
					400 руб./тыс. показов без НДС
				</xsl:text>
            </p> 
        </div>
    </article>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>
