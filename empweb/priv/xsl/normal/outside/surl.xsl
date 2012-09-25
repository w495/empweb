<?xml version="1.0" encoding="utf-8"?>
<!--
    ЛИЧНЫЙ КАБИНЕТ РЕКЛАМОДАТЕЛЯ
-->

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/pers-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="head-scripts-pers">
</xsl:template>

<xsl:template name="s-main-pers">
    <section class="s-pers m-pers-f">
        <xsl:call-template name="s-pers" />
    </section>
</xsl:template>

<!--
    w-495:
        Имхо суда надо какую-то картинку
-->

<xsl:template name="s-pers">
    <article class="b-a">
        <p class="m-docs-p">
            <xsl:text>Ваш платеж обрабатывается. </xsl:text>
            <xsl:text>Информация о платеже Вам будет выслана по е-мейлу.</xsl:text>
        </p>
        <p class="m-docs-p">
            <span>
                <xsl:text>Вы можете просмотреть созданные рекламные кампании </xsl:text>
                <xsl:text>в личном кабинете (</xsl:text>
            </span>
            <a href="/pers" title="переход в ваши личный кабинет" rel="details" >ссылка</a>
            <span>
                <xsl:text>).</xsl:text>
            </span>
        </p>
        <p class="m-docs-p">
            <span>
                <xsl:text>Для изменения параметров кампании </xsl:text>
                <xsl:text>просим связаться со службой поддержки  (</xsl:text>
            </span>
            <a href="http://www.tvzavr.ru/feedback/form" title="связаться со службой поддержки" rel="contact">ссылка</a>
            <span>
                <xsl:text>).</xsl:text>
            </span>
        </p>
    </article>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>
