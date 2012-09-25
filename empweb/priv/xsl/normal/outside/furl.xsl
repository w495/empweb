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

<xsl:template name="s-pers">
    <article class="b-a">
        <p class="m-docs-p">
            <xsl:text>Ваш платеж отклонен! </xsl:text>
        </p>
        <p class="m-docs-p">
            <span>
                <xsl:text>Просим проверить правильность введенных данных </xsl:text>
                <xsl:text>и повторить попытку (</xsl:text>
            </span>
            <a href="/pers" title="переход в ваши личный кабинет" rel="details" >ссылка</a>
            <span>
                <xsl:text>).</xsl:text>
            </span>
        </p>
        <p class="m-docs-p">
            <span>
                <xsl:text>При повторных неудачных попытках </xsl:text>
                <xsl:text>просим связаться со службой поддержки  (</xsl:text>
            </span>
            <a href="http://www.tvzavr.ru/feedback/form" title="связаться со службой поддержки" rel="contact">ссылка</a>
            <span>
                <xsl:text>).</xsl:text>
            </span>
        </p>
        <p class="m-docs-p">
            <xsl:text>Приносим свои извинения за испытанные неудобства.</xsl:text>
        </p>
    </article>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>


</xsl:stylesheet>
