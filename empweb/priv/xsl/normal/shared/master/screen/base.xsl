<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../root.xsl"/>




<xsl:template name="s-title-root">
    <!--
        Подзаголовок страницы.
    -->
    <xsl:call-template name="s-title-base" />
</xsl:template>

<xsl:template name="link-css-root">
    <xsl:call-template name="link-css-base" />
</xsl:template>

<xsl:template name="link-css-base">
</xsl:template>

<xsl:template name="head-scripts-root">
    <xsl:call-template name="head-scripts-base" />
</xsl:template>

<xsl:template name="head-scripts-base">

</xsl:template>


<xsl:template name="s-main-root">
    <!--
        Основное содержимое документа.
        Первый уровенгь вложенности.
    -->
    <xsl:call-template name="s-main-base" />
</xsl:template>

<xsl:template name="foot-scripts-root">
    <!--
        Скрипты добавляемые внизу страницы.
        Рекомендовано это использовать, если сами скрипты не меняют
        начальный вид страницы или ее DOM (до загрузки)
    -->
    <xsl:call-template name="foot-scripts-base" />
</xsl:template>

<xsl:template name="foot-scripts-base">

</xsl:template>


</xsl:stylesheet>
