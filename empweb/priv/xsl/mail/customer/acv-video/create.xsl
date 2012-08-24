<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../../shared/master/acv-video/create.xsl"/>

<xsl:template name="s-main-create">
    <xsl:text>Спасибо за создание кампании </xsl:text>
    <xsl:text>в рекламной системе Tvzavr! &#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Дождитесь одобрения вашей рекламной кампании </xsl:text>
    <xsl:text>и выставления Вам счета. &#xa;</xsl:text>
    <xsl:text>Вам придет оповещение по почте. &#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Для изменения параметров кампании &#xa;</xsl:text>
    <xsl:text>просим связаться со службой поддержки &#xa;</xsl:text>
    <xsl:text>http://tvzavr.ru/feedback/form</xsl:text>
    <xsl:text>&#xa;</xsl:text>
</xsl:template>
    

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>


