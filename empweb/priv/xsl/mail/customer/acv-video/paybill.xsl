<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../../shared/master/acv-video/change.xsl"/>



<xsl:template name="s-main-change">
    <xsl:text>Ваш счет успешно оплачен.&#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Рекламная компания будет отопражаться &#xa;</xsl:text>
    <xsl:text>в соответствии с указанными Вами параметрами &#xa;</xsl:text>
    <xsl:text>с </xsl:text>
    <xsl:call-template name="erlangFormatDate">
          <xsl:with-param name="DateTime" select="/data/acv-video/datestart"/>
    </xsl:call-template>
    <xsl:text> по </xsl:text>
    <xsl:call-template name="erlangFormatDate">
          <xsl:with-param name="DateTime" select="/data/acv-video/datestop"/>
    </xsl:call-template>
    <xsl:text>. &#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Для изменения параметров кампании &#xa;</xsl:text>
    <xsl:text>просим связаться со службой поддержки &#xa;</xsl:text>
    <xsl:text>http://tvzavr.ru/feedback/form &#xa;</xsl:text>
</xsl:template>
    

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>
