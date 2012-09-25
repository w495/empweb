<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/common-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="s-main-2">
    <!--
        Основное содержимое страницы
    -->
    <xsl:text>Основное содержимое простой страницы</xsl:text>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>
