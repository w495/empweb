<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/title-page.xsl"/>


<xsl:include href="../shared/utils/tipograf.xsl" />


<xsl:template name="s-title-1">
    <xsl:text>главная</xsl:text>
</xsl:template>



<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>