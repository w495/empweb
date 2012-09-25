<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template name="tipograf">
    <xsl:param name="Input" />
    <xsl:variable name="BBCodes" select="document('tipograf/bbcode.xml')/patterns"/>
    <xsl:variable name="TipoCodes" select="document('tipograf/tipo.xml')/patterns"/>
    
    <xsl:call-template name="replace-2-patterns">
        <xsl:with-param name="patterns" select="$BBCodes" />
        <xsl:with-param name="text">
            <xsl:call-template name="replace-2-patterns">
                <xsl:with-param name="patterns" select="$TipoCodes" />
                <xsl:with-param name="text" select="$Input" />
            </xsl:call-template>
        </xsl:with-param>
    </xsl:call-template>
    <!-- <xsl:value-of select="$Input" disable-output-escaping="yes" /> -->
</xsl:template>

<xsl:template name="replace-16-patterns">
    <xsl:param name="patterns" />
    <xsl:param name="text" />
    <xsl:call-template name="replace-8-patterns" >
        <xsl:with-param name="patterns" select="$patterns" />
        <xsl:with-param name="text">
            <xsl:call-template name="replace-8-patterns">
                <xsl:with-param name="patterns" select="$patterns" />
                <xsl:with-param name="text" select="$text" />
            </xsl:call-template>            
        </xsl:with-param>
    </xsl:call-template>
</xsl:template>

<xsl:template name="replace-8-patterns">
    <xsl:param name="patterns" />
    <xsl:param name="text" />
    <xsl:call-template name="replace-4-patterns" >
        <xsl:with-param name="patterns" select="$patterns" />
        <xsl:with-param name="text">
            <xsl:call-template name="replace-4-patterns">
                <xsl:with-param name="patterns" select="$patterns" />
                <xsl:with-param name="text" select="$text" />
            </xsl:call-template>            
        </xsl:with-param>
    </xsl:call-template>
</xsl:template>

<xsl:template name="replace-4-patterns">
    <xsl:param name="patterns" />
    <xsl:param name="text" />
    <xsl:call-template name="replace-2-patterns" >
        <xsl:with-param name="patterns" select="$patterns" />
        <xsl:with-param name="text">
            <xsl:call-template name="replace-2-patterns">
                <xsl:with-param name="patterns" select="$patterns" />
                <xsl:with-param name="text" select="$text" />
            </xsl:call-template>            
        </xsl:with-param>
    </xsl:call-template>
</xsl:template>

<xsl:template name="replace-2-patterns">
    <xsl:param name="patterns" />
    <xsl:param name="text" />
    <xsl:call-template name="replace-patterns-complex-3" >
        <xsl:with-param name="patterns" select="$patterns" />
        <xsl:with-param name="text">
            <xsl:call-template name="replace-patterns-complex-3">
                <xsl:with-param name="patterns" select="$patterns" />
                <xsl:with-param name="text" select="$text" />
            </xsl:call-template>
        </xsl:with-param>
    </xsl:call-template>
</xsl:template>

<xsl:template name="replace-patterns-complex-3">
    <xsl:param name="patterns" />
    <xsl:param name="text" />
    <xsl:call-template name="replace-patterns">
        <xsl:with-param name="patterns" select="$patterns/item" />
        <xsl:with-param name="text">
            <xsl:call-template name="replace-bi-patterns">
                <xsl:with-param name="patterns" select="$patterns/pair" />
                <xsl:with-param name="text">
                    <xsl:call-template name="replace-tri-patterns">
                        <xsl:with-param name="patterns" select="$patterns/triple" />
                        <xsl:with-param name="text" select="$text" />  
                    </xsl:call-template>
                </xsl:with-param>
            </xsl:call-template>            
        </xsl:with-param>
    </xsl:call-template>
</xsl:template>

<xsl:template name="replace-patterns">
    <xsl:param name="patterns" />
    <xsl:param name="text" />
    <xsl:choose>
        <xsl:when test="not($patterns)">
            <xsl:value-of select="$text" disable-output-escaping="yes"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:variable name="pattern" select="$patterns[1]/@pattern" />
            <xsl:choose>
                <xsl:when test="contains($text, $pattern)">
                    <xsl:variable name="before" select="substring-before($text, $pattern)" />
                    <xsl:variable name="after" select="substring-after($text, $pattern)" />
                    <xsl:call-template name="replace-patterns">
                        <xsl:with-param name="text" select="$before" />
                        <xsl:with-param name="patterns" select="$patterns" />
                    </xsl:call-template>
                    <xsl:value-of select="$patterns[1]" disable-output-escaping="yes" />
                    <xsl:call-template name="replace-patterns">
                        <xsl:with-param name="text" select="$after" />
                        <xsl:with-param name="patterns" select="$patterns" />
                    </xsl:call-template>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:call-template name="replace-patterns">
                        <xsl:with-param name="text" select="$text" />
                        <xsl:with-param name="patterns" select="$patterns[position() &gt; 1]" />
                    </xsl:call-template>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="replace-bi-patterns">
    <xsl:param name="patterns" />
    <xsl:param name="text" />
    <xsl:choose>
        <xsl:when test="not($patterns)">
            <xsl:value-of select="$text" disable-output-escaping="yes"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:variable name="left" select="$patterns[1]/left/@pattern" />
            <xsl:variable name="right" select="$patterns[1]/right/@pattern" />
            <xsl:choose>
                <xsl:when test="contains($text, $left) and contains($text, $right) ">
                    <xsl:variable name="before-left" select="substring-before($text, $left)" />
                    <xsl:variable name="after-left" select="substring-after($text, $left)" />
                    <xsl:variable name="middle" select="substring-before($after-left, $right)" />
                    
                    <xsl:variable name="after-right" select="substring-after($after-left, $right)" />
                    <xsl:call-template name="replace-bi-patterns">
                        <xsl:with-param name="text" select="$before-left" />
                        <xsl:with-param name="patterns" select="$patterns[position() &gt; 1]" />
                    </xsl:call-template>
                    <xsl:if test="($middle != '') and ($middle != $after-left)" >
                        <xsl:value-of select="$patterns[1]/left" disable-output-escaping="yes" />
                        <xsl:call-template name="replace-bi-patterns">
                            <xsl:with-param name="text" select="$middle" />
                            <xsl:with-param name="patterns" select="$patterns[position() &gt; 1]" />
                        </xsl:call-template>
                        <xsl:value-of select="$patterns[1]/right" disable-output-escaping="yes" />
                    </xsl:if>
                    <xsl:call-template name="replace-bi-patterns">
                        <xsl:with-param name="text" select="$after-right" />
                        <xsl:with-param name="patterns" select="$patterns" />
                    </xsl:call-template>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:call-template name="replace-bi-patterns">
                        <xsl:with-param name="text" select="$text" />
                        <xsl:with-param name="patterns" select="$patterns[position() &gt; 1]" />
                    </xsl:call-template>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="replace-tri-patterns">
    <xsl:param name="patterns" />
    <xsl:param name="text" />
    <xsl:choose>
        <xsl:when test="not($patterns)">
            <xsl:value-of select="$text" disable-output-escaping="yes"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:variable name="left" select="$patterns[1]/left/@pattern" />
            <xsl:variable name="middle" select="$patterns[1]/middle/@pattern" />
            <xsl:variable name="right" select="$patterns[1]/right/@pattern" />
            <xsl:choose>
                <xsl:when test="contains($text, $left) and contains($text, $right) ">
                    <xsl:variable name="before-left" select="substring-before($text, $left)" />
                    <xsl:variable name="after-left" select="substring-after($text, $left)" />
                    <xsl:variable name="middle-all" select="substring-before($after-left, $right)" />
                    <xsl:variable name="before-middle" select="substring-before($middle-all, $middle)" />
                    <xsl:variable name="after-middle" select="substring-after($middle-all, $middle)" />
                    <xsl:variable name="after-right" select="substring-after($after-left, $right)" />
                    
                    <xsl:call-template name="replace-tri-patterns">
                        <xsl:with-param name="text" select="$before-left" />
                        <xsl:with-param name="patterns" select="$patterns" />
                    </xsl:call-template>                    
                    <xsl:value-of select="$patterns[1]/left" disable-output-escaping="yes" />
                    <xsl:call-template name="replace-tri-patterns">
                        <xsl:with-param name="text" select="$before-middle" />
                        <xsl:with-param name="patterns" select="$patterns" />
                    </xsl:call-template>
                    <xsl:value-of select="$patterns[1]/middle" disable-output-escaping="yes" />
                    <xsl:call-template name="replace-tri-patterns">
                        <xsl:with-param name="text" select="$after-middle" />
                        <xsl:with-param name="patterns" select="$patterns" />
                    </xsl:call-template>
                    <xsl:value-of select="$patterns[1]/right" disable-output-escaping="yes" />
                    <xsl:call-template name="replace-tri-patterns">
                        <xsl:with-param name="text" select="$after-right" />
                        <xsl:with-param name="patterns" select="$patterns" />
                    </xsl:call-template>
                    
                </xsl:when>
                <xsl:otherwise>
                    <xsl:call-template name="replace-tri-patterns">
                        <xsl:with-param name="text" select="$text" />
                        <xsl:with-param name="patterns" select="$patterns[position() &gt; 1]" />
                    </xsl:call-template>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

</xsl:stylesheet>
