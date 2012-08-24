<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<!--
    Описание правостороннего педжера,
    который делает хитрую педженацию.

    <xsl:call-template name="simple-pager-right">
        <xsl:with-param name="Start" select="/data/meta/cur-id"/>
        <xsl:with-param name="Number" select="/data/meta/pages - 1"/>
        <xsl:with-param name="Maximum" select="3" />
        <xsl:with-param name="LinkPathPreffix" select="$LinkPathPreffix" />
    </xsl:call-template>
-->

<xsl:template name="simple-pager-right">
    <xsl:param name="Number"   />
    <xsl:param name="Start"    />
    <xsl:param name="Maximum"  />
    <xsl:param name="LinkPathPreffix" />
    <xsl:call-template name="count-up">
        <xsl:with-param name="Start" select="$Start"/>
        <xsl:with-param name="Number" select="$Number"/>
        <xsl:with-param name="Maximum" select="$Maximum" />
        <xsl:with-param name="LinkPathPreffix" select="$LinkPathPreffix" />
    </xsl:call-template>
</xsl:template>
    
<xsl:template name="count-up">
    <xsl:param name="Number"    />
    <xsl:param name="LinkPathPreffix"  />
    <xsl:param name="Start"     />
    <xsl:param name="Maximum"   />
    <xsl:if test="$Number &gt; $Start">
        <xsl:choose>
            <xsl:when test="$Number &gt; ($Maximum + $Start)">
                <xsl:call-template name="count-up">
                    <xsl:with-param name="Number" select="($Maximum + $Start) -1"/>
                    <xsl:with-param name="Maximum" select="$Maximum"/>
                    <xsl:with-param name="Start" select="$Start"/>
                    <xsl:with-param name="LinkPathPreffix" select="$LinkPathPreffix"/>-
                </xsl:call-template>        
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="count-up">
                    <xsl:with-param name="Number" select="$Number - 1"/>
                    <xsl:with-param name="Maximum" select="$Maximum"/>
                    <xsl:with-param name="Start" select="$Start"/>
                    <xsl:with-param name="LinkPathPreffix" select="$LinkPathPreffix"/>
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:if>
    <xsl:choose>
        <xsl:when test="$Number &lt; ($Maximum + $Start)">
            <xsl:call-template name="simple-pager-right-item">
                <xsl:with-param name="Link" select="concat($LinkPathPreffix, $Number)"/>
                <xsl:with-param name="Name" select="$Number + 1"/>
            </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
            <xsl:call-template name="simple-pager-right-item">
                <xsl:with-param name="Link" select="concat($LinkPathPreffix, $Number)"/>
                <xsl:with-param name="Name" select="'...'"/>                    
            </xsl:call-template>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="simple-pager-right-item">
    <xsl:param name="Link" />
    <xsl:param name="Name" />
    
    <xsl:text> </xsl:text>
    <a href="{$Link}/">
        <xsl:value-of select="$Name" />
    </a>
    <xsl:text> </xsl:text>    
</xsl:template>

</xsl:stylesheet>
