<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/common-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:include href="includes/signin-form.xsl" />

<xsl:template name="s-title-common">
    <xsl:text>вход</xsl:text>
</xsl:template>

<xsl:template name="s-signin">
</xsl:template>

<!--
    TODO: Поменять имена классов но нормальные
--> 

<xsl:template name="link-css-base">
    <link rel="stylesheet" type="text/css" href="/c/height100.css" /> 
</xsl:template>

<xsl:template name="s-main-common">
    <section class="b-signup">
        <h3 class="b-signup-h">
            <xsl:text>Вход</xsl:text>
        </h3>
        <div class="s-signup">
            <xsl:call-template name="l-signin" />
        </div>
    </section>
</xsl:template>


<xsl:template name="l-signin">

    <div class="signin-form">  
		<xsl:variable name="Error">
			<xsl:value-of select="/data/error"/>
		</xsl:variable>
    	<xsl:if test="$Error='true'">
    		<div class="b-error">
    			Неправильный логин или пароль. Проверьте данные и повторите попытку. Если проблема повторится - <a onclick="ContactUs()" href="#">свяжитесь с нами</a>
    		</div>
    	</xsl:if>
        <xsl:call-template name="signin-form">
            <xsl:with-param name="Action" select="concat('/signin/post', /data/meta/self-retpath)" />
            <xsl:with-param name="Method" select="'POST'"/>
            <xsl:with-param name="Has_errors" select="/data/meta/has-errors"/>
            <xsl:with-param name="Error_message" select="/data/meta/error-mess"/>
        </xsl:call-template>
    </div>

</xsl:template>

<xsl:template name="foot-scripts-common"> 
    <script src="/j/height100.js"><xsl:text><![CDATA[ ]]></xsl:text></script> 
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>
