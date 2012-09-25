<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:include href="../../shared/utils/u-ift.xsl" />
    
<xsl:template name="signin-form-mini">
    <xsl:param name="Action" select="'/Users/Registration/'" />
    <xsl:param name="Size" select="15" />
    <xsl:param name="Method" select="'POST'"/>
    <xsl:param name="Has_errors" select="/data/meta/has-errors"/>
    <xsl:param name="Error_message" select="/data/meta/error-mess"/>

    <xsl:if test="$Has_errors = 'true'">
        <div class="signup-form-alert">
            <span>
                <xsl:text> Есть ошибки: </xsl:text>
            </span>
            <xsl:choose>
                <xsl:when test="$Error_message = 'not_unique email'">
                    <xsl:text>пользователь с таким E-mail уже существует</xsl:text>
                </xsl:when>
                <xsl:when test="$Error_message = 'not_unique login'">
                    <xsl:text>пользователь с таким логином уже существует</xsl:text>
                </xsl:when>
                <xsl:when test="$Error_message = 'not conf'">
                    <xsl:text>пароли не совпадают</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$Error_message" />
                </xsl:otherwise>
            </xsl:choose>
        </div>
    </xsl:if>

    <form action="{$Action}" method="{$Method}" >
        <input class="s-sfm-input m-id" type="hidden" value="null" name="id" maxlength="{$Size}" size="{$Size}" placeholder="Id"/>
        <input class="s-sfm-input m-login" type="text" required="required" name="login" maxlength="{$Size}" size="{$Size}" placeholder="Логин"/>
        <input class="s-sfm-input m-password" type="password" required="required" name="password" maxlength="{$Size}" size="{$Size}" placeholder="Пароль"/>
        <input class="s-sfm-but submit" type="submit" value="↵"/>
    </form>
    <div class="s-sfm-lc" >
        <a class="s-sfm-l m-signup" href="/signup" ><xsl:text>Зарегестрироваться</xsl:text></a>
        <xsl:text> / </xsl:text>
        <a class="s-sfm-l m-rempass" href="/rempass"><xsl:text>Вспомнить пароль</xsl:text></a>
    </div>

</xsl:template>

</xsl:stylesheet>
