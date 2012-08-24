<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp "&#160;">
    <!ENTITY raquo  "&#187;">
    <!ENTITY laquo  "&#171;">
]>

<!--
    Самая верхняя строка с кнопками и поиском.
    Всего скорее будет одинакова для всех страниц сайта.
-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >

<xsl:template name="athorise-container">
    <xsl:param name ="Login"  select="/data/meta/login" />
    <xsl:param name ="LoginPath"  select="concat('/Users/Login', /data/meta/current-path)" />
    <xsl:param name ="LoginPathHidden"  select="concat('/Users/LoginAjax', /data/meta/current-path)" />
    <xsl:param name ="LogoutPath"  select="concat('/Users/Logout', /data/meta/self-retpath)" />
    <xsl:param name ="RegistrationPath"  select="concat('/Users/Registration', /data/meta/current-path)"  />
    <xsl:param name ="ChangePassPath"  select="'/Users/ChangePass/'" />
    <span id="athorise-container-on">
        <xsl:if test="not($Login) or ($Login = 'undefined')">
            <xsl:attribute name="style">display:none</xsl:attribute>
        </xsl:if>
        <xsl:text>Это Вы:</xsl:text>
        <span id="athorise-container-login"><xsl:value-of select="$Login" /></span>
        <span>&nbsp;/&nbsp;</span>
        <a href="{$LogoutPath}" class="relief">выйти</a>
        <span>&nbsp;</span>
    </span>
    <xsl:choose>
        <xsl:when test="not($Login) or ($Login = 'undefined')">
            <!--
                Эту разметку можно вставлять и через javascript,
                но разметка в javascript это очень плохо
            -->
            <div id="login-form-error" style="display:none">&nbsp;</div>
            <form action="{$LoginPath}"  method="POST" id="common-login-form" >
                <input type="text"
                       name="login"
                       placeholder="введите логин" />
                <input type="password"
                       name="password"
                       placeholder="введите пароль" />
                <input type="submit" value="войти" class="relief" />
                <span>
                    <span>&nbsp;</span>
                    <a href="{$RegistrationPath}">регистрация</a>
                    <span>&nbsp;/&nbsp;</span>
                    <a href="{$ChangePassPath}">забыли пароль?</a>
                </span>
            </form>
        </xsl:when>
    </xsl:choose>
</xsl:template>    


</xsl:stylesheet> 
