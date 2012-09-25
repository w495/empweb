<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:include href="../../shared/utils/u-ift.xsl" />
   
<xsl:template name="signin-form">
    <xsl:param name="Action" select="'/Users/Registration/'" />
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


<form action="{$Action}" method="{$Method}">  
		<div class="b-csif-c m-1">
			<div class="b-csif">
				<label class="e-csif-fl m-login">
					<xsl:text>Логин</xsl:text>
				</label>
				<input 
					type="text" 
					placeholder="Введите ваше имя пользователя" 
					size="1024" 
					maxlength="1024"   
					name="login" 
					class="e-csif-fi text-login" 
					required="required" />
			</div>
			<div class="b-csif">
				<label class="e-csif-fl m-password">
					<xsl:text>Пароль</xsl:text>
				</label>
				<input 
					type="password" 
					placeholder="Введите ваш пароль" 
					size="1024" 
					maxlength="1024"   
					name="password" 
					class="e-csif-fi text-password" 
					required="required" />
			</div> 
			<center style="text-align: center;">
                <button type="submit" value="Войти" class="s-sfm-oval m-login" style="height:30px;">
                    <span class="e-sfm-oval">Войти</span>
                </button> 
                <button type="button" onClick="window.location='/signup'" value="Зарегистрироваться" class="s-sfm-oval m-logout" style="height:30px;">
                    <span class="e-sfm-oval">Зарегистрироваться</span>
                </button>  
            </center>
	</div>
</form>


 

</xsl:template>

</xsl:stylesheet>
