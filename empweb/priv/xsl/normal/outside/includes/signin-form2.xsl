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

    <form action="{$Action}" method="{$Method}" >
        <table>
            <xsl:call-template name="u-ift">
                <xsl:with-param name="Label_name" select="'#'"  />
                <xsl:with-param name="Name" select="'id'"  />
                <xsl:with-param name="Value" select="'null'"  />
                <xsl:with-param name="Type" select="'hidden'"  />
            </xsl:call-template>
            <xsl:call-template name="u-ift">
                <xsl:with-param name="Label_name" select="'Логин'"  />
                <xsl:with-param name="Name" select="'login'"  />
                <xsl:with-param name="Placeholder" select="'Введите логин'"  />
                <xsl:with-param name="Required" select="'true'"/>
            </xsl:call-template>
            <xsl:call-template name="u-ift">
                <xsl:with-param name="Label_name" select="'Пароль'" />
                <xsl:with-param name="Name" select="'password'"  />
                <xsl:with-param name="Placeholder" select="'Введите пароль'"  />
                <xsl:with-param name="Type" select="'password'"  />
                <xsl:with-param name="Required" select="'true'"/>
            </xsl:call-template>
        </table>
        <input type="submit" value="Принять"/>
        <div>
            <xsl:text>Поля, обязательные для заполнения, помечены: </xsl:text>
            <xsl:call-template name="u-ift-rstar"/>
            <xsl:text>.</xsl:text> 
        </div>
    </form>

</xsl:template>

</xsl:stylesheet>
