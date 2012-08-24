<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/common-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />


<xsl:template name="s-title-common">
    <xsl:text>восстановление пароля</xsl:text>
</xsl:template>

<xsl:template name="link-css-base">
    <link rel="stylesheet" type="text/css" href="/c/height100.css" /> 
</xsl:template>
<xsl:template name="s-main-common">
    <section class="b-signup">
        <h3 class="b-signup-h">
            <xsl:text>Восстановление забытого пароля</xsl:text>
        </h3>
        <div class="s-signup">
            <xsl:call-template name="s-signup" />
        </div>
    </section>
</xsl:template>

<xsl:template name="u-csif-rs">
    <span class="u-csif-rstar">*</span>
</xsl:template>


<xsl:template name="u-csif">
    <xsl:param name="Label_name" select="'Поле'" />
    <xsl:param name="Type" select="'text'"/>
    <xsl:param name="Name" select="''"/>
    <xsl:param name="Placeholder" select="'Введите текст'"/>
    <xsl:param name="Required" select="''"/>
    <xsl:param name="Size" select="'25'"/>
    <xsl:param name="Auto_complete" select="'off'"/>
    <xsl:param name="Pattern" select="''"/>
    <xsl:param name="Max_length" select="'41'"/>
    <xsl:param name="Is_error" select="''"/>
    <xsl:param name="Error_type" select="''"/>
    <xsl:param name="Error_message" select="'поле введено не верно'"/>
    <xsl:param name="Val" select="''"/>


        <xsl:param name="Value" select="$Val/*[name()=$Name]"/>


    <div class="b-csif">
        <label class="e-csif-fl m-{$Name}">
            <xsl:if test="$Type != 'hidden'">
                <xsl:value-of select="$Label_name" />
                <xsl:if test="$Required">
                    <xsl:call-template name="u-csif-rs"/>
                </xsl:if>
            </xsl:if>
        </label>
        <input
            type="{$Type}"
            placeholder="{$Placeholder}"
            size="{$Size}"
            maxlength="{$Max_length}"
            autocomplete="{$Auto_complete}" 
            name="{$Name}"
            class="e-csif-fi {$Type}-{$Name}"
        >
            <xsl:if test="$Required != ''">
                <xsl:attribute name="required">required</xsl:attribute>
            </xsl:if>
            <xsl:if test="$Value != ''">
                <xsl:attribute name="value">
                    <xsl:value-of select="$Value" />
                </xsl:attribute>
            </xsl:if>
            <xsl:if test="$Value != ''">
                <xsl:attribute name="value">
                    <xsl:value-of select="$Value" />
                </xsl:attribute>
            </xsl:if>
            <xsl:if test="$Pattern != ''">
                <xsl:attribute name="pattern">
                    <xsl:value-of select="$Pattern" />
                </xsl:attribute>
            </xsl:if>
        </input>
        <div class="e-csif-fm">
            <xsl:choose>
                <xsl:when test="$Error_type = $Name">
                    <div class="e-csif-w m-{$Error_type}"
                        title="{$Error_message}">&nbsp;</div>
                </xsl:when>
                <xsl:when test="$Error_type != $Name">
                    <div class="e-csif-r">&nbsp;</div>
                </xsl:when>
                <xsl:otherwise>&nbsp;</xsl:otherwise>
            </xsl:choose>
        </div>
    </div>
</xsl:template>

<xsl:template name="signup-form2">
    <xsl:param name="Action" select="'/password_repair/post'" />
    <xsl:param name="Method" select="'POST'"/>
    <xsl:param name="Error_type" select="/data/meta/error-mess"/>
    <xsl:param name="Val" select="/data/val"/>

    <xsl:if test="$Error_type != ''">
        <div class="signup-form-alert">
            <span>
                <xsl:text> Есть ошибки: </xsl:text>
            </span>
            <xsl:choose>
                <xsl:when test="$Error_type = 'email'">
                    <xsl:text>пользователь с таким E-mail не существует</xsl:text>
                </xsl:when> 
                <xsl:otherwise>
                    <xsl:value-of select="$Error_type" />
                </xsl:otherwise>
            </xsl:choose>
        </div>
    </xsl:if>

    <form action="{$Action}" method="{$Method}" >
        <div>
            <div class="b-csif-c m-1"> 
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Электронная почта'" />
                    <xsl:with-param name="Name" select="'email'"  />
                    <xsl:with-param name="Placeholder" select="'mail@example.com'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Size" select="'1024'"/>
    				<xsl:with-param name="Type" select="'email'"/> 
    				<xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Error_message" select="'пользователь с таким E-mail уже существует'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Val" select="$Val"/>
                </xsl:call-template> 
            </div> 
        </div> 
        <button type="submit" value="Отправить" class="s-sfm-oval m-login" style="height:30px;">
            <span class="e-sfm-oval">Отправить</span>
        </button> 
    </form>
</xsl:template>


<xsl:template name="s-signup">
    <xsl:call-template name="signup-form2">
        <xsl:with-param name="Action" select="concat('/password_repair/post', /data/meta/self-retpath)" />
        <xsl:with-param name="Method" select="'POST'"/>
        <xsl:with-param name="Error_type" select="/data/error"/>
        <xsl:with-param name="Val" select="/data/val"/>
    </xsl:call-template>
</xsl:template>

<xsl:template name="foot-scripts-common">
    <!--
        Скрипты добавляемые внизу страницы.
        Рекомендовано это использовать, если сами скрипты не меняют
        начальный вид страницы или ее DOM (до загрузки)
    -->
    <script src="/j/jm.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
    <script src="/j/sl.js"><xsl:text><![CDATA[ ]]></xsl:text></script> 
    <script src="/j/height100.js"><xsl:text><![CDATA[ ]]></xsl:text></script>  
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>
