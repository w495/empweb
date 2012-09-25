<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/common-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:include href="includes/signin-form.xsl" />

<xsl:template name="s-title-common">
    <xsl:text>вход</xsl:text>
</xsl:template>

<xsl:template name="link-css-base">
    <link rel="stylesheet" type="text/css" href="/c/height100.css" /> 
</xsl:template>

<!--
    TODO: Поменять имена классов но нормальные
-->

<xsl:template name="s-main-common">
    <br />
    <h3 class="b-signup-h">Аккаунт еще не активирован</h3>
    <section class="b-wait">
        Подождите, пока модератор проверит ваши регистрационные данные. <br />
        Мы оповестим Вас по электронной почте сразу после активации учетной записи.<br />
        Перед началом работы, рекомендуем ознакомиться с <a href="/docs/howto">руководством пользователя</a>.
    </section>
</xsl:template>



<xsl:template name="foot-scripts-common">
    <!--
        Скрипты добавляемые внизу страницы.
        Рекомендовано это использовать, если сами скрипты не меняют
        начальный вид страницы или ее DOM (до загрузки)
    -->
    <script src="/j/jm.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
    <script src="/j/sl.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
    <script src="/j/reg.js"><xsl:text><![CDATA[ ]]></xsl:text></script> 
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>
