<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../../shared/master/customer/password_repair.xsl"/>

<xsl:template name="s-main-password-repair">
    <xsl:text>Вы, или кто-то другой выполнили запрос на смену пароля </xsl:text>
    <xsl:text>в рекламной системе Tvzavr! &#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Если Вы не совершали данной операции - просто проигнорируйте это сообщение. &#xa;</xsl:text>
    <xsl:text>Для смены пароля - просто перейдите по ссылке, или скопируйте ее и вставьте в адресную строку</xsl:text>
    <xsl:text> браузера (если Ваш почтовый клиент не позволяет перейти по ссылке). &#xa;</xsl:text>
</xsl:template>
    

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>


