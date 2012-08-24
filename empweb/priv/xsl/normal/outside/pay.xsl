<?xml version="1.0" encoding="utf-8"?>
<!--
    ЛИЧНЫЙ КАБИНЕТ РЕКЛАМОДАТЕЛЯ
-->

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/pers-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />


<xsl:template name="head-scripts-pers">

</xsl:template>

<xsl:template name="s-main-pers">
    <section class="s-pers m-pers-f">
        <xsl:call-template name="s-pers" />
    </section>
</xsl:template>

<xsl:template name="s-pers">
    <article class="b-a">
        <p class="m-docs-p">
            <xsl:text>Оплату можно произвести одним из следующих способов: </xsl:text>
            <xsl:text>поставить иконки всех способов </xsl:text>
            <xsl:text>(Виза и Мастеркард, Киви, Яндекс-деньги, Веб-мани)</xsl:text>
        </p>
        <p class="m-docs-p">
            <xsl:text>Обращаем Ваше внимание, </xsl:text>
            <xsl:text>что для оплаты Вы будете перенаправлены </xsl:text>
            <xsl:text>на защищенный сайт нашего партнера ЗАО «Хронопей».</xsl:text>
        </p>
    </article>

    <!-- https://83.229.137.196:8080/api/pay-ua/ -->
    <form class="s-pers-pay-f" method="POST" action="{/data/pay/action}">
        <input type="hidden" name="user_id"       value="{/data/pay/user_id}" />
        <input type="hidden" name="product_id"    value="{/data/pay/product_id}"  />
        <input type="hidden" name="amount"        value="{/data/pay/amount}"  />
        <input type="hidden" name="shop_id"       value="{/data/pay/shop_id}"  />
        <input type="hidden" name="ps_id"         value="{/data/pay/ps_id}"  />
        <input type="hidden" name="success_url"    value="{/data/pay/success_url}"  />
        <input type="hidden" name="failure_url"    value="{/data/pay/failure_url}"  />
        <input type="hidden" name="shop_f1"    value="{/data/pay/shop_f1}"  />
        <input type="hidden" name="shop_f2"    value="{/data/pay/shop_f2}"  />
        <input type="hidden" name="shop_f3"    value="{/data/pay/shop_f3}"  />
        <input type="hidden" name="shop_f4"    value="{/data/pay/shop_f4}"  />
        <input type="hidden" name="shop_f5"    value="{/data/pay/shop_f5}"  />

        <input type="hidden" name="sign" value="{/data/sign}" />
        <input class="s-pers-pay-but" type="submit" value="Перейти к странице оплаты"   />
    </form>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>
