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
    
<xsl:template name="top-line-container">
    <div id="top-line-links">
        <a href="/Home/Spec/On{/data/meta/current-path}" id="visually-impaired-version" title="Версия для слабовидящих">&nbsp;</a>
        <a href="/Hotline/Index/" id="phone-link" title="горячая линия">&nbsp;</a>
        <a href="/Contacts/List/" id="e-mail-link" title="контакты">&nbsp;</a>
        <a href="/Wap/" id="wap-link" title="мобильная версия" >мобильная версия</a>
    </div>
    <div id="top-line-search">
        
        <!--
            <a href="#" id="search-link" title="Поиск">&nbsp;</a>
        -->
        
        <form action="/Search/" method="POST" >
            <input id="search-field" type="text" size="32"  name="q" placeholder="Введите поисковый запрос" />
            <input id="search-submitter"  type="submit"  value="искать" />
        </form>

<!--        
    <div id="cse-search-form" style="width: 100%;">
    <div class="gsc-control-searchbox-only gsc-control-searchbox-only-ru" dir="ltr">
        <form class="gsc-search-box" accept-charset="utf-8">
            <table class="gsc-search-box" cellspacing="0" cellpadding="0">
                <tbody>
                    <tr>
                        <td class="gsc-input">
                                <input class="gsc-input"
                                       type="text"
                                       autocomplete="off"
                                       size="10"
                                       name="search"
                                       title="поиск"
                                       style="background: url('http://www.google.com/cse/intl/ru/images/google_custom_search_watermark.gif')
                                       no-repeat scroll left center rgb(255, 255, 255);" />
                        </td>
                        <td class="gsc-search-button">
                            <input class="gsc-search-button" type="submit" value="Поиск" title="поиск" />
                        </td>
                        <td class="gsc-clear-button">
                            <div class="gsc-clear-button" title="удалить результаты">&nbsp;</div>
                        </td>
                    </tr>
                </tbody>
            </table>
            
            <table class="gsc-branding" cellspacing="0" cellpadding="0">
                <tbody>
                    <tr>
                        <td class="gsc-branding-user-defined">&nbsp;</td>
                        <td class="gsc-branding-text">
                            <div class="gsc-branding-text">технология</div>
                        </td>
                        <td class="gsc-branding-img">
                            <img class="gsc-branding-img" src="http://www.google.com/uds/css/small-logo.png" />
                        </td>
                    </tr>
                </tbody>
            </table>
        </form>
    </div>
</div>
-->
    </div>
</xsl:template>


</xsl:stylesheet> 





