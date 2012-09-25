/* ************************************************************************
    Соединение описания форм
        с элементами меню.
    Представляет из себя фабрику форм.
************************************************************************ */

qx.Class.define("zqr.view.StaticForms",
{
    extend : Object,

    statics : {
        
        // мастер создания и редактирования рекламной компании
        acvVideoCreate : function(biz, Row, formDescr) {
            return new zqr.view.Form.AcvVideoCreateMaster(biz, Row, formDescr);
        },

        // мастер создания и редактирования рекламной компании
        acvVideoCopy : function(biz, Row, formDescr) {
            // пока показываем тоже самое, что и при создании
            return new zqr.view.Form.AcvVideoCreateMaster(biz, Row, formDescr);
        },

        // мастер создания и редактирования рекламной компании
        acvVideoShow : function(biz, Row, formDescr) {
            // пока показываем тоже самое, что и при создании
            return new zqr.view.Form.AcvVideoShow(biz, Row, formDescr);
        },

        /// Форма создания и редактирования групп пользователей
        customerGroupForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.CustomerGroupForm(biz, Row, formDescr);
        },

        // ------------------------------------------------------------------
        /// Форма создания и редактирования пользователей
        customerUpdateForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.CustomerUpdateForm(biz, Row, formDescr);
        },
        /// Форма просмотр пользователей
        customerViewForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.CustomerViewForm(biz, Row, formDescr);
        },
        // ------------------------------------------------------------------

        /// Форма просмотра профиля пользователя
        customerProfileForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.CustomerProfileForm(biz, Row, formDescr);
        },

        /// Форма пополнения кошелька
        billMakeForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.BillMakeForm (biz, Row, formDescr);
        },

        /// Форма просмотра счета кошелька
        billViewForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.BillViewForm(biz, Row, formDescr);
        },

        /// Форма оплаты счета
        billPayForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.BillPayForm(biz, Row, formDescr);
        },

        /// Форма редактирования пара метров
        sysvarForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.SysvarForm(biz, Row, formDescr);
        },

        /// Форма редактирования пара метров
        snetForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.SnetForm(biz, Row, formDescr);
        },

        /// Форма редактирования пара метров
        platformForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.PlatformForm(biz, Row, formDescr);
        },

        /// Форма редактирования пара метров
        pageurlForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.PageurlForm(biz, Row, formDescr);
        },

        /// Форма редактирования пара метров
        snetRegionForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.SnetRegionForm(biz, Row, formDescr);
        },

        /// Форма редактирования пара метров
        configForm : function(biz, Row, formDescr) {
            return new zqr.view.Form.ConfigForm(biz, Row, formDescr);
        }
    }
});

