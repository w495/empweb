/* ************************************************************************

#asset(qx/icon/Tango/128/apps/utilities-archiver.png)

#asset(qx/icon/Tango/128/apps/internet-web-browser.png)


************************************************************************ */


qx.Class.define("zqr.view.Form.BillPayForm",
{
    extend : zqr.view.Form.BaseForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row);
        this.setLegend("Оплата счета");
    },

    members : { 

        ppReq : null,
        pprc : {  // purse pay request config
            url: "/pursepay",
            method: "GET",
            mimetype: "application/json"
        },

        epReq : null,
        eprc : {  // extermal pay request config
            url: "/generate-bill/pdf",
            method: "GET",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-bill-info",
            method: "GET",
            mimetype: "application/json"
        },

        /* Поля формы */
        inp : {
            Id: null
        },


        buildForm : function() {
            console.log("buildForm : function");
            this.base(arguments);

            this.purseButton  =  new qx.ui.form.Button("Из кошелька", "icon/128/apps/utilities-archiver.png")
                .set({width: 300, height: 300});
            this.purseButton.setIconPosition("top");

            this.payButton= new qx.ui.form.Button("Напрямую", "icon/128/apps/internet-web-browser.png")
                .set({width: 300, height: 300});
            this.payButton.setIconPosition("top");

            var layout = new qx.ui.layout.Grid(12, 6);
            var cnt = new qx.ui.container.Composite(layout);


            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");


            var vertical_offset = 0;
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            cnt.add(this.purseButton, {row:++vertical_offset, column:0});
            cnt.add(this.payButton,   {row:vertical_offset, column:1});

            this.submitButton = null;
            this.addbuttonRow(cnt, ++vertical_offset);
            this.controller.placeForm(cnt);
            console.log("buildForm : function");
        },




        _onPursePayClick: function(value){
            return function() {
                this.ppReq = new qx.io.remote.Request
                    (this.pprc.url, this.pprc.method, this.pprc.mimetype);
                this.ppReq.setTimeout(60000);
                this.ppReq.setParameter("id", value.id);
                this.ppReq.setParameter("uid", value.uid);
                this.ppReq.setParameter("customer_id", value.customer_id);
                this.ppReq.addListener("completed", this._onPursePayCompleted, this);
                this.ppReq.send();
            }
        },

        _onPursePayCompleted : function(response) {
            this.controller.enableForm();

            var result = response.getContent();
            alert(result);
            console.log(result);
            
            this._handleResultOnError(
                result,
                function(){
                    this._pursePayWindow();
                    this.close().close();
                }
            );
        },

        _pursePayWindow: function(){
            this.infoWindow("Заявка принята.<br/>Инструкции по оплате будут высланы на ваш email.");
        },

        _onExternalPayClick: function(value){
            if("" != value.paydate){
                /** Счет уже оплачен **/
                return function() {
                    this.infoWindow("Счет уже оплачен");
                };
            }
            if("true" == value.customer_legal_type){
                /** Пользователь юридическое лицо **/
                return function() {
                    this.epReq = new qx.io.remote.Request
                        (this.eprc.url, this.eprc.method, this.eprc.mimetype);
                    this.epReq.setTimeout(60000);
                    this.epReq.setParameter("id", value.id);
                    this.epReq.setParameter("uid", value.uid);
                    this.epReq.setParameter("customer_id", value.customer_id);
                    this.epReq.addListener("completed", this._onExternalPayCompleted, this);
                    this.epReq.send();
                };
            }
            else{
                /** Пользователь физическое лицо **/
                return function() {
                    var actionurl = zqr.util.utils.buidUrl(
                        "/directpay",
                        {id: value.id, uid: value.uid}
                    );
                    if("" == value.paydate){
                        window.open(actionurl, "_blank", "", false);
                    }
                }
            }
    
            return function() {
                this.alert("Oшибка");
            };
        },

        _onExternalPayCompleted : function(response) {
            this.controller.enableForm();
            var result = response.getContent();
            console.log(result);
            this._handleResultOnError(result, function(){
                if(result && result.bill_url){
                    console.log("result.bill_url = ", result.bill_url);
                    this.confirm(
                        "Вам на почту был выслан бланк для оплаты счета.<br/>" +
                        "Желаете ли Вы скачать бланк прямо сейчаc?",
                        function(){
                            this.makeClosable(function(){
                                window.open(result.bill_url, "_self", "", false);
                            });
                        }
                    );
                    this.close().close();
                }
            });
        },

        addListeneres:  function(value){
            this.purseButton.addListener("execute", this._onPursePayClick(value), this);
            this.payButton.addListener("execute", this._onExternalPayClick(value), this);
        },

        /**
            Заполняет форму полученными данными.
        **/
        fillForm: function(data) {
            this.setLegend("Оплата счета " + data.value.uid);

            if("" != data.value.paydate){
                this.purseButton.setEnabled(false);
                this.payButton.setEnabled(false);
            }
    
            this.addListeneres(data.value);
        }
    }
});

