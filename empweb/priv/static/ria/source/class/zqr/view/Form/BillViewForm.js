/**
    Форма просмотра
    конкретного счета кошелька.
**/


/* ************************************************************************


************************************************************************ */

qx.Class.define("zqr.view.Form.BillViewForm",
{
    extend : zqr.view.Form.BaseForm,

    construct : function(controller, Row) {
        this.row = Row;
        this.base(arguments, controller, Row);
        this.setLegend("Просмотр счета");
    },

    members : { 
        urc : {  // upload request config
            url: "/update-bill",
            method: "POST",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-bill-info",
            method: "GET",
            mimetype: "application/json"
        },

        /* Поля формы */

//         inp : {
//             Id                          : null,
//             Uid                         : null,
//             Uid_type                    : null,
//             Purse_id                    : null,
//             Paydate                     : null,
//             Billdate                    : null,
//             Sum                         : null,
//             Product_id                  : null,
//             Product_type_name           : null,
//             Product_type_description    : null
//         },

        textArea : null,

        buildForm : function() {
            this.base(arguments);

            // this.inp.Id = new qx.ui.form.TextField();
            // this.inp.Uid = new qx.ui.form.TextField();
            // this.inp.Uid_type = new qx.ui.form.TextField();
            // this.inp.Purse_id = new qx.ui.form.TextField();
            // this.inp.Paydate = new qx.ui.form.TextField();
            // this.inp.Billdate = new qx.ui.form.TextField();
            // this.inp.Sum = new qx.ui.form.Spinner(0, 100, 1152921504606846976);
            // this.inp.Product_id = new qx.ui.form.TextField();
            // this.inp.Product_type_name = new qx.ui.form.TextField();
            // this.inp.Product_type_description = new qx.ui.form.TextField();

            var cnt = new qx.ui.container.Composite(new qx.ui.layout.VBox());

            this.textArea = new qx.ui.form.TextArea()
                .set({width:400, height:300, readOnly: true});
            cnt.add(this.textArea);

            
            this.submitButton =  new qx.ui.form.Button("Оплатить", "icon/32/actions/dialog-apply.png");
            this.submitButton.addListener("execute", this.__pay, this);
            
            this.addbuttonRow(cnt);

            this.controller.placeForm(cnt);
        },

        __pay: function(){
            this.__buildPayController();
        },

        __payController: function(model){
            var biz = this.controller.biz;
            return new zqr.view.Controller.FormController(biz, this.row, model, "")
        },
                
        __buildPayController: function(){
            var model = {
                "type" : "form",
                "readOnly" : true,
                "controller" : "billPayForm",
                "toolbar" : [
                    {"type" : "back"}
                ]
            };
            var controller = this.__payController(model)
            this.controller.biz.setController(controller);
        },

        __getUid: function(bill){
            var preffix = bill.uid_type;
            if("" == preffix) {
                preffix = "rc-"
            }
            var result = (preffix + bill.uid);
            return "Uid: " +  result  + "\n";
        },

        __getSum: function(bill){
            var result = bill.sum + " р.";
            return "Cумма: " +  result  + "\n";
        },

        __getProductTypeName: function(bill){
            var txt = "неопределено";
            switch(bill.product_type_name) {
                case "":
                    txt = "пополнение кошелька";
                    break;
                case "acv_video":
                    var product_id = bill.product_id;
                    var product_name = bill.acv_video.name;
                    txt = "видео-реклама (" + product_name + ") ";
                    break;
                case "donation":
                    txt = "пожертвование";
                    break;
            }
            var result = txt;
            return "Тип: " + result  + "\n";
        },

        __getProductTypeDescription: function(bill){
            var result = bill.product_type_description
            if("" != result)
                return "Описание: " + result + "\n";
            return "";
        },
                
        /**
            Заполняет форму
        **/
        fillForm : function(result) {

            var bill = result.value;
            console.log("bill = ", bill);

            var txt = "";
            txt += this.__getUid(bill);
            txt += this.__getSum(bill) ;
            txt += this.__getProductTypeName(bill);
            txt += this.__getProductTypeDescription(bill);

            this.textArea.setValue(txt);

            if("" != result.value.paydate){
                this.submitButton.setEnabled(false);
            }
            
            
            return true;
        }
        
    }
});

