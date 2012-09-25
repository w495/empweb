/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("zqr.view.Form.AcvVideoCreateMaster.SubnetTargeting",
{
    extend : zqr.view.Form.AcvVideoCreateMaster.BasePage,
    
    construct : function(uReq, Row, Options) {
        this.base(arguments, uReq, Row, Options);
    },

    statics : {
        DATE_PAST_OFFSET : 0,
        DATE_FUTURE_OFFSET : 1,
        DATE_DELTA : 1
    },

    members : {
        
        /* Upload request берется из конструктора */
        uReq : null,
        
        /* Download request делаем сами*/
        dReq : null,
        
        /**
         * Download  request config
         *
         * Предполагается, что загружать данные каждая страница
         * мастера будет самостоятельно, а вот выгружаться на сервер они будут
         * одним запросом.
         * 
        **/
        drc : {             
            url: "/get-acv-video/common",
            method: "GET",                  // POST \ GET
            mimetype: "application/json"    // application/json
        },
        
        getComposite : function(){
            return this.composite;
        },

        /**
         * Поля формы.
         * Вообще, учитывая, богатсво форм они могут не понадобиться.
        **/
        inp : {
            Snet_ufwich :null
        },
        
        // -------------------
        textfield1 : null,
        textfield2 : null,
        
        buildForm : function(){
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            
            this.composite  = new qx.ui.container.Composite(new qx.ui.layout.VBox());
            this.composite.setWidth(zqr.Config.MASTER_FORM_WIDTH);

            this.boxSnet = new qx.ui.groupbox.CheckGroupBox("Таргетирование по подсетям").set({
                toolTip: new qx.ui.tooltip.ToolTip("Оставьте комментарий, если хотите показывать ролик только в указанных подсетям.")
            });

            var layout = new qx.ui.layout.Grid(2, 5);
            layout.setColumnFlex(1, 1);
            layout.setColumnAlign(0, "right", "top");
            
            this.boxSnet.setValue(false);
            
            this.boxSnet.setLayout(layout);

            this.inp.Snet_ufwich = new qx.ui.form.TextArea()
                .set({
                    placeholder: "Посеть",
                    required:true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Посеть")
                });


            var vertical_offset = -1;

            this.boxSnet.add(new qx.ui.basic.Label().set({value: "Описание",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.boxSnet.add(this.inp.Snet_ufwich,   {row:vertical_offset, column:1});


            this.composite.add(this.boxSnet);
                   
            this.inp.Snet_ufwich.focus();
            return this.composite;
        },
        
        _onLoadFormDataCompl : function(response) {
            var result = response.getContent();
            if (false == zqr.util.errors.process(this, result))
                return false;
            this.fillForm(result);
            return true;
        },
        
        /**
            Заполняет форму полученными данными.
        **/
        fillForm : function(data) {
//             for(var fieldName in this.inp){
//                 var item = fieldName.toLowerCase();
//                 this.inp[fieldName].setValue(data.value[item]);
//             }
        },

        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;
            return flag;
        },

        /**
            Применив некоторые преобразования <<загружает>> данные на сервер
        **/
        saveData : function(e) {
            this.base(arguments, e);
            
            var formIsValid = this.validateForm();
            if(formIsValid){
                var res = {}
                for(var fieldName in this.inp){
                        item = fieldName.toLowerCase()
                        res[item] = this.inp[fieldName].getValue();
                }  
                for(var item in res){
                    console.log(item, res[item]);
                    this.uReq.setParameter(item, res[item], true);
                }
            }
            return formIsValid;
        }
    }
});


