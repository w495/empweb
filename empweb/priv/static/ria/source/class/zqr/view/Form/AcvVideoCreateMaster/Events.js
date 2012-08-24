/* ************************************************************************
#asset(qx/icon/Tango/16/actions/list-add.png)
#asset(qx/icon/Tango/16/actions/list-remove.png)

************************************************************************ */

qx.Class.define("zqr.view.Form.AcvVideoCreateMaster.Events",
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
        
        getWidget : function(){
            return this.composite;
        },

        /**
         * Поля формы.
         * Вообще, учитывая, богатсво форм они могут не понадобиться.
        **/
        inp : {

        },

        // -------------------
        url1 : null,
        url2 : null,
        
        buildForm : function(){
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            this.composite  = new qx.ui.container.Composite().set({
                width : zqr.Config.MASTER_FORM_WIDTH,
                layout: new qx.ui.layout.VBox()
            });

            this.econtroller = new qx.ui.container.Composite(new qx.ui.layout.VBox());
            this.econtroller.add(
                /**
                    Добавляем виджет (Addline) с кнопкой [+|-],
                        owner кто обладает этим объектом,
                            чтобы создавать копии исходного, как детей owner;
                        insider функцию создания внутренностей Addline,
                            c контекстом ее выполнения;
                        layout позиционирования объектов внутри Addline
                **/
                new zqr.view.Addline({
                    owner: this.econtroller,
                    insider: {
                        fun: this._buildElineCont,
                        context: this
                    },
                    layout: new qx.ui.layout.Grid(2, 1)
                        .setColumnFlex(0, 1)
                        .setColumnAlign(0, "right", "middle")
                }).getWidget()
            );

            var ebox = new qx.ui.groupbox.CheckGroupBox("Учет событий").set({
                toolTip: new qx.ui.tooltip.ToolTip("Учет событий"),
                layout: new qx.ui.layout.HBox(),
                value : false
            });
            var escroll = new qx.ui.container.Scroll().set({
                width : zqr.Config.MASTER_FORM_WIDTH,
                height: 300
            });
            
            escroll.add(this.econtroller);
            ebox.add(escroll);
            this.composite.add(ebox);

            return this.composite;
        },

        _buildElineCont : function() {
            // ---------------------------------------------------------------
            var ta = new qx.ui.container.Composite(new qx.ui.layout.VBox());
            var type = new qx.ui.form.SelectBox();
            this._fillSelect(type, [
                {name:"Show — начало показа рекламы",       value: "show"    },
                {name:"Fullshow — конец показа рекламы",    value: "fullshow"},
                {name:"Click — клик на рекламный креатив",  value: "click"   }
            ], "name", "value");
            ta.add(type);
            // ---------------------------------------------------------------
            var uc = new qx.ui.container.Composite(new qx.ui.layout.VBox());
            var url = new qx.ui.form.TextArea("").set({
                placeholder: "Ссылка для событий",
                height: 64
            });
            var comment  = new qx.ui.form.TextArea("").set({
                placeholder: "Комментарий, по событиям рекламы",
                height: 64
            });
            uc.add(url);
            uc.add(comment);
            // ---------------------------------------------------------------
            return {
                left: ta,
                bottom: uc,
                getValue: function(){
                    return {
                        url: url.getValue(),
                        type: type.getSelection()[0].getModel(),
                        action: "get",
                        comment: comment.getValue()
                    }
                }
            }
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
                this.uReq.setParameter("event_list",
                    this.econtroller.getChildren().map(function(i){
                        return qx.util.Serializer.toJson(i.getValue());
                    }),
                    true
                );
            }

            return formIsValid;
        }
    }
});


