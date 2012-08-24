/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("zqr.view.Form.AcvVideoCreateMaster.PlatfromTargeting",
{
    extend : zqr.view.Form.AcvVideoCreateMaster.BasePage,
    
    construct : function(uReq, Row, Options) {
        this.Options = Options;
        this.base(arguments, uReq, Row, Options);
    },

    members : {
        helpUrl : "/docs/offer",

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
            url: "/get-acv-video/platform-targeting",
            method: "GET",                  // POST \ GET
            mimetype: "application/json"    // application/json
        },
        
        platformListOptions: {
            url:            "/get-platforms",
            labelFieldName: "description",
            descrFieldName: "name"
        },
        
        getComposite : function(){
            //return this.composite;
            return this.platfromCat;
        },
        
        buildForm : function(){
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
/*            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Таргетирование по категориям",  font: "bold",
                    alignX: "left", rich : true
                }); */


            this.platfromCat = new qx.ui.groupbox.CheckGroupBox("Таргетирование по платформам").set({
                toolTip: new qx.ui.tooltip.ToolTip("Активируйте, если хотите показывать ролик только в фильмах на определенных устройствах")
            });
            this.platfromCat.setValue(false);

            var layout = new qx.ui.layout.Grid(2, 1);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            
            //this.composite  = new qx.ui.container.Composite (layout);
            this.platfromCat.setLayout(layout);
            
            this.inp.List = new zqr.view.
                Sltdac(
                    this.platformListOptions.url,
                    this.platformListOptions.labelFieldName,
                    this.platformListOptions.descrFieldName,
                    this.Options
                );
            
            var vertical_offset = -1;
            
//            this.platfromCat.add(pageName,
//                {row:++vertical_offset, column:0});
            this.platfromCat.add(this.inp.List,
                {row:++vertical_offset, column:0});
            
            return this.platfromCat;
        },
        
        
        /**
            Заполняет форму полученными данными.
        **/
        fillForm : function(data) {
            var list = [];
            console.log("data = ", data);
            console.log("data.values = ", data.values);
            for(var cat in data.values){
                console.log("cat = ", cat);
                list.push(data.values[cat].cat_id);
            }
            console.log("list = ", list);
            this.inp.List.setChecked(list);
            this.platfromCat.setValue(list.length > 0);
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
            if(this.validateForm()) {
                var list = [];
                if(this.platfromCat.getValue())
                    list = this.inp.List.getSelectedId();
                console.log("list = ", list ),
                this.uReq.setParameter("platform_list", list, true);
            }
            return true;
        }
    }
});

