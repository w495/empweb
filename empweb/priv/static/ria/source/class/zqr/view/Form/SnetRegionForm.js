/* ************************************************************************

    Класс описания формы по созданию группы
    ИСПОЛЬЗОВАНИЕ:
        Администрировнаие > группы пользователей | [Создать]

************************************************************************ */


qx.Class.define("zqr.view.Form.SnetRegionForm",
{
    extend : zqr.view.Form.BaseForm,

    construct : function(controller, Row) {
        console.log("zqr.view.Form.SnetRegionForm");
        this.row = {id : null};
        if(Row && Row.id){
            this.row.id = Row.id;
        }else{
            this.row.id = null;
        }
        console.log("this.row = ", this.row)
        this.base(arguments, controller, Row);
    },

    members : {

        /* Upload request config */
        urc : {
            url: "/update-snet-region",
            method: "POST",
            mimetype: "application/json"
        },

        /* Download  request config */
        drc : {
            url: "/get-snet-region-info",
            method: "GET",
            mimetype: "application/json"
        },

        /* Поля формы */
        inp : {
            Id           : null,
            Name         : null,
            Alias         : null,
            Description  : null
        },

        buildForm : function() {
            this.base(arguments);
            
            this.inp.Id = new qx.ui.form.TextField().set({
                enabled: false
            });
            if(this.row.id){
                this.inp.Id.setValue(this.row.id);
            }
            

            this.inp.Name = new qx.ui.form.TextField();
            this.inp.Alias = new qx.ui.form.TextField();
            this.inp.Description  = new qx.ui.form.TextArea().set({
                height: 200,
                width: 150
            });

            var layout = new qx.ui.layout.Grid(12, 6);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            
            var cnt = new qx.ui.container.Composite(layout);

            var l1 = new qx.ui.basic.Label("Общая информация").set({
                font:"bold",
                alignX:"left"
            });

            cnt.add(l1, {row:0, column:0, colSpan:2});

            var vertical_offset = 0;
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            
            cnt.add(new qx.ui.basic.Label("#"),
                {row:++vertical_offset, column:0});
            cnt.add(this.inp.Id, {row:vertical_offset, column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Имя" + RFM,  rich : true}),
                {row:++vertical_offset, column:0});
            cnt.add(this.inp.Name,{row:vertical_offset, column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Описание"+ RFM,  rich : true}),
                {row:++vertical_offset, column:0});
            cnt.add(this.inp.Description,{row:vertical_offset, column:1});
            var region_id = this.row.id;
            if(region_id){
                var l2 = new qx.ui.basic.Label("Права");
                cnt.add(l2, {row:0, column:2});
                var model = {
                    toolbar : [
                        {type : "button",
                            name : "Обновить",
                            action : "/get-snets",
                            icon : "icon/32/actions/view-refresh.png",
                            specParam: "tab-change-state-action"
                        },
                        {type : "button",
                            name : "Создать",
                            action : function(){
                                /**
                                    В данном случае, мы передаем не строку,
                                    а функцию, которая выполняется на момент
                                    нажатия кнопки. Это маленький хак,
                                    но это позволяет нам сохранить текущее
                                    положение region_id.
                                **/
                                this.biz.onAction(
                                    {isNew:true,region_id:region_id},
                                    this.filterForm.getValues(),
                                    "resource/zqr/descr/snet/snet-form.json");
                            },
                            icon : "icon/32/actions/document-new.png"
                        },
                        {type : "button",
                            name : "Редактировать",
                            action : "resource/zqr/descr/snet/snet-form.json",
                            icon : "icon/32/actions/zoom-in.png",
                            specParam: "tab-row"
                        },
                        {type : "button",
                            name : "Удалить",
                            action : "/delete-snet",
                            icon : "icon/32/actions/edit-delete.png",
                            specParam: "tab-row-action",
                            confirmMsg: "Уверены ли вы в том, что хотите удалить?"
                        }
                    ],
                    columns : [
                        {name : "id",
                            alias: "#",
                            type : "float",
                            width: "5%",
                            sortable : true},
                        {name : "ip",
                            alias : "ip",
                            type : "string",
                            sortable : true},
                        {name : "name",
                            alias : "name",
                            type : "string",
                            sortable : true},
                        {name : "description",
                            alias : "description",
                            type : "string",
                            sortable : true}
                    ],
                    filter : {
                        region_id: region_id,
                        submit_url : "/get-snets"
                    },
                    sort : "id",
                    ascending : false,
                    //index_name : ["id"],
                    dblclick_action : "resource/zqr/descr/snet/snet-form.json"
                };
                var snetTab = new zqr.view.Controller.TabController(this, undefined, undefined, model).set({
                    width:600,
                    height:200
                });
                this.snetTab = snetTab;
                cnt.add(snetTab, {row:1, column:2, rowSpan: vertical_offset});
            }
            this.addbuttonRow(cnt, ++vertical_offset);
            this.controller.placeForm(cnt);
            this.inp.Name.focus();
        },

        submit : function(req) {
            if(this.row.id)
                return this._submit_close(req);
            return this._submit_noclose(req, this, function(res){
                this.controller.displaceForm();
                if(res.value){
                    this.controller.form  =
                        new zqr.view.Form.SnetRegionForm(
                            this.controller,
                            {id:"" + res.value}
                        );
                }
            });
        },

        refresh: function() {
            console.log("zqr.view.Form.SnetRegionForm");
            if(this.snetTab && this.snetTab.refresh)
                this.snetTab.refresh();
        },

        hide_global_pb : function() {
            this.controller.biz.hide_global_pb ()
        },

        show_global_pb : function() {
            this.controller.biz.show_global_pb()
        },

        onAction: function(x, y, z){
            return this.controller.biz.onAction(x, y, z);
        },

        validateForm : function() {
            var flag = true;
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Name);
            return flag
        }

    }
});

