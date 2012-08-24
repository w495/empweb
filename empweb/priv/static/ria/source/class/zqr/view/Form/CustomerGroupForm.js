/* ************************************************************************

    Класс описания формы по созданию группы
    ИСПОЛЬЗОВАНИЕ:
        Администрировнаие > группы пользователей | [Создать]

************************************************************************ */


qx.Class.define("zqr.view.Form.CustomerGroupForm",
{
    extend : zqr.view.Form.BaseForm,

    construct : function(controller, Row) {            
        this.base(arguments, controller, Row);
    },

    members : {

        /* Upload request config */
        urc : {             
            url: "/update-customer-group",
            method: "POST",
            mimetype: "application/json"
        },

        /* Download  request config */
        drc : {             
            url: "/get-customer-group-info",
            method: "GET",
            mimetype: "application/json"
        },

        /* Поля формы */
        inp : {
            Id           : null,
            Name         : null,
            Description  : null
        },

        /* Список разрешений справа */
        
        permissionList: null,
        permissionListOptions : {
            url:            "/get-permissions",
            labelFieldName: "name",
            descrFieldName: "description"
        },

        /**
         *
         * TODO: отрефакторить, так чтобы было мало букаф
         */
        
        buildForm : function() {
            this.base(arguments);
            
            this.inp.Id           = new qx.ui.form.TextField();
            this.inp.Name         = new qx.ui.form.TextField();
            this.inp.Description  = new qx.ui.form.TextField();
            
            this.permissionList = new zqr.view.SelListTree(this,
                this.permissionListOptions.url,
                this.permissionListOptions.labelFieldName,
                this.permissionListOptions.descrFieldName,
                {descrlen: 350}
            );
        
            var gCnt = new qx.ui.container.Composite(new qx.ui.layout.HBox(12));
            var layout = new qx.ui.layout.Grid(12, 6);
            var cnt = new qx.ui.container.Composite(layout);

            var cnt_right = new qx.ui.container.Composite(new qx.ui.layout.VBox(6));

            gCnt.add(cnt);
            gCnt.add(cnt_right);

            this.inp.Id.setEnabled(false);
            this.inp.Description.setWidth(150);

            this.permissionList.setWidth(500);

            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");

            var l1 = new qx.ui.basic.Label("Общая информация");
            l1.setFont("bold");
            l1.setAlignX("left");

            cnt.add(l1, {row:0, column:0, colSpan:2});

            var vertical_offset = 1;
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            
            cnt.add(new qx.ui.basic.Label("#"),                  {row:++vertical_offset, column:0});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Имя"               + RFM,  rich : true}),  {row:++vertical_offset, column:0});
            cnt.add(new qx.ui.basic.Label().set({value: "Описание"               + RFM,  rich : true}),  {row:++vertical_offset, column:0});

                            
            vertical_offset = 1;

            cnt.add(this.inp.Id,             {row:++vertical_offset, column:1});
            cnt.add(this.inp.Name,           {row:++vertical_offset, column:1});
            cnt.add(this.inp.Description,    {row:++vertical_offset, column:1});

            var l2 = new qx.ui.basic.Label("Права");
            l2.setFont("bold");

            cnt_right.add(l2);
            cnt_right.add(this.permissionList);
            var buttonRow = new qx.ui.container.Composite();
            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);
            buttonRow.add(this.submitButton);
            buttonRow.add(this.cancelButton);

            cnt_right.add(buttonRow);

            this.controller.placeForm(gCnt);
            this.inp.Name.focus();

        },

        validateForm : function() {
            var flag = true;
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Name);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Description);
            return flag
        },

        /**
            Формирует данные для сервера
        **/
        _uploadData : function(e) {
            this.base(arguments, e);

            var permIdList = this.permissionList.getSelectedId();

            if(this.validateForm()) {
                this.uReq.setParameter("permissions", permIdList, true);
            }
        },

        fillForm : function(data) {
            this.base(arguments, data);
            this.permissionList.setChecked(data.permissions)
        }
    }
});

