/* ************************************************************************



************************************************************************ */


qx.Class.define("zqr.view.Form.ConfigForm",
{
    extend : zqr.view.Form.BaseForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row);
        this.submitButton.addListener("execute", function(){
            var win = zqr.util.utils.infoWindow("Конфигурация изменена");
            this.controller.biz.getRoot().add(win, {
                left : win.l*1,
                top  : win.t*1
            });
        }, this);
    },

    members : {

        urc : {  // upload request config
            url: "/update-config",
            method: "POST",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-config",
            method: "GET",
            mimetype: "application/json"
        },
        
        /* Поля формы */
        inp : {
            Id: null,
            Acv_video_loadnext              : null
        },


        buildForm : function() {
            this.base(arguments);

            this.inp.Id = new qx.ui.form.TextField();
            this.inp.Acv_video_loadnext
                = new qx.ui.form.Spinner(0, 100, 1152921504606846976);

            var layout = new qx.ui.layout.Grid(12, 6);
            var cnt = new qx.ui.container.Composite(layout);


            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");

            var l1 = new qx.ui.basic.Label("Параметры системы");
            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});

            var vertical_offset = 0;
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            cnt.add(new qx.ui.basic.Label()
                    .set({value: "Acv_video_loadnext" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Acv_video_loadnext,
                    {row:vertical_offset , column:1});

            this.cancelButton = null;
            this.addbuttonRow(cnt, ++vertical_offset);
            this.controller.placeForm(cnt);
        },

        fillForm : function(data) {


            var dholder = data.values[0];

            this.inp.Id.setValue(dholder.id);
            this.inp.Acv_video_loadnext.setValue(parseInt(dholder.acv_video_loadnext));
        }

    }
});

