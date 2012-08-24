
/* ************************************************************************

************************************************************************ */

qx.Class.define("zqr.view.Controller.FormController",
{
    //extend : qx.ui.groupbox.GroupBox,
    extend : qx.ui.container.Composite,

    construct : function(biz, row, fmodel, oname) {
        this.biz = biz;
        this.base(arguments, new qx.ui.layout.VBox());

        this.toolbar = new zqr.view.ToolBar(this.biz, this, fmodel.toolbar);
        this.add(this.toolbar);

        this.formBox = new qx.ui.container.Composite();


        var legend = this.makeLegend(row, fmodel, oname);

        this.formCont = new qx.ui.groupbox.GroupBox(legend);
        //this.formCont = new qx.ui.container.Composite();
        this.formCont.setLayout(new qx.ui.layout.VBox());

        if(fmodel.centerForm == undefined || fmodel.centerForm==true) {
            var d = new qx.ui.layout.Dock();
            d.setSort("y");
            this.formBox.setLayout(d);
            this.set({allowGrowX : true});

            var w1 = new qx.ui.core.Widget();
            var w2 = new qx.ui.core.Widget();
            var w3 = new qx.ui.core.Widget();
            var w4 = new qx.ui.core.Widget();
            this.formBox.add(w2, {edge:"west", flex:1})
            this.formBox.add(w3, {edge:"south", flex:1});
            this.formBox.add(w4, {edge:"east", flex:1});
            this.formBox.add(this.formCont, {edge:"center", flex:0});
        }
        else {
            this.formBox.setLayout(new qx.ui.layout.VBox());
            this.formBox.add(this.formCont, {flex:1});
        }

        this.add(this.formBox, {flex:1});
        if(fmodel.controller == undefined) {
            this.form = new zqr.view.Form.GenericForm(this, row, fmodel);
        }
        else {
            this.form = zqr.view.StaticForms[fmodel.controller](this, row, fmodel);
        }
    },

    members : {

        __itext:function(oname) {
            return "Информация " + oname;
        },

        __ctext:function(oname) {
            return "Создание " + oname;
        },

        __etext:function(oname) {
            return "Редактирование " + oname;
        },

        makeLegend :function(row, fmodel, oname) {
            if(fmodel.legend)
                return fmodel.legend;

            if(row == undefined || row.isNew)
                return this.__ctext(oname);

            if(!(fmodel.readOnly))
                return this.__etext(oname);

            return this.__itext(oname);
        },


        makeClosable: function(callback, owner){
            if(this.biz.makeClosable){
                if(!(owner))
                    owner = this
                this.biz.makeClosable(callback, owner);
            }
        },

        setLegend :function(legend) {
           this.formCont.setLegend(legend);
        },

        getLegend :function() {
           return this.formCont.getLegend();
        },

        placeForm :function(f) {
           this.__f = f;
           this.formCont.add(f, {flex:1});
        },

        displaceForm :function(f) {
            if(f)
                this.formCont.remove(f);
            this.formCont.remove(this.__f);
        },


        submited : function(result) {
            this.back();
        },

        back : function() {
            this.biz.back();
        },

        onCancelClick : function() {
            return this.back();
        },

        close : function() {
            return this.onCancelClick();
        },

        getExtraParams : function(params) {
            return params;
        },

        disableForm : function() {
            this.formCont.setEnabled(false);
            this.biz.show_global_pb();
        },

        enableForm : function() {
            this.formCont.setEnabled(true);
            this.biz.hide_global_pb();
        },

        refresh : function() {
            console.log("zqr.view.Controller.FormController");
            if(this.form.refresh)
                this.form.refresh();
        }


    }
});

