
/* ************************************************************************
#asset(qx/icon/Tango/16/actions/go-next.png)
#asset(qx/icon/Tango/16/actions/go-previous.png)
************************************************************************ */

qx.Class.define("zqr.view.Pager",
{
    extend: qx.ui.container.Composite,
 
    construct : function(cntl, pageSize) {
        var l = new qx.ui.layout.HBox();
        l.setAlignY("middle");
        this.base(arguments, l);
        this.cntl = cntl;
        this.pageSize = pageSize;

        
        this.prevBtn = new qx.ui.form.Button("", "icon/16/actions/go-previous.png");
        this.prevBtn.setToolTip(new qx.ui.tooltip.ToolTip("Предыдущая страница"));
        this.prevBtn.addListener("execute", this._onPrevBtn, this);
        this.prevBtn.setWidth(30);

        this.nextBtn = new qx.ui.form.Button("", "icon/16/actions/go-next.png");
        this.nextBtn.setToolTip(new qx.ui.tooltip.ToolTip("Следующая страница"));
        this.nextBtn.addListener("execute", this._onNextBtn, this);
        this.nextBtn.setWidth(30);

        this.pNum = new qx.ui.form.TextField();
        this.pNum.setFilter(/[0-9]/);
        this.pNum.addListener("changeValue", this._onPNumChange, this);
        this.pNum.setWidth(25);
        
        this.add(new qx.ui.basic.Label("Страница:  "));
        this.add(this.prevBtn);
        this.add(this.pNum);
        this.add(this.nextBtn);

        this.offset = 0;
        this.setEnabled(false);
    },

    members : {
        _onPNumChange : function(e) {
            var val = this.pNum.getValue();
            if(val < 0) {
                this.pNum.setValue("0");
                val = 0;
            }
            this.offset = val * this.pageSize;
            this.setEnabled(false);
            this.cntl.onPageChange();
        },

        _onPrevBtn : function(e) {
            if(this.offset - this.pageSize > 0)
                this.offset = this.offset - this.pageSize;
            this.setEnabled(false);
            this.cntl.onPageChange();
        },

        _onNextBtn : function(e) {
            this.offset += this.pageSize;
            this.setEnabled(false);
            this.cntl.onPageChange();
        },

        getOffset : function() {
            return this.offset;
        },

        getLimit : function() {
            return this.pageSize;
        },

        calcPage : function() {
            return Math.floor(this.offset / this.pageSize) + 1;
        },

        updatePager : function(val) {
            this.offset = val.offset;
            this.count = val.values.length;
            this.pNum.removeListener("changeValue", this._onPNumChange, this);
            this.pNum.setValue("" + this.calcPage());
            this.pNum.addListener("changeValue", this._onPNumChange, this);
            this.setEnabled(true);
        }
    }
});


