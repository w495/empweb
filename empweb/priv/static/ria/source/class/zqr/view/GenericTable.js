

qx.Class.define("zqr.view.GenericTable",
{
    extend : qx.ui.table.Table,
 
    construct : function(cntl, tabDescription) {
        this.cntl=cntl;
        this.biz = cntl.biz;
        this.tabModel = tabDescription;
        var custom = {
            tableColumnModel : function(obj) {
                return new qx.ui.table.columnmodel.Resize(obj);
            }
        };

        
    
        this.model = new zqr.view.GenericTableModel(cntl, this, tabDescription);
        this.base(arguments, this.model, custom);
        // this.base(arguments, this.model);

        /*
        this.width = 600;
        this.height = 400;
        this.decorator = null;
        */
        
        console.log("thisthis --------- >>>>", this);
        
        this.setStatusBarVisible(false); // убрать rows в гридах
        this.setColumnVisibilityButtonVisible(false);

        this.addListener("cellClick", this._onCellClick, this);
        this.addListener("cellDblclick", this._onCellDblClick, this);

        this.tcm = this.getTableColumnModel();
        this.resizeBehavior = this.tcm.getBehavior();
        
        /*
            for(var i=0; i<this.tabModel.columns.length; i++) {
                var I = this.tabModel.columns[i];
                switch(I.type) {
                    case "float":
                        console.log("I = ", I);
                        //this.resizeBehavior.setWidth(i, "50%");
                        this.resizeBehavior.setMaxWidth(i, 100);
                        //this.resizeBehavior.setMaxWidth(i, 320);
                        break;
                    default:
                        this.resizeBehavior.setMaxWidth(i, 100);
                        break;
                }
            }
         */
            

        
        // Tooltip
        this.tooltipTimer = new qx.event.Timer(1000);
        this.tooltipTimer.addListener("interval", this._onTooltipTimer, this);
 
        this.tooltip = new qx.ui.tooltip.ToolTip("");
        this.tooltip.setRich(true);
        this.setToolTip(this.tooltip);
        this.tooltip.setShowTimeout(100000);
        this.addListener("mousemove", this.showTooltip, this);
        this.addListener("mouseout", function(e) {
            if(this.tooltip.isHidden() == false)
                this.tooltip.hide();
            this.tooltipTimer.stop();
        }, this);
        
        
        this.setWidth(900);
        this.setHeight(200);
    },

    members : {

        _onTooltipTimer : function(e) {
            this.tooltipTimer.stop();
            var pageX = this.ttEvent.getDocumentLeft();
            var pageY = this.ttEvent.getDocumentTop();

            var tooltipValue = null;
            var sc = this.getTablePaneScrollerAtPageX(pageX);
            if (sc != null) {
                var tm = this.getTableModel();
                if (tm != null) {
                    var row = sc._getRowForPagePos(pageX, pageY);
                    var col = sc._getColumnForPageX(pageX); /**/
                    if (row >= 0 && col >= 0) {
                        tooltipValue = tm.getToolTip(col, row);
                    }
                }
            }

            if (tooltipValue != null && tooltipValue != "") {
                this.tooltip.placeToMouse(this.ttEvent);
                this.tooltip.setLabel(tooltipValue);
                this.tooltip.show();
            } 
        },

        showTooltip : function(e) {
            if(this.tooltip.isHidden() == false)
                this.tooltip.hide();
            this.tooltipTimer.stop();
            this.ttEvent = e;
            this.tooltipTimer.start();
        },

        _onCellClick : function(e) {
            this.focus();
        },

        _onCellDblClick : function(e) {
            var id = this.getSelectionModel().getSelectedRanges()[0];
            if(id == undefined)
                return;
            var d = this.model.getData();
            var rowId = d[id.minIndex];
            if(this.model.data[rowId] != undefined && this.tabModel.dblclick_action != undefined) {
                this.cntl.rowDblClick(this.model.data[rowId]);
            }
        },

        getAllSelected : function(e) {
            var ranges = this.getSelectionModel().getSelectedRanges();
            var d = this.model.getData();
            var result = [];
            for(var i=0; i<ranges.length; i++) {
                var R = ranges[i];
                for(var j=R.minIndex; j<R.maxIndex+1; j++) {
                    var rowId = d[j];
                    if(this.model.data[rowId] != undefined)
                        result.push(this.model.data[rowId]);
                }
            }
            return result;
        },

        getSelection : function(e) {
            var id = this.getSelectionModel().getSelectedRanges()[0];
            if(id == undefined)
                return null;
            var d = this.model.getData();
            var rowId = d[id.minIndex];
            if(this.model.data[rowId] != undefined)
                return this.model.data[rowId];
            return null;
        },

        _onCellSelected : function(e) {
            if(this.selectionModel.getSelectedRanges()[0] == undefined)
                return;
            var minIndex = this.selectionModel.getSelectedRanges()[0].minIndex;
            var maxIndex = this.selectionModel.getSelectedRanges()[0].maxIndex;
            alert("min: " + minIndex + " max: " + maxIndex);

        }
    }
});


