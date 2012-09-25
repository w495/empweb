/* ************************************************************************

************************************************************************ */

qx.Class.define("zqr.view.GenericTableModel",
{
    //extend : qx.ui.table.model.Remote,
    extend : qx.ui.table.model.Simple,
 
    construct : function(cntl, tab, tabDescription) {
        this.base(arguments);
        this.cntl = cntl;
        this.tab = tab;
        this.data = {};
        this.createColumns(tabDescription);
        //this.asc = tabDescription.ascending || true;
        this.asc = tabDescription.ascending;
        this.addListener("sorted", this.onSortChange, this);
        //this.loadRowData(this.tabDescription.filter.url);
    },

    statics:
    {
        sph: 3600,
        spm: 60,

        mktd: function(td) {
            var rtd = zqr.util.utils.clone(td);
            rtd.columns = [];
            return rtd;
        }
    },

    members :
    {
        cntl            : null,
        tab             : null,
        data            : null,
        asc             : null,

        tabDescription  : null,
        sortIndex       : null,
        columnsDesc     : null,
        sortColumn      : null,
        sortIndex       : null,

        findSortColumnName : function() {
            var ret = null;
            var i = 0;
            for(var k in this.columnsDesc) {
                if(i == this.sortIndex) {
                    ret = this.columnsDesc[k];
                    break;
                }
                i++;
            }
            return ret;
        },

        getSortColumnName : function() {
            return this.sortColumn.name;
        },

        getSortDirection : function() {
            if(this.isSortAscending())
                return "asc";
            return "desc";
        },

        onSortChange : function(e) {
            this.sortIndex = this.getSortColumnIndex();
            this.sortColumn = this.findSortColumnName();
            this.asc = this.isSortAscending();

            this.cntl.onSortChange();
        },

        createColumns : function(tabDescription) {
            if(null == this.tabDescription)
                this.tabDescription = zqr.view.GenericTableModel.mktd(tabDescription);

            this.sortIndex = 0;
            this.columnsDesc = {};

            var colNames = [];
            var colAlias = [];

            for(var i=0; i<tabDescription.columns.length; i++) {
                var item = tabDescription.columns[i];
                if(item.hidden){
                    continue;
                }
                item.columnIndex = i;
                colNames.push(item.name);
                colAlias.push(item.alias);
                this.columnsDesc[item.name] = item;
                this.setColumnSortable(i, item.sortable);
                this.tabDescription.columns.push(item);
                
                if(tabDescription.sort == item.name) {
                    this.sortColumn = item;
                    this.sortIndex = i;
                }
            }
            this.setColumns(colAlias, colNames);
        },

        mySort: function(row1, row2, sortby, ascending){
            var sec1 = Math.round(row1[sortby].substr(row1[sortby].lastIndexOf(":")+1));
            var min1 = Math.round( row1[sortby].substr(row1[sortby].indexOf(":")+1,2) );
            var hrs1 = Math.round( row1[sortby].substr(0,row1[sortby].indexOf(":")) );
            var sec2 = Math.round(row2[sortby].substr(row2[sortby].lastIndexOf(":")+1));
            var min2 = Math.round( row2[sortby].substr(row2[sortby].indexOf(":")+1,2) );
            var hrs2 = Math.round( row2[sortby].substr(0,row2[sortby].indexOf(":")) );
            var r1 = hrs1*zqr.view.GenericTableModel.sph+min1*zqr.view.GenericTableModel.spm+sec1;
            var r2 = hrs2*zqr.view.GenericTableModel.sph+min2*zqr.view.GenericTableModel.spm+sec2;
            var r = -ascending;
            if(r1 > r2)
                r = ascending;
            if(r1 == r2)
                r = 0;
            return r;
        },
        
        updateDataCellRenderers : function() {
            var tcm = this.tab.getTableColumnModel();
            var resizeBehavior = tcm.getBehavior();
            for(var i=0; i<this.tabDescription.columns.length; i++) {
                var item = this.tabDescription.columns[i];
                switch(item.type) {
                    case "bool-cb" :
                        console.log("<bool-cb>");
                        tcm.setDataCellRenderer(i, new qx.ui.table.cellrenderer.Boolean());
                        this.setColumnEditable(i,true);
                        tcm.setCellEditorFactory(i, new qx.ui.table.celleditor.CheckBox());
                        break;
                    case "bool" :
                        console.log("<bool>");
                        tcm.setDataCellRenderer(i, new qx.ui.table.cellrenderer.Boolean());
                        break;
                    case "date" :
                        tcm.setDataCellRenderer(i, new qx.ui.table.cellrenderer.Date());
                        break;
                    case "float":
                        var rnd = new qx.ui.table.cellrenderer.Number();
                        if(item.maximumFractionDigit != undefined) {
                            var fmt = new qx.util.format.NumberFormat();
                            fmt.setMaximumFractionDigits(item.maximumFractionDigit);
                            rnd.setNumberFormat(fmt);
                        }
                        tcm.setDataCellRenderer(i, rnd);
                        break;
                    case "sec":
                        var own = this;
                        this.setSortMethods(i, {
                            "ascending": function(row1, row2) { 
                                return own.mySort(row1, row2, arguments.callee.columnIndex, 1)
                            }, 
                            "descending": function(row1, row2) { 
                                return own.mySort(row1, row2, arguments.callee.columnIndex, -1)
                            }
                        });
                        break;
                }
                if(item.width != undefined)
                    resizeBehavior.setWidth(i, item.width);
            }
        },

        getToolTip : function(column, row) {
            return "";
        },


        onRowDataIncome : function(result) {
            var sortColumnIndex=0;
            var rows = [];
            this.data = {};

            if (result.ok)
                 result.values = result.ok;

            if (!result.values)
                result.values = result;

            for(var i=0; i < result.values.length; i++) {
                var Row = result.values[i];
                var rData = [];
                for(var j in this.columnsDesc) {
                    var desc = this.columnsDesc[j];
                    if(Row[desc.name] != undefined) {
                        var rowVal = Row[desc.name];
                        switch(desc.type) {
                            case "bool-cb":
                                if(0 == rowVal){
                                    rowVal = false;
                                }
                                if(1 == rowVal){
                                    rowVal = true;
                                }
                                break;
                            case "bool":
                                if(0 == rowVal){
                                    rowVal = false;
                                }
                                if(1 == rowVal){
                                    rowVal = true;
                                }
                                break;
                            case "sec":
                                rowVal = rowVal *1;
                                var hour = Math.floor(rowVal/zqr.view.GenericTableModel.sph);
                                rowVal = rowVal%zqr.view.GenericTableModel.sph;
                                var min = Math.floor(rowVal / zqr.view.GenericTableModel.spm);
                                var sec = (rowVal % zqr.view.GenericTableModel.spm).toFixed(0);
                                rowVal = zqr.util.utils.formatTime(hour, min, sec);
                                break;
                            case "float":
                                rowVal = rowVal *1;
                                if(desc.tofixed)
                                    rowVal = rowVal.toFixed(desc.tofixed);
                                break;
                            case "erl_datetime":
                                var dt = zqr.util.utils.getDateLocal(rowVal, 0);
                                rowVal = zqr.util.utils.formatJsDateTime(dt);
                                break;
                            case "erl_date":
                                var dt = zqr.util.utils.getDateLocal(rowVal, 0);
                                rowVal = zqr.util.utils.formatJsDate(dt);
                                break;
                            case "erl_datetime_nullable":
                                if("" != rowVal){
                                    var dt = zqr.util.utils.getDateLocal(rowVal, 0);
                                    rowVal = zqr.util.utils.formatJsDateTime(dt);
                                } else {
                                    rowVal = "отсутвует"
                                }
                                break;
                            case "erl_datetime_utc":
                                var dt = zqr.util.utils.getDate(rowVal, 0);
                                rowVal = zqr.util.utils.formatJsDateTime(dt);
                                break;
                            case "percent":
                                rowVal = rowVal * 100;
                                if(desc.tofixed)
                                    rowVal = rowVal.toFixed(desc.tofixed);
                                rowVal += "%";
                                break;
                            case "checkbox":
                                //rowVal = "<span style=\"color:red\">" + rowVal + "</span>";   
                                break;
                            case "enum":
                                if(desc.dict){
                                    var old_rowVal = rowVal
                                    rowVal = desc.dict[old_rowVal];
                                    if(!rowVal)
                                        rowVal = desc.dict['default'] + " (" + old_rowVal + ")";
                                }
                                break;
                            default:
                                break;
                        }
                    }
                    else
                        rowVal = "---";
                    rData.push(rowVal);
                }
                this.data[rData] = Row;
                rows.push(rData);
            }
 //           this.data = result.values;
            this.addRows(rows, 0);
            if(result.sort != undefined && this.columnsDesc[result.sort] != undefined)
                this.sortIndex = this.columnsDesc[result.sort].columnIndex;

            
            if(result.ascending != undefined)
                this.asc = result.ascending;
            this.removeListener("sorted", this.onSortChange, this);
            this.sortByColumn(this.sortIndex, this.asc);
            this.addListener("sorted", this.onSortChange, this);
        },

        clear : function() {
            this.removeRows(0, this.getRowCount());
        }
    }
});


