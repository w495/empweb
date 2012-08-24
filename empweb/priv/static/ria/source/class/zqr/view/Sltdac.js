/* ************************************************************************
 * )(((Sorted (Sel List Tree)) Double) Arrowed) Container)
 * 
 * Оно выглядит так:
 *    --------------------------------
 *    |  [(    text    )]            |
 *    |                              |
 *    |  |--------| [->] |--------|  |
 *    |  |        | [<-] |        |  |
 *    |  |        |      |        |  |
 *    |  |        |      |        |  |
 *    |  |--------|      |--------|  |
 *    |                              |
 *    --------------------------------
 *    
 * ********************************************************************** */

qx.Class.define("zqr.view.Sltdac",
{
    extend : qx.ui.container.Composite,
    
    /**
     *
     *  @param url --- URL запроса.
     *  @param labelFieldName
     *  @param descrFieldName
     *  @param paramdict --- список параметоров url (может быть undefined)
     */
    construct : function(url, labelFieldName, descrFieldName, paramdict) {
        this.base(arguments);
        
        this.tfield = new qx.ui.form.TextField()
            .set({placeholder: "Фраза для поиска в списке"});
        this.ltree = new zqr.view
            .SortedSelListTree(this, url, labelFieldName,
                descrFieldName, paramdict)
        
        //this.addButton = new qx.ui.form.Button(null, "icon/16/actions/list-add.png");
        //this.delButton  = new qx.ui.form.Button(null, "icon/16/actions/list-remove.png");

        this.addButton = new qx.ui.form.Button(null, "icon/16/actions/go-next.png");
        this.delButton  = new qx.ui.form.Button(null, "icon/16/actions/go-previous.png");

        
        this.rtree = new zqr.view
            .SortedSelListTree(this, undefined, labelFieldName,
                descrFieldName, paramdict);
         
        this.ltree.setWidth(200);
        this.rtree.setWidth(200);
        
        this.ltree.setHeight(zqr.Config.SELLISTTREE_HEIGHT);
        this.rtree.setHeight(zqr.Config.SELLISTTREE_HEIGHT);
        
        var layout = new qx.ui.layout.Grid(3, 3);
        layout.setColumnFlex(0, 1);
        layout.setColumnFlex(1, 1);
        layout.setColumnAlign(0, "center", "middle");
        layout.setColumnAlign(1, "center", "middle");
        
        this.setLayout(layout);
        var vertical_offset = 0;
        
        var blayout = new qx.ui.layout.VBox()
            .set({
                // spacing: Math.floor(zqr.Config.SELLISTTREE_WIDTH / 4),
                alignY: "middle",
                alignX: "center" // left center
            });
            
        var composite = new qx.ui.container.Composite(blayout);
        composite.add(this.addButton);
        composite.add(this.delButton);
        //this.add(composite, {row:vertical_offset, column:1});
        
        this.add(this.tfield, {row:vertical_offset, column:0});
        ++vertical_offset;
        this.add(this.ltree , {row:vertical_offset, column:0});
        this.add(composite, {row:vertical_offset, column:1});
        this.add(this.rtree , {row:vertical_offset, column:2});
        //this.field.addListener("input", this._OnChange, this);
        
        this.tfield.addListener("input", this._OnChange, this);
        this.addButton.addListener("execute", this._OnAdd, this);
        this.delButton.addListener("execute", this._OnDel, this);
    },
    
    members : {
        
        ltree: null,
        rtree: null,
        tfield: null,
        addButton: null,
        delButton: null,
        
        _OnChange : function(e){
            console.log("getData = ", e.getData());
            this.tree.testItems(e.getData());
        },
        
        _OnAdd : function(){
            var selected = this.ltree.getSelected();
            console.log("selected --> ", selected);
            this.rtree.addItems(selected);
            this.ltree.remItems(selected);
        },
        
        _OnDel : function(e){
            var selected = this.rtree.getSelected();
            this.rtree.remItems(selected);
            this.ltree.addItems(selected);
        },
        
        _OnChange : function(e){
            var _this = this;
            var _data = e.getData();
            qx.event.Timer.once(function() {
                _this.ltree.testItems(_data);
            }, this, zqr.Config.SORTEDSELLISTTREE_TIMEOUT);
        },
        
        setChecked : function(list){
            console.log("list --> ", list);
            this.ltree.setChecked(list);
            console.log("list --> ", list);
            this._OnAdd();
        },
        
        getSelectedId : function() {
            return this.rtree.getAllId();
        }
    }
});



