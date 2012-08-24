
/* ************************************************************************
  
#asset(qx/icon/Tango/32/actions/list-add.png)
#asset(qx/icon/Tango/32/actions/list-remove.png)

************************************************************************ */

qx.Class.define("zqr.view.Form.Upload.AttachContainer",
{
    extend : qx.ui.container.Composite,


    construct: function(list)
    {
        this.attache_list = list;
        
        var mainlayout = new qx.ui.layout.HBox();
        mainlayout.setSpacing(2);
        this.base(arguments, mainlayout);
        
        this.add(list,  {flex: 1});
        
        var butlayout = new qx.ui.layout.VBox();
        var butcnt = new qx.ui.container.Composite(butlayout);
        butlayout.setSpacing(2);
        
        this.addButton = new qx.ui.form.Button(null, "icon/32/actions/list-add.png");
        this.remButton = new qx.ui.form.Button(null, "icon/32/actions/list-remove.png");
        
        butcnt.add(this.addButton);
        butcnt.add(this.remButton);
        
        this.add(butcnt);
        
        this.addListeners();
    },

    members:
    {
        doc_id: 0,
        
        attache_list: null,
        
        addButton : null,
        remButton : null,
        
        win: null,
        
        setId: function(id){
            this.doc_id = id;
        },
        
        addListeners: function(){
            
            var _this = this;
            
            this.addButton.addListener('click',function(e){
                /**
                 * Добавление вложения
                 **/
                _this.win = new zqr.view.Form.Upload.AttachWindow();
                _this.win.setId(_this.doc_id);
                _this.win.addListener('completed',function(e){
                    var data  = e.getData();
                    if('' != data) {
                        _this.attache_list.addItem(data);
                    };
                });
                _this.win.open();
            });
            
            this.remButton.addListener('click',function(e){
                /**
                 * Удаление вложения
                 **/
                var selected = _this.attache_list.getSelected();
                console.log("selected = ", selected);
                var deletedIdList = _this.attache_list.getSelectedId();
                console.log("deletedIdList = ", deletedIdList);        
                _this.attache_list.remItems(selected);
            });
        }
    }
});
