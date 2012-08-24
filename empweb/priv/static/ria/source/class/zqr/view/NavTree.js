
qx.Class.define("zqr.view.NavTree",
{
    extend : qx.ui.tree.Tree,
 
   include : [zqr.view.NavMixin],
   
    construct : function(root) {
        this.base(arguments);
        this.setHideRoot(true);
        this.setOpenMode("click");
        
        this.init(root);
    },

    members : {

        buildMenu : function(menu) {
            var root = new qx.ui.tree.TreeFolder("root");
            root.setOpen(true);
            this.setRoot(root);
            this.menu = {};
            for(var i=0; i<menu.length; i++) {
                var I = menu[i];
                var F = new qx.ui.tree.TreeFolder(I.name);
                if(I.icon != undefined)
                    F.setIcon(I.icon);
                F.setOpen(I.opened == true)

                root.add(F);
                if(I.subitems != undefined) {
                    for(var j=0; j<I.subitems.length; j++) {
                        var M = I.subitems[j];
                        var E = new qx.ui.tree.TreeFile(M.name);
                        this.menu[M.name] = M;
                        E.setIcon("icon/16/categories/system.png");
                        F.add(E);
                    }
                }
            }
            this.biz.hide_global_pb();
        },

        _onR1 : function(e) {
        }
    }
});


