
/* ************************************************************************

#asset(qx/icon/Tango/16/*)
#asset(qx/icon/Tango/16/apps/utilities-dictionary.png)
#asset(qx/icon/Tango/16/places/network-workgroup.png)
#asset(qx/icon/Tango/16/apps/preferences-theme.png)


#asset(qx/icon/Tango/22/actions/help-contents.png)

************************************************************************ */

qx.Class.define("zqr.view.NavBar",
{
    extend : qx.ui.container.Composite,
 
   include : [zqr.view.NavMixin],
   
    construct : function(root) {
        this.base(arguments);
        this.setLayout(new qx.ui.layout.HBox());
        this.init(root);
    },

    members : {
        toolbar: null,
        focus : function() {
            return this.toolbar.focus();
        },

        buildMenu : function(menuModel) {
            this.toolbar = new qx.ui.toolbar.ToolBar();
            this.add(this.toolbar, {flex: 1});
            this.menuPart = this.makeMenuPart(menuModel);
            this.toolbar.add(this.menuPart);
            this.toolbar.addSpacer();
            this.helpPart = this.makeMenuPart(menuModel, true, true);
            this.toolbar.add(this.helpPart);

//             var helpButton = new qx.ui.toolbar.Button("Помощь", "icon/22/actions/help-contents.png");
//             helpButton.addListener("execute", function(){
//                 window.open("/docs","_blank","",false);
//             });
//             this.toolbar.add(helpButton);
      
            this.biz.hide_global_pb();
        },
 
        makeMenuPart : function(menuModel, isPartContainer, afterSpacer) {
            console.log("menuModel = ", menuModel);
            var menuPart = null;
            if(isPartContainer){
                menuPart = new qx.ui.toolbar.PartContainer();
            }else{
                menuPart = new qx.ui.toolbar.Part();
            }
            
            for(var key = 0; key != menuModel.length; ++key){
                var item = menuModel[key];
                if(item){
                    if((!!item.afterSpacer) != (!!afterSpacer))
                        continue;
                    
                    var itemMenu = null;
                    if(item.model){
                        if(item.external){
                            itemMenu = this.__getExternalButton(item, true)
                        }
                        else{
                            console.log("item = ", item);
                            itemMenu = this.__getInternalButton(item, true);
                        }
                    }
                    else{
                        itemMenu = this.__getMenuButton(item);
                    }

                    console.log("item = ", itemMenu);
                    menuPart.add(itemMenu);
                }
            }
            return menuPart;
        },

        __getMenuButton : function(item) {
            var menuButton = new qx.ui.toolbar.MenuButton(item.name, item.icon);
            if(item.subitems){
                menuButton.setMenu(this.__getItemMenu(item.subitems));
            }
            return menuButton;
        },
                
        __getExternalButton : function(item, toolbar) {
            var button = null;
            if(toolbar){
                button = new qx.ui.toolbar.Button(item.name, item.icon);
            }
            else{
                button = new qx.ui.menu.Button(item.name, item.icon);
            }
            button.itemMenuModel = item;
            console.log("button.itemMenuModel = ", button.itemMenuModel);
            button.addListener("execute", function(){
                console.log("this.itemMenuModel.model = ", this.itemMenuModel.model);
                console.log("this.itemMenuModel.externalType = ", this.itemMenuModel.externalType);
                window.open(this.itemMenuModel.model,this.itemMenuModel.externalType, "",false);
            });
            return button;
        },
                
        __getInternalButton : function(item, toolbar) {
            var button = null;
            if(toolbar){
                button = new qx.ui.toolbar.Button(item.name, item.icon);
            }
            else{
                button = new qx.ui.menu.Button(item.name, item.icon);
            }
            
            this.menu[item.name] = item;
            button.itemMenuModel = item;
            button.ths_ = this;
            button.addListener("execute", function(){
                this.ths_.biz.onMenuChange(this.itemMenuModel);
            });
            return button;
        },
                
        __getItemMenu : function(itemMenuModel) {
            var menu = new qx.ui.menu.Menu();
            for(var ikey = 0; ikey  != itemMenuModel.length; ikey++){
                var item = itemMenuModel[ikey];
                var button = this.__getInternalButton(item)
                menu.add(button);
            }
            return menu;
        },

        debugRadio : function(e) {
            console.log("Execute button: ", e);
        },

        debugCommand : function(e) {
            console.log("Execute button: ", e);
        },

        debugButton : function(e) {
            console.log("Execute button: ", e);
        },

        debugCheckBox : function(e) {
            console.log("Execute button: ", e);
        }

    }
});


