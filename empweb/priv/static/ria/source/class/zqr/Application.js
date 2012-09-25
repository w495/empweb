/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

/* ************************************************************************

#asset(zqr/*)

************************************************************************ */

/**
 * This is the main application class of your custom application "zqr"
 */
qx.Class.define("zqr.Application",
{
  //extend : qx.application.Standalone,
  extend : qx.application.Inline,
  
  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

    statics: {
        APP_WIDTH  : 1000,
        APP_HEIGHT : 700,
        MENU_HEIGHT : 30,
        workarea_height : function(){
            return zqr.Application.APP_HEIGHT - zqr.Application.MENU_HEIGHT - 1;
        }
    },
    
    members :
    {
        title : "Система рекламы tvzavr: кабинет рекламодателя",
        
        main : function()
        {
            zqr.util.utils.ensureNativeFunctions();
            
            if (qx.core.Environment.get("qx.debug")){
              //qx.log.appender.Native;
              //qx.log.appender.Console;
            }
            else{
                console = {};
                console.log =
                    console.error =
                    console.info =
                    console.debug =
                    console.warn =
                    console.trace =
                    console.dir =
                    console.dirxml =
                    console.group =
                    console.groupEnd =
                    console.time =
                    console.timeEnd =
                    console.assert =
                    console.profile = function() {};
            }
            // Call super class
            this.base(arguments);
            qx.locale.Manager.getInstance().setLocale("ru");
            qx.io.remote.RequestQueue.getInstance().setDefaultTimeout(60000*5);
            this.history = [];
            this.screenMap = {};
            this.curMenu = null;
            document.title = this.title;
            this._createLayout();
        },

        ___close : function() {
            return "Вы уверены, что хотите покинуть страницу?";
        },
        
        /**
         * Выводит сообщение останавливающее пользователя уйти со страницы.
         * @see qx.application.AbstractGui
         */
        close : function() {
            return this.___close();
        },
        
        _createLayout : function() {
            var dockLayout = new qx.ui.layout.VBox();
            var dockLayoutComposite = new qx.ui.container.Composite(dockLayout);
            this.tcont = new qx.ui.container.Composite(new qx.ui.layout.Dock());
            this.navBar = new zqr.view.NavBar(this).set({
                width: zqr.Application.APP_WIDTH,
                height: zqr.Application.MENU_HEIGHT
            });
            this.tcont.add(this.navBar, {edge:"north"});
            var windowManager = new qx.ui.window.Manager();
            this.right_cont = new qx.ui.window.Desktop(windowManager).set({
                focusable: false
            });
            this.tcont.add(this.right_cont, {edge:"center"});
            this._createInitialView();
            var isle = new qx.ui.root.Inline(document.getElementById("ria"), true, true)
                .set({
                padding: 0,
                width: zqr.Application.APP_WIDTH,
                minHeight: zqr.Application.APP_HEIGHT,
                textColor: "black",
                backgroundColor: "#cccccc"
            });
            dockLayoutComposite.add(this.tcont);
            dockLayoutComposite.setWidth(zqr.Application.APP_WIDTH);
            dockLayoutComposite.setHeight(zqr.Application.workarea_height());
            isle.add(dockLayoutComposite);
        },

        _createInitialView : function() {
            this.right_cont.removeAll();
            this.navBar
            var imm = {
                name:  "Видео",
                icon:  "icon/16/places/network-workgroup.png",
                model: "resource/zqr/descr/acv-videos-tab.json"
            };
            this.onMenuChange(imm);
        },

        onMenuChange : function(curMenu) {
            console.log("curMenu = ", curMenu);
            
            var cScreen = {
                history  : this.history,
                controller : this.cur_controller,
                actionRow : this.ActionRow,
                filterVal : this.FilterVal
            };
            if(this.curMenu != undefined && this.curMenu != null && this.cur_controller != undefined) {
                this.screenMap[this.curMenu.model] = cScreen;
            }

            this.curMenu = curMenu;

            if(curMenu.model != undefined) {
                if(this.screenMap[curMenu.model] == undefined || this.screenMap[curMenu.model].controller == undefined) {
                    this.history = [];
                    this.cur_controller = undefined;
                    this.ActionRow = undefined;
                    this.FilterVal = undefined;
                    if(this.curMenu.unclose){
                        this.makeClosable(function(){
                            this.loadActionModel(this.curMenu.model);
                        });
                    }
                    else{
                        this.loadActionModel(this.curMenu.model);
                    }
                    this.show_global_pb();
                }
                else {
                    var screen = this.screenMap[this.curMenu.model];
                    this.history = screen.history;
                    this.cur_controller = screen.controller;
                    this.ActionRow = screen.actionRow;
                    this.FilterVal = screen.filterVal;
                    this._makeWindow(this.cur_controller)
                }
            }
            else {
                alert("не задано описание модели");
            }
        },

        makeClosable: function(callback, owner) {
            if(!(owner)){
                owner = this
            }
            this.enableClose(true);
            callback.apply(owner, arguments);
            this.enableClose(false);
        },

        enableClose: function(state) {
            if(state){
                this.close = function(){return null};
            }
            else{
                this.close = function() {return this.___close();}
            }
        },

        loadActionModel : function(ActionUrl) {
            zqr.util.utils.getStaticJson(ActionUrl, this._onIncomeActionModel, this);
        },

        setController: function(controller){
            this.__historyPush();
            this.__setController(controller);
        },

        __historyPush: function(){
            if(this.cur_controller != undefined) {
                this.history.push(this.cur_controller);
            }
        },
                
        __setController: function(controller){
            if(controller) {
                this.right_cont.removeAll();
                this._makeWindow(controller);
            }
            else {
                this.history.pop();
            }
        },

        swapCont: function(callback, a) {
            this.__historyPush();
            this.__setController(callback.apply(this, [this]));
            return true;
        },

        _onIncomeActionModel : function(response) {
            this.hide_global_pb();
            var result = zqr.util.utils.parseStaticJsonRsp(response);
            if (zqr.util.errors.process(this, result)==false) {
                return false;
            }
            this.swapCont(function(){
                var cont = null;
                console.log("this 235 = ", this);
                switch(result.type) {
                    case "table" :
                        cont = new zqr.view.Controller.TabController(this, this.ActionRow, this.FilterVal, result);
                        break;
                    case "dir-double-table" :
                        cont = new zqr.view.Controller.DirDoubleTabController(this, this.ActionRow, this.FilterVal, result);
                        break;
                    case "form" :
                        cont = new zqr.view.Controller.FormController(this, this.ActionRow, result, "");
                        break;
                    case "inline-form" :
                        var tmpActionRow = {id:0,isInline:true}
                        cont = new zqr.view.Controller.FormController(this, tmpActionRow, result, "");
                        break;
                }
                return cont;
            });
            return true;
        },

        _makeWindow : function(cont) {
                var win = new qx.ui.window.Window(
                    zqr.util.utils.capitalize(this.curMenu.name))//,
                    //this.curMenu.icon)
                        .set({
                            //focusable: false,
                            allowMaximize: true,
                            allowMinimize: false,
                            showMinimize: false,
                            showStatusbar: true,
                            movable: false,
                            resizable: false,
                            showClose: false,
                            showMaximize: false
                        });
                win.setLayout(new qx.ui.layout.HBox());
                win.setWidth(zqr.Application.APP_WIDTH);
                win.setHeight(zqr.Application.workarea_height());
                this.cur_controller = cont;
                win.add(this.cur_controller, {flex: 1});
                this.right_cont.add(win);
                win.focus = function(){ }
                win.open();
                win.maximize();
                return win;
        },
        
        onEditClick : function() { // toolbar
            this.ActionRow = undefined;
            this.FilterVal = undefined;
            if(this.cur_controller && this.cur_controller["getActionUrl"] != undefined)
                this.loadActionModel(this.cur_controller.getActionUrl());
        },

        onAction : function(Row, FilterVal, ActionUrl) {//tab dblClick            
            this.ActionRow = Row;
            this.FilterVal = FilterVal;
            this.loadActionModel(ActionUrl);
        },

        getActiveWindow : function() {
            return this.right_cont.getActiveWindow();
        },

        refresh : function() {},
        
        back : function() {
            var cont = this.history.pop();
            if(!cont)
                cont = this.cur_controller;
            this.getActiveWindow().removeAll();
            this.getActiveWindow().add(cont);
            cont.refresh();
            this.cur_controller = cont;
            return this.cur_controller;
        },

        hide_global_pb : function() {
            //document.getElementById("global_progress_bar").style.visibility="hidden";
            console.log("zqr.view.Form.Upload.UploadFakeStatusBar.off");

            clearTimeout(this.global_pb_timeout);
            var resRootItem = document.getElementById("global_progress_bar");
            resRootItem.style.display = "none";
            return resRootItem;
            
        },

        show_global_pb : function() {
            //document.getElementById("global_progress_bar").style.visibility="visible";
            console.log("zqr.view.Form.Upload.UploadFakeStatusBar.on");
            if(this.global_pb_timeout){
                this.hide_global_pb();
            }
            this.global_pb_timeout = setTimeout( function() {
                var resRootItem = document.getElementById("global_progress_bar");
                resRootItem.style.display = "block";
            }, 1000);

            ///document.getElementById("upload_progress_bar").style.visibility="hidden";
        }
    }
});

