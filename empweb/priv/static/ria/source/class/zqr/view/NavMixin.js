/* ************************************************************************
#asset(qx/icon/Tango/16/apps/office-project.png)
#asset(qx/icon/Tango/16/apps/office-calendar.png)
#asset(qx/icon/Tango/16/apps/office-chart.png)
#asset(qx/icon/Tango/16/apps/utilities-calculator.png)
#asset(qx/icon/Tango/16/apps/utilities-dictionary.png)
#asset(qx/icon/Tango/16/apps/utilities-statistics.png)
#asset(qx/icon/Tango/16/categories/system.png)
************************************************************************ */

qx.Mixin.define("zqr.view.NavMixin",
{
    /** 
     * Используем "смесь" как альтернативу
     * множественному наследованию.
    **/
    
    members : {
        
        init : function(root) {
            this.biz = root;
            this.addListener("changeSelection", this._onMenuSelect, this);
            this.menu = {};
            this.__buildMenu();
        },
    
        _onMenuSelect : function(e) {
            var I = this.getSelection()[0];
            var L = I.getLabel();
            if(this.menu[L] != undefined){
                this.biz.onMenuChange(this.menu[L]);
            }
        },

        __buildMenu : function() {
            zqr.util.utils.getStaticJson('resource/zqr/descr/menu.json',
                                         this.__onGetMenuResource, this);
        },

        __onGetMenuResource : function(response) {
            var result = zqr.util.utils.parseStaticJsonRsp(response);
            if (zqr.util.errors.process(this, result)==false)
                return false;
            this.buildMenu(result);
            return true;
        }
    }
});


