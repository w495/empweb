

qx.Class.define("zqr.view.Controller.DirDoubleTabController",
{
    extend : qx.ui.container.Composite,

    construct : function(biz, Data, FilterVal, tabModels)
    {
        this.base(arguments, new qx.ui.layout.VBox());
        var splitpane = new qx.ui.splitpane.Pane("horizontal");
        
        //Переменные денные левого главнее.
        tabModels.right.vardata = tabModels.left.vardata;

    // ----------------------------------------------------------------------
    //      ЛЕВЫЙ КОНТРОЛЛЕР
    // ----------------------------------------------------------------------

        this.left = new zqr.view.Controller.TabController(biz, Data, FilterVal, tabModels.left);
        this.left.setDecorator(null);
        this.left.parentController = this
        this.left.dirStack = new Array();
        this.left.defaultVal  = this.left.tabModel.vardata.id;

        // alert(this.left.tabModel.vardata.id);

        this.left.dirStack.push(this.left.tabModel.vardata.id);

        // Переопределим действие для перехода.
        this.left.rowDblClick = function(Row) {
            // Cоединяем переменные данные левого и правого контроллера.
            this.parentController.bindVardata(Row);
            // Если стек пуст забиваем его значением по-умолчанию.
            if(!this.dirStack.length){
                this.dirStack.push(this.defaultVal);
            }
            // Кладем в стек директорию по которой мы перешли.
            this.dirStack.push(Row.id);
            // Обновляем левый и правый контроллеры.
            this.parentController.refresh ();
        }

        // Переопределим действие для кнопки назад
        this.left._tabChangeStateAction = function() {
            if(this.tabModel.vardata.parent_dir_id){
                if(this.dirStack && (this.dirStack.length >= 2)){
                    // Cнимаем со стека родителя.
                    this.tabModel.vardata.parent_dir_id = this.dirStack.pop();
                    // Текущая директория находится на верхушке стека.
                    this.tabModel.vardata.id = this.dirStack[this.dirStack.length-1];
                }
            }
            console.log(this.dirStack);
            // Обновляем левый и правый контроллеры.
            this.parentController.refresh ();
        }

        // Переопределим действие для кнопки создать.
        this.left._tabChangeState = function(actionUrl) {
            var vardata = this.tabModel.vardata
            //\ var newVardata = zqr.util.utils.clone(vardata)
            //\ newVardata.parent_dir_id = vardata.id;

            var newVardata = {isNew: true, id: vardata.id, dir_type_id: vardata.dir_type_id};

            // Вызываем форму создания директории с данными о ее родителе
            this.biz.onAction(newVardata, this.filterForm.getValues(), actionUrl);
            //this.refresh ();
        }

    // ----------------------------------------------------------------------
    //      ПРАВЫЙ КОНТРОЛЛЕР
    // ----------------------------------------------------------------------

        this.right = new zqr.view.Controller.TabController(biz, Data, FilterVal, tabModels.right);
        this.right.setDecorator(null);
        this.right.parentController = this;

        // Переопределим действие для кнопки создать.    
        this.right._tabChangeState = function(actionUrl) {
            var vardata = this.parentController.left.tabModel.vardata;
            //\ var newVardata = zqr.util.utils.clone(vardata)
            //\ newVardata.parent_dir_id = vardata.id;

            var newVardata = {isNew: true, id: vardata.id};

            // Вызываем форму создания документа с данными о его родителе
            this.biz.onAction(newVardata, this.filterForm.getValues(), actionUrl);
            this.refresh ();
        }

        splitpane.add(this.left, 1)
        splitpane.add(this.right, 1)
        this.add(splitpane, {flex : 1});
    },

    members : {

        left   : null,
        right  : null,

        refresh : function() {
            this.left.refresh ();
            this.right.refresh ();
        },

        /**
            Соединяет переменные данные левого и правого контроллера
        **/
        bindVardata: function(data) {
            this.left.bindVardata(data);
            this.right.bindVardata(data);
        }
    }
});

