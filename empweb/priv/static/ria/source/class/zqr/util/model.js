

/**
    Моделе-зависимые статические функции
**/

qx.Class.define("zqr.util.model",
{
    type : "static",

    statics : {

        /**
            Заменяет id директории модели на id родителя.
            Эквивалентно переходу вверх по дереву директорий.
        **/
        goToPrevDir: function(object) {
            if(object.tabModel.vardata.parent_dir_id)
                if(object.dirStack && (object.dirStack.length >= 2))
                    object.tabModel.vardata.parent_dir_id = object.dirStack.pop();
                    // текущая директория находится на верхушке стека
                    object.tabModel.vardata.id = object.dirStack[object.dirStack.length-1];
        },

        goToDir: function(object, Row){
            if(!object.dirStack.length)
                object.dirStack.push(this.defaultVal);
            object.dirStack.push(Row.id)
        },

        updatePrevDir: function(object) {
            if(object.tabModel.vardata.parent_dir_id)
                object.tabModel.vardata.parent_dir_id = 0
        }

    }
});
