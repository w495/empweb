/* ************************************************************************
#asset(qx/icon/Tango/16/actions/list-add.png)
#asset(qx/icon/Tango/16/actions/list-remove.png)

************************************************************************ */

/**
        --------------------------------
        | <<obj.insider>>         [+] |
        --------------------------------
                    или
        --------------------------------
        | <<obj.insider.top>>         |
        | [+]   <<obj.insider.left>>  |
        | <<obj.insider.bottom>>      |
        --------------------------------
                    или
        --------------------------------
        | <<obj.insider.top>>         |
        | <<obj.insider.left>>    [+] |
        | <<obj.insider.bottom>>      |
        --------------------------------
**/

qx.Class.define("zqr.view.Addline",
{
    extend : qx.core.Object,
    construct : function(obj) {
        console.log(">-1");

        this.layout     = obj.layout;
        this.insider    = obj.insider;
        this.owner      = obj.owner;

        this.counter = 1;
        this.label = "";

        if(obj.counter)
            this.counter = obj.counter;
        if(obj.label)
            this.label = obj.label;

        this.__i = this.__apply(this.insider);

        var clone = function(){};
        clone.prototype = this.layout;
        this.__l =  new clone();
        this.__w = new qx.ui.groupbox.GroupBox(
            this.label + " (" + this.counter + ")"
        ).set({
            layout: this.__l
        });

        if(this.__i.getValue){
            var pointer = this;
            this.__w.getValue = function(){
                return pointer.__i.getValue();
            };
        }


        if(this.__l instanceof qx.ui.layout.Grid){
            var row = 0;
            if(this.__i instanceof qx.core.Object){
                this.__w.add(this.__i,       {row:row,column:0});
                this.__w.add(this.__fbb(),   {row:row,  column:1});
                row += 1;
            }
            else{
                this.__i.top    = this.__apply(this.__i.top);
                this.__i.left   = this.__apply(this.__i.left);
                this.__i.right  = this.__apply(this.__i.right);
                this.__i.bottom = this.__apply(this.__i.bottom);

                if(this.__i.top && this.__i.top instanceof qx.core.Object){
                    this.__w.add(
                        this.__i.top,
                        {row:row, column:0, colSpan:2}
                    );
                    row += 1;
                }
                if(this.__i.left &&  this.__i.left instanceof qx.core.Object){
                    this.__w.add(
                        this.__i.left,
                        {row:row, column:0}
                    );
                    this.__w.add(
                        this.__fbb(),
                        {row:row,   column:1}
                    );
                    row += 1;
                }
                else if(this.__i.right && this.__i.right instanceof qx.core.Object){
                    this.__w.add(
                        this.__fbb(),
                        {row:row, column:0}
                    );
                    this.__w.add(
                        this.__i.left,
                        {row:row,   column:1}
                    );
                    row += 1;
                }
                if(this.__i.bottom && this.__i.bottom instanceof qx.core.Object){
                    this.__w.add(
                        this.__i.bottom,
                        {row:row, column:0, colSpan:2}
                    );
                    row += 1;
                }

                if(!this.__w.getValue){
                    var pointer = this;
                    this.__w.getValue = function(){
                        return {top:    pointer.__i.top &&
                                        pointer.__i.top.getValue &&
                                        pointer.__i.top.getValue(),
                                left:   pointer.__i.left &&
                                        pointer.__i.left.getValue &&
                                        pointer.__i.left.getValue(),
                                right:  pointer.__i.right &&
                                        pointer.__i.right.getValue &&
                                        pointer.__i.right.getValue(),
                                bottom: pointer.__i.bottom &&
                                        pointer.__i.bottom.getValue &&
                                        pointer.__i.bottom.getValue()}
                    }
                }
            }
        }
        else{
            if(this.__i instanceof qx.core.Object){
                this.__w.add(this.__i,{flex:1});
                this.__w.add(this.__fbb());
            }
        }
    },
    members : {
        __b:    null,
        __i:    null,
        __l:    null,
        __w:    null,

        layout:     null,
        insider:    null,
        owner:      null,
        counter:    null,
        label:      null,

        getWidget: function() {
            return this.__w;
        },

        __clone : function(obj){
            var Clone = function(){};
            Clone.prototype = obj;
            return new Clone();
        },

        __apply : function(maybeFun) {
            if(!maybeFun){
                return undefined;
            }
            if("function" == typeof(maybeFun)){
                return maybeFun();
            }
            else if("object" == typeof(maybeFun)){
                if(maybeFun instanceof qx.core.Object)
                    return maybeFun
                else{
                    if(!maybeFun.context)
                        maybeFun.context = this.owner;
                    if(!maybeFun.args)
                        maybeFun.args = [];
                    return maybeFun.fun.apply(maybeFun.context, maybeFun.args);
                }
            }
        },

        __fbb : function() {
            console.log("__fbb");
            var button = new qx.ui.form.Button(
                null,
                "icon/16/actions/list-add.png"
            );
            button.addListener("execute", function(){
                if((!button.__on)){
                    var line = new this.constructor({
                        layout:     this.layout,
                        owner:      this.owner,
                        insider:    this.insider,
                        counter:    this.counter+1,
                        label:      this.label
                    });
                    this.owner.add(line.__w);
                    button.__on = true;
                    button.setIcon("icon/16/actions/list-remove.png");
                }
                else{
                    this.owner.remove(this.__w);
                    button.__on = false;
                }
            }, this);
            return button;
        }
    }
});

