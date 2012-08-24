
/* ************************************************************************
  
#asset(qx/icon/Tango/16/actions/list-add.png)
#asset(qx/icon/Tango/16/actions/zoom-in.png)
#asset(qx/icon/Tango/16/actions/list-remove.png)

************************************************************************ */

qx.Class.define("zqr.view.Form.Format.RichTextArea",
{
    /// 
    /// Простой способ редактирования текста на основе простого текста.
    /// Предполагается использовать bb-code
    /// TODO: добавить кнопочик [Ж] [Н] [З] [Л] [П] [Ц]
    ///
    
    extend : qx.ui.container.Composite,
    
    construct : function(initval) {
        this.base(arguments, new qx.ui.layout.VBox());
        
        this.textArea = new qx.ui.form.TextArea(initval);
        
        
        var butcnt = new qx.ui.container.Composite(new qx.ui.layout.HBox());
        
        this.bButton = new qx.ui.form.Button("[ж]");
        this.iButton = new qx.ui.form.Button("[н]");
        this.pButton = new qx.ui.form.Button("[пap]");
        this.aButton = new qx.ui.form.Button("[ссылка]");
        this.imgButton = new qx.ui.form.Button("[картинка]");

        butcnt.add(this.bButton);
        butcnt.add(this.iButton);
        butcnt.add(this.pButton);
        butcnt.add(this.aButton);
        butcnt.add(this.imgButton);
        
        // this.add(butcnt, {flex : 0})
        this.add(this.textArea, {flex : 1})
    },
    
    members : {        
        textArea: null,

        getValue : function(){
            return this.textArea.getValue();
        },
        setValue : function(value){
            return this.textArea.setValue(value);
        },
        setValid: function(val){
            return this.textArea.setValid(val);
        },
        resetValid: function(){
            return this.textArea.resetValid();
        },        
        setInvalidMessage: function(val){
            return this.textArea.setInvalidMessage(val)
        },
        resetValue: function(){
            return this.textArea.resetValue()
        }
    }
});

/*
qx.Class.define("zqr.view.Form.Format.RichTextArea",
{
    /// 
    /// Способ редактирования текста на основе html
    /// Немного притормаживает, надо доводить до ума.
    ///
    
    extend : qx.ui.container.Composite,
    construct : function(initval) {
    this.base(arguments, new qx.ui.layout.VBox());
        this.DefaultFontSize = 2;
        this.DefaultFontFamily = "sans-serif";
        
        this.htmlArea = new qx.ui.embed.HtmlArea(zqr.view.Form.Format.RichTextArea.descriptionText, null, "blank.html");
        this.htmlArea.setDefaultFontFamily(this.DefaultFontFamily);
        this.htmlArea.setDefaultFontSize(this.DefaultFontSize);
        
        this.htmlArea.set( { width: 600, height: 300 } );
        this.add(this.htmlArea, {flex : 2})
    },
    statics : {
        descriptionText : ""
    },
    members : {
        getValue : function(){
            this.htmlArea.setDefaultFontFamily("monospace");
            this.htmlArea.setDefaultFontSize(1);
            return this.htmlArea.getValue();
        },
        setValue : function(value){
            this.remove(this.htmlArea);
            this.htmlArea = new qx.ui.embed.HtmlArea(zqr.view.Form.Format.RichTextArea.descriptionText, null, "blank.html");
            this.htmlArea.set( { width: 600, height: 300 } );
            this.htmlArea.setValue(value);
        this.htmlArea.setDefaultFontFamily(this.DefaultFontFamily);
        this.htmlArea.setDefaultFontSize(this.DefaultFontSize);
            this.add(this.htmlArea, {flex : 1})
        },
        setValid: function(val){},
        resetValid: function(val){},
        setInvalidMessage: function(val){},
        resetValue: function(){
            this.remove(this.htmlArea);
            this.htmlArea = new qx.ui.embed.HtmlArea(zqr.view.Form.Format.RichTextArea.descriptionText, null, "blank.html");
            this.htmlArea.set( { width: 600, height: 300 } );
            this.htmlArea.setDefaultFontFamily(this.DefaultFontFamily);
            this.htmlArea.setDefaultFontSize(this.DefaultFontSize);
        
            this.add(this.htmlArea, {flex : 1})
        }
    }
});
*/