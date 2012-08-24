/* ************************************************************************
    Мастер создания видео рекламы.
    
************************************************************************ */


qx.Class.define("zqr.view.Form.AcvVideoShow__",
{

    extend : zqr.view.Form.AbstractForm,

    construct : function(controller, Row) {
        
        if(Row)
            this.createNew = (Row.isNew == true);

        this.row = Row;
        this.base(arguments, controller, Row);
        
        this.buildForm();
    },
    
    members : {
       
        d: function(){console.log(arguments);},
        
        buildForm : function(){
            var cnt = new qx.ui.container.Composite(new qx.ui.layout.HBox(10));
            this.taSummary = new qx.ui.form.TextArea();
            this.taSummary.set({width:500, height:300});
            cnt.add(this.taSummary, {flex : 1});
            this.flashBar = new qx.ui.container.Composite(new qx.ui.layout.HBox());
            cnt.add(this.flashBar);
            this.flashBar.setWidth(640);
            this.flashBar.setHeight(480);
            // -------------------------------------------------------------
            this.controller.placeForm(cnt);
            var req = new qx.io.remote.Request("/get-acv-video", "GET", "application/json");
            req.setParameter("id", this.row.id);
            req.addListener("completed", this._onAcvIncome, this);
            req.send();
            return {controller : cnt, offset: 0};
        },

        _onAcvIncome : function(response) {
            var result = response.getContent();
            if (zqr.util.errors.process(this, result)==false) return false;
            var clip = result.value;
            var catList = result.cats.values;
            var geoList = result.geo.values;
            var txt = "Рекламная кампания, размещение видео в видео.\n";
            txt += "Название: " + clip.name + "\n";
            txt += "Комментарий: " + clip.comment + "\n";
            txt += "Статус: ";
            switch(clip.active) {
                case "":        txt += "на модерации"; break;
                case "false":   txt += "запрещен"; break;
                case "true":    txt += "разрешен"; break;
            }
            txt += "\n";
            txt += "Дата начала: " + zqr.util.utils.formatJsDateTime(zqr.util.utils.getDate(clip.datestart, 0)) + "\n";
            txt += "Дата конца: " + zqr.util.utils.formatJsDateTime(zqr.util.utils.getDate(clip.datestop, 0)) + "\n";
            txt += "Внешняя ссылка: " + clip.url + "\n";
            txt += "URL ролика: " + clip.ref + "\n";
            txt += "Продолжительность ролика: " + clip.duration + " секунд\n";
            txt += "Желаемое количество показов: " + clip.wish + "\n";
            txt += "Показано: " + clip.shown + "\n";
            var tpl = "";
            var apl = ["preroll", "midroll", "postroll"];
            for(var i=0; i<apl.length; i++) {
                if(clip[apl[i]] == "true") {
                    if(tpl != "")
                        tpl += ", ";
                    tpl += apl[i];
                }
            }
            txt += "Размещение ролика: " + tpl + "\n";
            txt += "Повторный показ ролика учтенному пользователю: ";
            if(clip.rerun_hours == "")
                txt += "не ограничен";
            else
                txt += clip.rerun_hours + ":" + clip.rerun_minutes;
            txt += "\n";
            txt += "\nТаргетирование пользователей:\n";
            txt += "Пол: ";
            switch(clip.user_male) {
                case "true": txt += "для мужчин"; break;
                case "false": txt += "для женщин"; break;
                default: txt += "---";
            }
            txt += "\n";
            txt += "Возраст:";
            if(clip.age_from == "")
                txt += "---";
            else
                txt += "от " + clip.age_from + " до " + clip.age_to + " лет";
            txt += "\n";
            txt += "Время показа: ";
            if(clip.time_from == "")
                txt += "---";
            else
                txt += "с " + clip.time_from + " до " + clip.time_to + " часов";
            txt += "\n";
            txt += "\nТаргетирование по регионам: ";
            if(geoList.length == 0)
                txt += "весь мир\n";
            else
                txt += "\n";
            for(var i=0; i<geoList.length; i++)
                txt += geoList[i].name + " (" + geoList[i].code + ")\n";
            txt += "\nТаргетирование по жанрам: ";
            if(!catList)
                catList = [];
            if(catList.length == 0)
                txt += "все жанры\n";
            else
                txt += "\n";
            for(var i=0; i<catList.length; i++)
                txt += catList[i].name + "\n";
            txt += "\n";
            this.taSummary.setValue(txt);
            this.flashPlayer = new qx.ui.embed.Flash("resource/zqr/flash/gddflvplayer.swf").set({
//                scale: "noscale",
                width: 640,
                height: 480,
                variables : {
                    vdo: "/" + clip.ref,
//                    vdo: "/static/data/acv-video/common/5831108/adv02.mp4",
                    autoplay : "false"
                }
            });
            this.flashBar.add(this.flashPlayer);//, {flex: 1});
            return true;
        },
        
        addListeners: function() {
            var _this = this;
        },
        
        _onCancelClick : function(e) {
            this.controller.onCancelClick();
        },
        
        _onSendClick: function() {
        }
        
    }
});

