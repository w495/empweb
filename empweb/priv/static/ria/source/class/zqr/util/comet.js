qx.Class.define("zqr.util.comet",
{
    extend : Object,

    construct : function(url, tManager, root, onInfoIncome) {
        this.url = url;
        this.oldLen = 0;
        this.timer = new qx.event.Timer(5000);
        this.timer.addListener("interval", this._onTick, this);

        this.tManager = tManager;
        this.root = root;

        this.onInfoIncome = onInfoIncome;

        this.timerReconnection = new qx.event.Timer(5000);
        this.timerReconnection.addListener("interval", this._onReconnectTick, this);
    },

    members : {
        // public
        setParams : function(params) {
            this.params = params;
            this._reconnect();
        },

        stop : function() {
            if(this.req == undefined)
                return;
            this.timer.stop();
            this.req.removeListener("completed", this._onCompleted, this);
            this.req.removeListener("receiving", this._onFeed, this);
            this.req.removeListener("failed", this._onFeedFailed, this);
            this.req.removeListener("aborted", this._onFeedAborted, this);
            this.req.removeListener("timeout", this._onFeedTimeout, this);

            try {
                this.req.reset();
            }
            catch(err) {
            }

            this.req = undefined;
        },

        // protected
        _insertComma : function(cnt) {
            var counter = 0;
            var res = "";
            var p = 0;
            for(var c = 0; c < cnt.length; c++) {
                if(cnt[c] == '[')
                    counter += 1;
                else if(cnt[c] == ']')
                    counter -= 1;

                if(counter == 0) {
                    res += cnt.slice(p, c+1);
                    p = c+1;
                    if(c+1 < cnt.length)
                        res += ',';
                }
            }
            return res;
        },

        _onTick : function(e) {
            var tr = this.req.getTransport().getImplementation();
            var newLen = tr.getFetchedLength();
//alert(this.req.$$user_transport.$$user_implementation._req.responseText);
//alert(tr._req.responseXML);
//qx.log.Logger.info(tr._req.readyState);
//qx.log.Logger.info(tr.getFetchedLength());
//qx.log.Logger.info(tr._req.responseStream);
//qx.log.Logger.info(tr.getResponseXml());
//qx.log.Logger.info(tr._req.responseXML);
            if(newLen > this.oldLen) {
                var cnt = tr.getResponseText();
                var newCnt = "[" + this._insertComma(cnt.slice(this.oldLen)) + "]";
                if(this.onInfoIncome(newCnt))
                    this.oldLen = newLen;
            }
        },

        _onReconnectTick : function() {
            this._reconnect();
        },

        _reconnect : function() {
//            alert("reconnect");
            this.timerReconnection.stop();
            if(this.params == undefined)
                return;
            this.oldLen = 0;
            if(this.req != undefined)
                this.stop();

            this.req = new qx.io.remote.Request(this.url, "GET", "application/json");
            for(var i=0; i<this.params.length; i++) {
                var E = this.params[i];
                this.req.setParameter(E.name, E.val);
            }
            //this.req.setParameter('trackerPhone', this.trackerPhone);
            this.req.addListener("completed", this._onCompleted, this);
            this.req.addListener("receiving", this._onFeed, this);
            this.req.addListener("failed", this._onFeedFailed, this);
            this.req.addListener("aborted", this._onFeedAborted, this);
            this.req.addListener("timeout", this._onFeedTimeout, this);
            this.req.setTimeout(300000);
            this.req.send();
        },

        _onCompleted : function(e) {
            var content = e.getContent();
//            if(zqr.util.errors.process(content) == false)
//                return;
//            alert("COMPLETED" + content);
//            var newCnt = "[" + this._insertComma(content) + "]";
            if(content != null) {
                if (zqr.util.errors.process(this, content)==false)
                    return;
                this.onInfoIncome(content);
            }
            else
                this.timerReconnection.start();
//            this._reconnect();
        },

        _onFeed : function(e) {
//            alert("feed started");
//            var tr = this.req.getTransport().getImplementation();
//            tr.addListener("receiving", this._onTick, this);
//            this.timer.start();
        },

        _onFeedFailed : function(e) {
//            alert("feed filed");
            this.timerReconnection.start();
        },

        _onFeedAborted : function(e) {
//            alert("feed aborted");
            this.timerReconnection.start();
        },

        _onFeedTimeout : function(e) {
//            alert("feed timeout");
            this.timerReconnection.start();
        }
    }
});
