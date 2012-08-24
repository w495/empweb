

qx.Class.define("zqr.util.errors",
{
    statics : {
         process : function(own, resp) {
            if(resp.REDIRECT) /*(resp.REDIRECT != undefined)*/{
                window.location=resp.REDIRECT;
                return false;
            }
            if(resp.RELOAD != undefined) {
                location.reload();
                return false;
            }
            if(resp.PERM_REQUIRED != undefined) {
                alert("Не хватает прав доступа. Обратиесь к администратору.");
                return false;
            }
            if(resp.ERROR != undefined) {
                if(own.show_error)
                    own.show_error(resp.ERROR.type, resp.ERROR.info);
                return false;
            }
            if(resp.WARN != undefined) {
                if(own.show_warn)
                    own.show_warn(resp.WARN.type, resp.WARN.info);
                return false;
            }
            return true;

        }
    }
});
