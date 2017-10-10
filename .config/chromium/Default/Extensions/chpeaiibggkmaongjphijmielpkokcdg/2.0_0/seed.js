(function() {
    var j = [];
    var b = "fanyi.youdao.com",
        i = "/web2/";
    if (window.location.host === b && window.location.pathname === i) {
        var e = new Image();
        e.src = "https://fanyi.youdao.com/web2/rl.do?action=w_try&ts=" + (new Date()).getTime();
        j[0] = e;
        alert("请在浏览英文网页时使用有道网页翻译2.0");
        return
    }
    if (!(window.location.protocol == "http:" || window.location.protocol == "https:")) {
        alert("请在浏览网页时使用有道网页翻译2.0");
        return
    }
    var h = "https://fanyi.youdao.com/web2";
    var e = new Image();
    e.src = "https://fanyi.youdao.com/web2/rl.do?action=init&relatedURL=" + encodeURIComponent(document.location.href) + "&ts=" + (new Date()).getTime();
    j[0] = e;
    if (!window.OUTFOX_JavascriptTranslatoR) {
        var scriptEl = document.createElement('script');
        scriptEl.src = chrome.extension.getURL('all-packed-utf-8.js');
        // scriptEl.addEventListener('load', callback, false);
        document.body.appendChild(scriptEl)
    } else {
        var k = "https://fanyi.youdao.com";
        var a = "/web2/conn.html";
        var l = h + "/index.do";
        var g = k + "/jtr";
        var c = h + "/rl.do";
        var f = h + "/styles/all-packed.css";
        J.loadCSS(document, f);
        window.OUTFOX_JavascriptTranslatoR = new J.TR.UI(document.body, {
            domain: k,
            update: false,
            updateTipMsg: "增加关闭按钮",
            updateDate: "2011-3-15",
            cssURL: f,
            tipsURL: l,
            transURL: g,
            logURL: c,
            connFilePath: a,
            reqSize: 20
        })
    }
})();