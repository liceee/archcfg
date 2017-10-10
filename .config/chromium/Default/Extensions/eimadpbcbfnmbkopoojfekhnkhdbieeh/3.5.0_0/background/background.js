var DarkReader;
(function (DarkReader) {
    var Background;
    (function (Background) {
        // Initialize extension
        Background.extension;
        Background.onExtensionLoaded = new xp.Event();
        DarkReader.loadConfigs(function () {
            Background.extension = new DarkReader.Extension(new DarkReader.FilterCssGenerator());
            Background.onExtensionLoaded.invoke(Background.extension);
        });
        if (DarkReader.DEBUG) {
            // Reload extension on connection
            var listen = function () {
                var req = new XMLHttpRequest();
                req.open('GET', 'http://localhost:8890/', true);
                req.onload = function () {
                    if (req.status >= 200 && req.status < 300) {
                        chrome.runtime.reload();
                    }
                    else {
                        setTimeout(listen, 2000);
                    }
                };
                req.onerror = function () { return setTimeout(listen, 2000); };
                req.send();
            };
            setTimeout(listen, 2000);
        }
    })(Background = DarkReader.Background || (DarkReader.Background = {}));
})(DarkReader || (DarkReader = {}));
