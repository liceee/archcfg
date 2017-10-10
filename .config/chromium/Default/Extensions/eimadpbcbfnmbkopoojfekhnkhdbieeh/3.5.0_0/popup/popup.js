var DarkReader;
(function (DarkReader) {
    var Popup;
    (function (Popup) {
        // Create window
        Popup.popupWindow;
        // Edge fix
        if (window.chrome && !chrome.extension && window.browser && window.browser.extension) {
            chrome.extension = window.browser.extension;
        }
        if (window.chrome && chrome.extension) {
            // Access extension from the background
            var background = chrome.extension.getBackgroundPage().DarkReader.Background;
            if (background.extension) {
                Popup.popupWindow = new Popup.PopupWindow(background.extension);
            }
            else {
                var onExtLoaded = function (ext) {
                    Popup.popupWindow = new Popup.PopupWindow(ext);
                    background.onExtensionLoaded.removeHandler(onExtLoaded);
                };
                background.onExtensionLoaded.addHandler(onExtLoaded);
            }
            // Remove popup and unbind from model
            window.addEventListener('unload', function (e) {
                Popup.popupWindow.scope = null;
                Popup.popupWindow.remove();
            });
        }
        else {
            Popup.popupWindow = getMockPopup();
        }
        /**
         * Mock for tests.
         */
        function getMockPopup() {
            return new Popup.PopupWindow(xp.observable({
                enabled: true,
                config: {
                    mode: 1 /*DarkReader.FilterMode.dark*/,
                    brightness: 110,
                    contrast: 80,
                    grayscale: 30,
                    sepia: 10,
                    useFont: false,
                    fontFamily: 'Segoe UI',
                    textStroke: 0,
                    siteList: [
                        'mail.google.com',
                        'npmjs.com'
                    ],
                    invertListed: false
                },
                fonts: [
                    'Arial',
                    'B', 'C', 'D', 'E', 'F', 'G', 'H',
                    'Open Sans',
                    'Segoe UI',
                    'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
                ],
                getActiveTabInfo: function (callback) { callback({ host: 'server1.mail.veryverylongnameveryverylongnameveryverylongnameveryverylongname.com' }); }
            }));
        }
    })(Popup = DarkReader.Popup || (DarkReader.Popup = {}));
})(DarkReader || (DarkReader = {}));
