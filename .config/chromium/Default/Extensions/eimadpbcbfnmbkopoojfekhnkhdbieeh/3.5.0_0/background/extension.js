var __extends = this.__extends || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var DarkReader;
(function (DarkReader) {
    /**
     * Chrome extension.
     * Extension uses CSS generator to process opened web pages.
     */
    var Extension = (function (_super) {
        __extends(Extension, _super);
        /**
         * Creates a chrome extensions.
         * @param generator CSS-generator.
         */
        function Extension(generator) {
            var _this = this;
            _super.call(this);
            this.tabUpdateListener = function (tabId, info, tab) {
                console.log("Tab updated: " + tab.id + ", status: " + info.status);
                _this.addStyleToTab(tab);
            };
            this.tabReplaceListener = function (addedTabId, replacedTabId) {
                console.log("Tab " + replacedTabId + " replaced with " + addedTabId);
                chrome.tabs.get(addedTabId, function (tab) { return _this.addStyleToTab(tab); });
            };
            this.generator = generator;
            // Define properties
            xp.Model.property(this, 'enabled', false);
            xp.Model.property(this, 'config', null);
            xp.Model.property(this, 'fonts', []);
            // Handle config changes
            var changeReg = new xp.EventRegistrar();
            this.onPropertyChanged.addHandler(function (prop) {
                if (prop === 'enabled') {
                    _this.onAppToggle();
                }
                if (prop === 'config') {
                    changeReg.unsubscribeAll();
                    changeReg.subscribe(_this.config.onPropertyChanged, _this.onConfigPropChanged, _this);
                    changeReg.subscribe(_this.config.siteList.onCollectionChanged, _this.onConfigPropChanged, _this);
                    _this.onConfigPropChanged();
                }
            });
            // Default icon
            chrome.browserAction.setIcon({
                path: {
                    '19': ICON_PATHS.inactive_19,
                    '38': ICON_PATHS.inactive_38
                }
            });
            // Load user settings from Chrome storage
            this.loadUserSettings();
            // Subscribe on keyboard shortcut
            chrome.commands.onCommand.addListener(function (command) {
                if (command === 'toggle') {
                    console.log('Toggle command entered');
                    _this.enabled = !_this.enabled;
                }
                if (command === 'addSite') {
                    console.log('Add Site command entered');
                    _this.toggleCurrentSite();
                }
            });
            // Load font list
            this.getFontList(function (fonts) { return _this.fonts = fonts; });
            // TODO: Try to remove CSS before ext disabling or removal.
            window.addEventListener('unload', function () {
                chrome.tabs.query({}, function (tabs) {
                    tabs.forEach(_this.removeStyleFromTab, _this);
                });
            });
        }
        /**
         * Returns info of active tab
         * of last focused window.
         */
        Extension.prototype.getActiveTabInfo = function (callback) {
            chrome.tabs.query({
                active: true,
                lastFocusedWindow: true
            }, function (tabs) {
                if (tabs.length === 1) {
                    var url = tabs[0].url;
                    var host = url.match(/^(.*?:\/{2,3})?(.+?)(\/|$)/)[2];
                    var info = {
                        url: url,
                        host: host,
                        isChromePage: (url.indexOf('chrome') === 0 || url.indexOf('https://chrome.google.com/webstore') === 0),
                        isInDarkList: DarkReader.isUrlInList(url, DarkReader.DARK_SITES)
                    };
                    callback(info);
                }
                else {
                    if (DarkReader.DEBUG) {
                        throw new Error('Unexpected tabs count.');
                    }
                    console.error('Unexpected tabs count.');
                    callback({ url: '', host: '', isChromePage: false, isInDarkList: false });
                }
            });
        };
        /**
         * Adds host name of last focused tab
         * into Sites List (or removes).
         */
        Extension.prototype.toggleCurrentSite = function () {
            var _this = this;
            this.getActiveTabInfo(function (info) {
                if (info.host) {
                    var index = _this.config.siteList.indexOf(info.host);
                    if (index < 0) {
                        _this.config.siteList.push(info.host);
                    }
                    else {
                        // Remove site from list
                        _this.config.siteList.splice(index, 1);
                    }
                }
            });
        };
        //------------------------------------
        //
        //       Handle config changes
        //
        Extension.prototype.onAppToggle = function () {
            var _this = this;
            if (this.enabled) {
                //
                // Switch ON
                // Change icon
                chrome.browserAction.setIcon({
                    path: {
                        '19': ICON_PATHS.active_19,
                        '38': ICON_PATHS.active_38
                    }
                });
                // Subscribe to tab updates
                this.addTabListeners();
                // Set style for active tabs
                chrome.tabs.query({ active: true }, function (tabs) {
                    tabs.forEach(_this.addStyleToTab, _this);
                });
                // Update style for other tabs
                chrome.tabs.query({ active: false }, function (tabs) {
                    tabs.forEach(function (tab) {
                        setTimeout(function () { return _this.addStyleToTab(tab); }, 0);
                    });
                });
            }
            else {
                //
                // Switch OFF
                // Change icon
                chrome.browserAction.setIcon({
                    path: {
                        '19': ICON_PATHS.inactive_19,
                        '38': ICON_PATHS.inactive_38
                    }
                });
                // Unsubscribe from tab updates
                this.removeTabListeners();
                // Remove style from active tabs
                chrome.tabs.query({ active: true }, function (tabs) {
                    tabs.forEach(_this.removeStyleFromTab, _this);
                });
                // Remove style from other tabs
                chrome.tabs.query({ active: false }, function (tabs) {
                    tabs.forEach(function (tab) {
                        setTimeout(function () { return _this.removeStyleFromTab(tab); }, 0);
                    });
                });
            }
            this.saveUserSettings();
        };
        Extension.prototype.onConfigPropChanged = function () {
            var _this = this;
            if (this.enabled) {
                // Update style for active tabs
                chrome.tabs.query({ active: true }, function (tabs) {
                    tabs.forEach(_this.addStyleToTab, _this);
                });
                // Update style for other tabs
                chrome.tabs.query({ active: false }, function (tabs) {
                    tabs.forEach(function (tab) {
                        setTimeout(function () { return _this.addStyleToTab(tab); }, 0);
                    });
                });
            }
            this.saveUserSettings();
        };
        //-------------------------
        //
        // Working with chrome tabs
        //
        //-------------------------
        Extension.prototype.addTabListeners = function () {
            if (!chrome.tabs.onUpdated.hasListener(this.tabUpdateListener)) {
                chrome.tabs.onUpdated.addListener(this.tabUpdateListener);
            }
            // Replace fires instead of update when page loaded from cache
            // https://bugs.chromium.org/p/chromium/issues/detail?id=109557
            // https://bugs.chromium.org/p/chromium/issues/detail?id=116379
            if (!chrome.tabs.onReplaced.hasListener(this.tabReplaceListener)) {
                chrome.tabs.onReplaced.addListener(this.tabReplaceListener);
            }
        };
        Extension.prototype.removeTabListeners = function () {
            chrome.tabs.onUpdated.removeListener(this.tabUpdateListener);
            chrome.tabs.onReplaced.removeListener(this.tabReplaceListener);
        };
        //----------------------
        //
        // Add/remove css to tab
        //
        //----------------------
        Extension.prototype.canInjectScript = function (tab) {
            // Prevent throwing errors on specific chrome adresses
            return (tab
                && tab.url
                && tab.url.indexOf('chrome') !== 0
                && tab.url.indexOf('https://chrome.google.com/webstore') !== 0);
        };
        /**
         * Adds style to tab.
         */
        Extension.prototype.addStyleToTab = function (tab) {
            if (!this.canInjectScript(tab)) {
                return;
            }
            chrome.tabs.executeScript(tab.id, {
                code: this.getCode_addStyle(tab.url),
                runAt: 'document_start'
            });
        };
        /**
         * Removes style from tab.
         */
        Extension.prototype.removeStyleFromTab = function (tab) {
            if (!this.canInjectScript(tab)) {
                return;
            }
            chrome.tabs.executeScript(tab.id, {
                code: this.getCode_removeStyle()
            });
        };
        Extension.prototype.getCode_addStyle = function (url) {
            var css = this.generator.createCssCode(this.config, url);
            var code = "\n" + (DarkReader.DEBUG ? "console.log('Executing DR script (add)...');" : "") + "\n//debugger;\nvar createDRStyle = function() {\n    var css = '" + css.replace(/'/g, '\\\'') + "';\n    var style = document.createElement('style');\n    style.setAttribute('id', 'dark-reader-style');\n    style.type = 'text/css';\n    style.appendChild(document.createTextNode(css));\n    return style;\n};\nif (document.head) {\n    var style = createDRStyle();\n    var prevStyle = document.getElementById('dark-reader-style');\n    if (!prevStyle) {\n        document.head.appendChild(style);\n        " + (DarkReader.DEBUG ? "console.log('Added DR style.');" : "") + "\n    } else if (style.textContent.replace(/^\\s*/gm, '') !== prevStyle.textContent.replace(/^\\s*/gm, '')) {\n        prevStyle.parentElement.removeChild(prevStyle);\n        document.head.appendChild(style);\n        " + (DarkReader.DEBUG ? "console.log('Updated DR style.');" : "") + "\n    }\n} else {\n    var drObserver = new MutationObserver(function (mutations) {\n        for (var i = 0; i < mutations.length; i++) {\n            if (mutations[i].target.nodeName === 'HEAD') {\n                drObserver.disconnect();\n                document.removeEventListener('readystatechange', onReady);\n                var prevStyle = document.getElementById('dark-reader-style');\n                if (!prevStyle) {\n                    var style = createDRStyle();\n                    document.head.appendChild(style);\n                    " + (DarkReader.DEBUG ? "console.log('Added DR style using observer.');" : "") + "\n                }\n                break;\n            }\n        }\n    });\n    drObserver.observe(document, { childList: true, subtree: true });\n    var onReady = function() {\n        if (document.readyState !== 'complete') { \n            return;\n        }\n        drObserver.disconnect();\n        document.removeEventListener('readystatechange', onReady);\n        if (!document.head) {\n            var head = document.createElement('head');\n            document.documentElement.insertBefore(head, document.documentElement.firstElementChild);\n        }\n        var prevStyle = document.getElementById('dark-reader-style');\n        if (!prevStyle) {\n            var style = createDRStyle();\n            document.head.appendChild(style);\n            " + (DarkReader.DEBUG ? "console.log('Added DR style on document ready.');" : "") + "\n        }\n    };\n    document.addEventListener('readystatechange', onReady);\n    if (document.readyState === 'complete') { \n        onReady();\n    }\n}\n";
            return code;
        };
        Extension.prototype.getCode_removeStyle = function () {
            var code = "\n" + (DarkReader.DEBUG ? "console.log('Executing DR script (remove)...');" : "") + "\nvar style = document.getElementById('dark-reader-style');\nstyle && style.parentElement.removeChild(style);\n";
            return code;
        };
        //-------------------------------------
        //
        //       Configuration management
        //
        //-------------------------------------
        /**
         * Loads configuration from Chrome storage.
         */
        Extension.prototype.loadUserSettings = function () {
            var _this = this;
            var defaultFilterConfig = xp.clone(DarkReader.DEFAULT_FILTER_CONFIG);
            var defaultStore = {
                enabled: true,
                config: defaultFilterConfig
            };
            chrome.storage.sync.get(defaultStore, function (store) {
                if (!store.config) {
                    store.config = defaultFilterConfig;
                }
                if (!Array.isArray(store.config.siteList)) {
                    var arr = [];
                    for (var key in store.config.siteList) {
                        arr[key] = store.config.siteList[key];
                    }
                    store.config.siteList = arr;
                }
                _this.config = xp.observable(store.config);
                _this.enabled = store.enabled;
                console.log('loaded', store);
            });
        };
        /**
         * Saves configuration to Chrome storage.
         */
        Extension.prototype.saveUserSettings = function () {
            var _this = this;
            // NOTE: Debounce config saving.
            if (this.savedTimeout) {
                clearTimeout(this.savedTimeout);
            }
            this.savedTimeout = setTimeout(function () {
                var store = {
                    enabled: _this.enabled,
                    config: _this.config
                };
                chrome.storage.sync.set(store, function () {
                    console.log('saved', store);
                    _this.savedTimeout = null;
                });
            }, SAVE_CONFIG_TIMEOUT);
        };
        /**
         * Returns the list of fonts
         * installed in system.
         */
        Extension.prototype.getFontList = function (onReturned) {
            if (!chrome.fontSettings) {
                // Todo: Remove it as soon as Firefox and Edge get support.
                setTimeout(function () { return onReturned([
                    'serif',
                    'sans-serif',
                    'monospace',
                    'cursive',
                    'fantasy',
                    'system-ui'
                ]); });
                return;
            }
            chrome.fontSettings.getFontList(function (res) {
                // id or name?
                var fonts = res.map(function (r) { return r.fontId; });
                onReturned(fonts);
            });
        };
        //-------------------------------------
        //
        //          Developer tools
        //
        //-------------------------------------
        Extension.prototype.getSavedDevInversionFixes = function () {
            return localStorage.getItem('dev_inversion_fixes') || null;
        };
        Extension.prototype.saveDevInversionFixes = function (json) {
            localStorage.setItem('dev_inversion_fixes', json);
        };
        Extension.prototype.getDevInversionFixesText = function () {
            var fixes = this.getSavedDevInversionFixes();
            var text = DarkReader.formatJson(fixes ? JSON.parse(fixes) : DarkReader.copyJson(DarkReader.RAW_INVERSION_FIXES));
            return text;
        };
        Extension.prototype.resetDevInversionFixes = function () {
            localStorage.removeItem('dev_inversion_fixes');
            DarkReader.handleInversionFixes(DarkReader.copyJson(DarkReader.RAW_INVERSION_FIXES));
            this.onConfigPropChanged();
        };
        Extension.prototype.applyDevInversionFixes = function (json, callback) {
            var obj;
            try {
                obj = JSON.parse(json);
                var text = DarkReader.formatJson(obj);
                this.saveDevInversionFixes(text);
                DarkReader.handleInversionFixes(obj);
                this.onConfigPropChanged();
                callback(null);
            }
            catch (err) {
                callback(err);
            }
        };
        return Extension;
    })(xp.Model);
    DarkReader.Extension = Extension;
    //
    // ---------- Constants --------------------
    var ICON_PATHS = {
        active_19: '../img/dr_active_19.png',
        active_38: '../img/dr_active_38.png',
        inactive_19: '../img/dr_inactive_19.png',
        inactive_38: '../img/dr_inactive_38.png'
    };
    var SAVE_CONFIG_TIMEOUT = 1000;
})(DarkReader || (DarkReader = {}));
