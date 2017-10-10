var __extends = this.__extends || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var DarkReader;
(function (DarkReader) {
    var Popup;
    (function (Popup) {
        var PopupWindow = (function (_super) {
            __extends(PopupWindow, _super);
            function PopupWindow(ext) {
                var _this = this;
                _super.call(this, { title: 'Dark Reader settings', scope: ext, scrollBar: 'none' }, [
                    //
                    // ---- Logo ----
                    new xp.Html({ html: '<img id="logo" src="img/dark-reader-type.svg" alt="Dark Reader" />', flex: 'none' }),
                    //
                    // ---- Top section ----
                    new xp.HBox({ name: 'topSection', itemsAlign: 'top', flex: 'none' }, [
                        // new xp.Label({
                        //     name: 'appDescription',
                        //     style: 'description',
                        //     text: 'Adjust settings that\nbetter fit your screen'
                        // }),
                        new xp.VBox({ flex: 'stretch', style: 'controlContainer siteToggleContainer' }, [
                            new xp.Button({
                                name: 'siteToggle',
                                onClick: function () {
                                    ext.toggleCurrentSite();
                                }
                            }),
                            new Popup.HotkeyLink({
                                commandName: 'addSite',
                                noHotkeyText: 'setup current site\ntoggle hotkey',
                                hotkeyTextTemplate: 'toggle current site\n#HOTKEY',
                                style: 'status',
                                enabled: '{enabled}'
                            })
                        ]),
                        new xp.VBox({ flex: 'stretch', style: 'controlContainer' }, [
                            new Popup.Toggle({
                                value: '{enabled}'
                            }),
                            new Popup.HotkeyLink({
                                commandName: 'toggle',
                                noHotkeyText: 'setup extension\ntoggle hotkey',
                                hotkeyTextTemplate: 'toggle extension\n#HOTKEY',
                                style: 'status'
                            })
                        ])
                    ]),
                    //
                    // ---- Tab panel ----
                    new Popup.TabPanel({ onTabSwitched: function (t) { return t.tabName === 'Site list' && _this.siteList.focus(); }, flex: 'stretch', enabled: '{enabled}' }, [
                        //
                        // ---- Filter ----
                        new Popup.Tab({ tabName: 'Filter', itemsAlign: 'stretch' }, [
                            // Mode
                            new xp.VBox({ name: 'modeToggle', style: 'controlContainer' }, [
                                new xp.HBox({ style: 'line' }, [
                                    new xp.ToggleButton({
                                        style: 'iconButton',
                                        icon: '.darkMode',
                                        item: 1 /*FilterMode.dark*/,
                                        selectedItem: '{config.mode}'
                                    }),
                                    new Popup.Toggle({
                                        value: '{config.mode}',
                                        valueOn: 1,
                                        valueOff: 0,
                                        labelOn: 'Dark',
                                        labelOff: 'Light',
                                        flex: 'stretch'
                                    }),
                                    new xp.ToggleButton({
                                        style: 'iconButton',
                                        icon: '.lightMode',
                                        item: 0 /*FilterMode.light*/,
                                        selectedItem: '{config.mode}'
                                    })
                                ]),
                                new xp.Label({ style: 'status', text: 'Mode' })
                            ]),
                            // Brightness
                            new Popup.UpDown({
                                label: 'Brightness',
                                step: 0.1,
                                value: '{config.brightness}',
                                setterConvertor: function (v) { return (v - 50) / 100; },
                                getterConvertor: function (v) { return Math.round(v * 100) + 50; },
                                statusCreator: function (v) { return v > 100 ?
                                    '+' + (v - 100)
                                    : v < 100 ?
                                        '-' + (100 - v)
                                        : 'off'; }
                            }),
                            // Contrast
                            new Popup.UpDown({
                                label: 'Contrast',
                                step: 0.1,
                                value: '{config.contrast}',
                                setterConvertor: function (v) { return (v - 50) / 100; },
                                getterConvertor: function (v) { return Math.round(v * 100) + 50; },
                                statusCreator: function (v) { return v > 100 ?
                                    '+' + (v - 100)
                                    : v < 100 ?
                                        '-' + (100 - v)
                                        : 'off'; }
                            }),
                            // Grayscale
                            new Popup.UpDown({
                                label: 'Grayscale',
                                step: 0.1,
                                value: '{config.grayscale}',
                                setterConvertor: function (v) { return v / 100; },
                                getterConvertor: function (v) { return Math.round(v * 100); },
                                statusCreator: function (v) { return v > 0 ?
                                    '+' + v
                                    : 'off'; }
                            }),
                            // Sepia
                            new Popup.UpDown({
                                label: 'Sepia',
                                step: 0.1,
                                value: '{config.sepia}',
                                setterConvertor: function (v) { return v / 100; },
                                getterConvertor: function (v) { return Math.round(v * 100); },
                                statusCreator: function (v) { return v > 0 ?
                                    '+' + v
                                    : 'off'; }
                            }),
                        ]),
                        //
                        // ---- Font ----
                        new Popup.Tab({ tabName: 'Font' }, [
                            // Select font
                            new xp.VBox({ style: 'controlContainer' }, [
                                new xp.HBox({ style: 'line fontSelectContainer' }, [
                                    new xp.CheckBox({
                                        checked: '{config.useFont}'
                                    }),
                                    new Popup.FontSelect({
                                        fonts: '{fonts}',
                                        selectedFont: '{config.fontFamily}',
                                        flex: 'stretch',
                                    }),
                                ]),
                                new xp.Label({ style: 'status', text: 'Select a font' })
                            ]),
                            // Text stroke
                            new Popup.UpDown({
                                label: 'Text stroke',
                                step: 0.1,
                                value: '{config.textStroke}',
                                //setterConvertor: (v: number) => (v - 50) / 100,
                                getterConvertor: function (v) { return Math.round(v * 10) / 10; },
                                statusCreator: function (v) { return v > 0 ?
                                    '+' + v
                                    : v === 0 ?
                                        'off'
                                        : 'Wrong value'; }
                            })
                        ]),
                        //
                        // ---- Site list ----
                        new Popup.Tab({ tabName: 'Site list' }, [
                            new Popup.Toggle({
                                value: '{config.invertListed}',
                                labelOn: 'Invert listed only',
                                labelOff: 'Not invert listed',
                                flex: 'none'
                            }),
                            new xp.VBox({ flex: 'stretch', scrollBar: 'vertical', style: 'siteListParent' }, [
                                new Popup.SiteList({
                                    sites: '{config.siteList}',
                                    init: function (sl) { return _this.siteList = sl; }
                                }),
                                new Popup.HotkeyLink({
                                    commandName: 'addSite',
                                    noHotkeyText: 'setup a hotkey for adding site',
                                    hotkeyTextTemplate: 'hotkey for adding site: #HOTKEY',
                                    style: 'description'
                                })
                            ])
                        ])
                    ]),
                    //
                    // ---- Footer ----
                    new xp.VBox({ name: 'footer', flex: 'none', itemsIndent: 'none' }, [
                        new xp.Html({
                            flex: 'none',
                            html: "\n                        <p class=\"description\">Some things should not be inverted?\n                            You can <strong>help and fix it</strong>, here is a tool\n                        ",
                            enabled: '{enabled}'
                        }),
                        new xp.Button({
                            text: '🛠 Open developer tools',
                            onClick: function () { return _this.devTools.isOpen = true; }
                        })
                    ]),
                    //
                    // ---- Dev Tools ----
                    new Popup.DevTools({
                        init: function (dt) { return _this.devTools = dt; },
                        isOpen: false,
                        inversionFixText: ext.getDevInversionFixesText(),
                        onApply: function (text) {
                            ext.applyDevInversionFixes(text, function (err) {
                                if (err) {
                                    _this.devTools.inversionFixErrorText = err.message;
                                }
                                else {
                                    _this.devTools.inversionFixErrorText = '';
                                    _this.devTools.inversionFixText = ext.getDevInversionFixesText();
                                }
                            });
                        },
                        onReset: function () {
                            ext.resetDevInversionFixes();
                            _this.devTools.inversionFixErrorText = '';
                            _this.devTools.inversionFixText = ext.getDevInversionFixesText();
                        }
                    })
                ]);
                this.initSiteToggleButton(this.siteToggle, ext);
            }
            PopupWindow.prototype.getTemplate = function () {
                // Clear body
                while (document.body.lastElementChild) {
                    document.body.removeChild(document.body.lastElementChild);
                }
                return _super.prototype.getTemplate.call(this);
            };
            PopupWindow.prototype.initSiteToggleButton = function (btn, ext) {
                var _this = this;
                ext.getActiveTabInfo(function (info) {
                    // NOTE: Disable button if toggle has no effect.
                    var toggleHasEffect = function () {
                        return (!info.isChromePage
                            && !(!ext.config.invertListed
                                && info.isInDarkList));
                    };
                    btn.enabled = ext.enabled && toggleHasEffect();
                    var changeReg = new xp.EventRegistrar();
                    changeReg.subscribe(ext.onPropertyChanged, function (prop) {
                        if (prop === 'enabled') {
                            btn.enabled = ext.enabled && toggleHasEffect();
                        }
                    });
                    changeReg.subscribe(ext.config.onPropertyChanged, function (prop) {
                        if (prop === 'invertListed') {
                            btn.enabled = ext.enabled && toggleHasEffect();
                        }
                    });
                    _this.onRemoved.addHandler(function () { return changeReg.unsubscribeAll(); });
                    // Set button text
                    if (info.host) {
                        // HACK: Try to break URLs at dots.
                        btn.text = '*';
                        btn.domElement.querySelector('.text').innerHTML = '<wbr>' + info.host.replace(/\./g, '<wbr>.');
                    }
                    else {
                        btn.text = 'current site';
                    }
                });
            };
            return PopupWindow;
        })(xp.Window);
        Popup.PopupWindow = PopupWindow;
    })(Popup = DarkReader.Popup || (DarkReader.Popup = {}));
})(DarkReader || (DarkReader = {}));
