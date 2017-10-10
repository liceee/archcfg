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
        /**
         * Developer tools panel.
         */
        var DevTools = (function (_super) {
            __extends(DevTools, _super);
            function DevTools(markup) {
                var _this = this;
                _super.call(this, markup, [
                    new xp.Label({
                        flex: 'none',
                        style: 'devToolsTitle',
                        text: 'Developer Tools'
                    }),
                    new xp.Button({
                        style: 'devToolsCloseButton',
                        text: '✕',
                        onClick: function () { return _this.isOpen = false; }
                    }),
                    new xp.VBox({ style: 'inversionFixEditor', flex: 'stretch' }, [
                        new xp.Label({
                            style: 'description',
                            text: 'Inversion fix editor'
                        }),
                        new xp.TextArea({
                            flex: 'stretch',
                            init: function (ta) { return _this.inversionTextArea = ta; }
                        }),
                        new xp.Label({
                            style: 'inversionFixErrorText',
                            init: function (el) { return _this.inversionErrorLabel = el; }
                        }),
                        new xp.HBox({ flex: 'none', style: 'inversionFixEditorButtons' }, [
                            new xp.Button({
                                flex: 'stretch',
                                text: 'Reset',
                                onClick: function () { return markup.onReset(); }
                            }),
                            new xp.Button({
                                flex: 'stretch',
                                text: 'Apply',
                                onClick: function () { return markup.onApply(_this.inversionFixText); }
                            })
                        ]),
                        new xp.Html({
                            html: "\n                            <div class=\"description selectable\">\n                                Read about this tool <strong><a href=\"https://github.com/alexanderby/darkreader#how-to-contribute\" target=\"_blank\">here</a></strong>. If a <strong>popular</strong>\n                                website looks incorrect e-mail to\n                                <strong>DarkReaderApp@gmail.com</strong>\n                            </div>\n                        "
                        })
                    ])
                ]);
                this.isOpen = markup.isOpen;
                this.inversionFixText = markup.inversionFixText;
            }
            DevTools.prototype.getTemplate = function () {
                var t = _super.prototype.getTemplate.call(this);
                t.classList.add('DevTools');
                return t;
            };
            DevTools.prototype.defineProperties = function () {
                var _this = this;
                _super.prototype.defineProperties.call(this);
                this.defineProperty('isOpen', {
                    getter: function () {
                        return _this.domElement.classList.contains('open');
                    },
                    setter: function (v) {
                        if (v) {
                            _this.domElement.classList.add('open');
                        }
                        else {
                            _this.domElement.classList.remove('open');
                        }
                    }
                });
                this.defineProperty('inversionFixText', {
                    getter: function () {
                        return _this.inversionTextArea.text;
                    },
                    setter: function (text) {
                        _this.inversionTextArea.text = text;
                    }
                });
                this.defineProperty('inversionFixErrorText', {
                    getter: function () {
                        return _this.inversionErrorLabel.text;
                    },
                    setter: function (text) {
                        _this.inversionErrorLabel.text = text;
                    }
                });
            };
            return DevTools;
        })(xp.VBox);
        Popup.DevTools = DevTools;
    })(Popup = DarkReader.Popup || (DarkReader.Popup = {}));
})(DarkReader || (DarkReader = {}));
