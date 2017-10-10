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
         * Displays a hotkey and navigates
         * to Chrome Commands page on click.
         */
        var HotkeyLink = (function (_super) {
            __extends(HotkeyLink, _super);
            function HotkeyLink(markup) {
                _super.call(this, markup);
                this.setupText();
            }
            HotkeyLink.prototype.getTemplate = function () {
                return xp.Dom.create('<label class="Label shortcut"></label>');
            };
            // TODO: Bind to chrome command shortcut change if possible.
            HotkeyLink.prototype.setupText = function () {
                var _this = this;
                if (!chrome.commands) {
                    this.text = 'Chrome hotkey';
                    return;
                }
                // TODO: Edit chrome type definition.
                chrome.commands.getAll(function (commands) {
                    if (commands) {
                        var cmd = commands.filter(function (c) { return c.name === _this.commandName; })[0];
                        if (cmd) {
                            _this.text = cmd.shortcut ?
                                _this.hotkeyTextTemplate.replace(/#HOTKEY/g, cmd.shortcut)
                                : _this.noHotkeyText;
                        }
                        else {
                            throw new Error("Command \"" + _this.commandName + "\" not found.");
                        }
                    }
                });
            };
            HotkeyLink.prototype.initEvents = function () {
                var _this = this;
                _super.prototype.initEvents.call(this);
                this.onClick.addHandler(function (e) {
                    chrome.tabs.create({
                        url: "chrome://extensions/configureCommands#command-" + chrome.runtime.id + "-" + _this.commandName,
                        active: true
                    });
                });
            };
            return HotkeyLink;
        })(xp.Label);
        Popup.HotkeyLink = HotkeyLink;
    })(Popup = DarkReader.Popup || (DarkReader.Popup = {}));
})(DarkReader || (DarkReader = {}));
