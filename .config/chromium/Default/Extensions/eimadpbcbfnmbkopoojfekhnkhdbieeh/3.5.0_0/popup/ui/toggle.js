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
         * Toggle switch.
         */
        var Toggle = (function (_super) {
            __extends(Toggle, _super);
            function Toggle(markup) {
                _super.call(this, markup);
            }
            Toggle.prototype.getTemplate = function () {
                var _this = this;
                return xp.Dom.create("\n                <div class=\"Toggle\">\n                  <span class=\"on\">On</span>\n                  <span class=\"off\">Off</span>\n                </div>\n                ", {
                    '.on': function (el) { return _this.elementOn = el; },
                    '.off': function (el) { return _this.elementOff = el; }
                });
            };
            Toggle.prototype.defineProperties = function () {
                var _this = this;
                _super.prototype.defineProperties.call(this);
                this.defineProperty('value', {
                    setter: function (v) {
                        var switchedOn = v === _this.valueOn;
                        if (switchedOn) {
                            _this.elementOn.classList.add('active');
                            _this.elementOff.classList.remove('active');
                            _this.domElement.classList.add('switchedOn');
                        }
                        else {
                            _this.elementOff.classList.add('active');
                            _this.elementOn.classList.remove('active');
                            _this.domElement.classList.remove('switchedOn');
                        }
                    }
                });
                this.defineProperty('labelOn', {
                    setter: function (text) {
                        _this.elementOn.textContent = text;
                    }
                });
                this.defineProperty('labelOff', {
                    setter: function (text) {
                        _this.elementOff.textContent = text;
                    }
                });
            };
            Toggle.prototype.initEvents = function () {
                var _this = this;
                _super.prototype.initEvents.call(this);
                this.elementOn.addEventListener('click', function (e) {
                    _this.onInput('value', _this.valueOn);
                });
                this.elementOff.addEventListener('click', function (e) {
                    _this.onInput('value', _this.valueOff);
                });
            };
            Toggle.prototype.setDefaults = function () {
                _super.prototype.setDefaults.call(this);
                this.valueOn = true;
                this.valueOff = false;
                this.labelOn = 'On';
                this.labelOff = 'Off';
            };
            return Toggle;
        })(xp.Element);
        Popup.Toggle = Toggle;
    })(Popup = DarkReader.Popup || (DarkReader.Popup = {}));
})(DarkReader || (DarkReader = {}));
