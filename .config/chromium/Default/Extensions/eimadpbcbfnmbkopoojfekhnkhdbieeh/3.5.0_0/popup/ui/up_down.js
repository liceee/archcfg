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
         * Control for adjusting value using up/down buttons.
         */
        var UpDown = (function (_super) {
            __extends(UpDown, _super);
            function UpDown(markup) {
                var _this = this;
                _super.call(this, markup, [
                    new xp.HBox({ style: 'line' }, [
                        new xp.Button({
                            style: 'iconButton',
                            icon: '.buttonDown',
                            init: function (el) { return _this.buttonDown = el; }
                        }),
                        new TrackBar({
                            init: function (el) { return _this.trackBar = el; }
                        }),
                        new xp.Button({
                            style: 'iconButton',
                            icon: '.buttonUp',
                            init: function (el) { return _this.buttonUp = el; }
                        })
                    ]),
                    new xp.Label({
                        style: 'status',
                        init: function (el) { return _this.status = el; }
                    })
                ]);
                // Increment
                this.buttonUp.onClick.addHandler(function () {
                    if (_this.trackBar.value < 1) {
                        var value = _this.trackBar.value + _this.step;
                        if (value > 1) {
                            value = 1;
                        }
                        if (_this.getterConvertor) {
                            value = _this.getterConvertor(value);
                        }
                        _this.onInput('value', value);
                    }
                }, this);
                // Decrement
                this.buttonDown.onClick.addHandler(function () {
                    if (_this.trackBar.value > 0) {
                        var value = _this.trackBar.value - _this.step;
                        if (value < 0) {
                            value = 0;
                        }
                        if (_this.getterConvertor) {
                            value = _this.getterConvertor(value);
                        }
                        _this.onInput('value', value);
                    }
                }, this);
                // Disable buttons on boundary values
                this.buttonDown.useParentScope = false;
                this.buttonDown.express('enabled', '{value} > 0', this.trackBar);
                this.buttonUp.useParentScope = false;
                this.buttonUp.express('enabled', '{value} < 1', this.trackBar);
            }
            UpDown.prototype.getTemplate = function () {
                var t = _super.prototype.getTemplate.call(this);
                t.classList.add('UpDown');
                return t;
            };
            UpDown.prototype.setDefaults = function () {
                _super.prototype.setDefaults.call(this);
                this.step = 0.1;
            };
            UpDown.prototype.defineProperties = function () {
                var _this = this;
                _super.prototype.defineProperties.call(this);
                this.defineProperty('value', {
                    getter: function () {
                        if (_this.getterConvertor) {
                            return _this.getterConvertor(_this.trackBar.value);
                        }
                        else {
                            return _this.trackBar.value;
                        }
                    },
                    setter: function (v) {
                        if (_this.setterConvertor) {
                            _this.trackBar.value = _this.setterConvertor(v);
                        }
                        else {
                            _this.trackBar.value = v;
                        }
                        if (_this.statusCreator) {
                            _this.status.text = _this.statusCreator(v);
                        }
                    }
                });
                this.defineProperty('label', {
                    getter: function () { return _this.trackBar.label; },
                    setter: function (v) { return _this.trackBar.label = v; }
                });
            };
            return UpDown;
        })(xp.VBox);
        Popup.UpDown = UpDown;
        /**
         * Track bar.
         */
        var TrackBar = (function (_super) {
            __extends(TrackBar, _super);
            function TrackBar(markup) {
                _super.call(this, markup);
            }
            // TODO: Trackbar click...
            TrackBar.prototype.getTemplate = function () {
                var _this = this;
                return xp.Dom.create("\n                <span class=\"TrackBar\">\n                  <span class=\"value\">&nbsp;</span>\n                  <label></label>\n                </span>\n                ", {
                    '.value': function (el) { return _this.elementValue = el; },
                    'label': function (el) { return _this.elementLabel = el; }
                });
            };
            TrackBar.prototype.defineProperties = function () {
                var _this = this;
                _super.prototype.defineProperties.call(this);
                this.defineProperty('value', {
                    setter: function (value) {
                        if (value < 0 || value > 1) {
                            throw new Error('Track bar value must be between 0 and 1');
                        }
                        _this.elementValue.style.width = Math.round(value * 100) + '%';
                    },
                    observable: true
                });
                this.defineProperty('label', {
                    setter: function (value) {
                        _this.elementLabel.textContent = value;
                    }
                });
            };
            return TrackBar;
        })(xp.Element);
    })(Popup = DarkReader.Popup || (DarkReader.Popup = {}));
})(DarkReader || (DarkReader = {}));
